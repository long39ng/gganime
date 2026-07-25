# Read panel attribution back out of a rendered widget, the way a browser sees
# it: which panel group each animated node is drawn in, and whether the values
# the timeline animates it to stay inside that panel's rectangle.

widget_doc <- function(widget) {
  doc <- xml2::read_xml(widget$x$svg)
  xml2::xml_ns_strip(doc)
  doc
}

# The PANEL of every animated node, in document order.
panel_of_elements <- function(widget) {
  doc <- widget_doc(widget)
  nodes <- xml2::xml_find_all(doc, ".//*[@data-animejs-id]")
  vapply(nodes, node_panel, character(1))
}

# A fixture symmetric across its panels animates the same number of elements in
# each, so an element attributed to the wrong panel shows up as an imbalance.
expect_balanced_panels <- function(widget, n_panels) {
  counts <- table(panel_of_elements(widget))
  testthat::expect_equal(names(counts), as.character(seq_len(n_panels)))
  testthat::expect_length(unique(as.integer(counts)), 1L)
}

node_panel <- function(node) {
  ids <- xml2::xml_attr(xml2::xml_find_all(node, "ancestor::g[@id]"), "id")
  hit <- grep("^panel-[0-9]+\\.gTree", ids, value = TRUE)
  if (length(hit) == 0L) {
    return(NA_character_)
  }
  sub("^panel-([0-9]+)\\.gTree.*$", "\\1", hit[[1]])
}

# Each panel's rectangle, from the clip path of its viewport group, as
# c(xmin, xmax, ymin, ymax) in the exported y-up space.
panel_bounds <- function(doc) {
  lapply(panel_group_nodes(doc), function(group) {
    clip <- xml2::xml_attr(xml2::xml_parent(group), "clip-path")
    id <- sub("^url\\(#(.*)\\)$", "\\1", clip)
    r <- xml2::xml_find_first(doc, sprintf(".//clipPath[@id='%s']/rect", id))
    at <- function(a) as.numeric(xml2::xml_attr(r, a))
    c(at("x"), at("x") + at("width"), at("y"), at("y") + at("height"))
  })
}

# One logical per animated element that has geometry: does every value its
# timeline animates to stay within its own panel? Constant tracks are dropped
# from the config, so the reference SVG attributes stand in for them.
elements_within_panels <- function(widget, tolerance = 1) {
  doc <- widget_doc(widget)
  bounds <- panel_bounds(doc)
  out <- logical(0)
  for (segment in widget$x$config$segments) {
    id <- sub("^\\[data-animejs-id='(.*)'\\]$", "\\1", segment$selector)
    node <- xml2::xml_find_first(
      doc,
      sprintf(".//*[@data-animejs-id='%s']", id)
    )
    if (inherits(node, "xml_missing")) {
      next
    }
    xy <- animated_xy(segment$props, node)
    if (length(xy$x) == 0L && length(xy$y) == 0L) {
      next
    }
    box <- bounds[[node_panel(node)]]
    out[[id]] <- all(
      xy$x >= box[1] - tolerance,
      xy$x <= box[2] + tolerance,
      xy$y >= box[3] - tolerance,
      xy$y <= box[4] + tolerance
    )
  }
  out
}

# Every x and y a segment animates to, whatever track shape the adapter emitted:
# cx/cy for circles, x/y plus width/height for rects, a `points` string for
# paths and polygons.
animated_xy <- function(props, node) {
  num <- function(v) {
    v <- suppressWarnings(as.numeric(unlist(v)))
    v[!is.na(v)]
  }
  attr_num <- function(a) num(xml2::xml_attr(node, a))

  x <- c(num(props$cx), num(props$x))
  y <- c(num(props$cy), num(props$y))
  for (points in unlist(props$points)) {
    pairs <- strsplit(strsplit(as.character(points), " ")[[1]], ",")
    pairs <- pairs[lengths(pairs) == 2L]
    x <- c(x, as.numeric(vapply(pairs, `[[`, character(1), 1L)))
    y <- c(y, as.numeric(vapply(pairs, `[[`, character(1), 2L)))
  }
  # A rect's far edges. Either edge track can be constant and dropped, so fall
  # back to the reference attribute.
  if (!is.null(props$width) || !is.null(props$height)) {
    x0 <- if (length(x)) x else attr_num("x")
    y0 <- if (length(y)) y else attr_num("y")
    w <- if (is.null(props$width)) attr_num("width") else num(props$width)
    h <- if (is.null(props$height)) attr_num("height") else num(props$height)
    x <- c(x0, outer(x0, w, `+`))
    y <- c(y0, outer(y0, h, `+`))
  }
  list(x = x, y = y)
}
