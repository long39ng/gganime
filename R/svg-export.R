# gridSVG export and the xml2 post-pass: build the data->SVG affine per panel,
# annotate data elements, inline circular pch to <circle>, and tidy the root.
# All gridSVG contact is isolated here so a different backend could swap in.

#' Export the reference gtable to an SVG document
#'
#' @param gtable A `gtable` from `render_union_gtable()`.
#' @param panel_ranges Per-panel data ranges from the scene spec.
#' @param res Export resolution in dpi.
#' @param width,height Export device size in inches. The viewBox is then
#'   `width * res` by `height * res`.
#' @return A list with `doc` (an xml2 document), `panels` (per-panel affine
#'   closures + ranges), and `symbols` (the pch symbol table).
#' @noRd
export_scene_svg <- function(
  gtable,
  panel_ranges,
  res = 96,
  width = 7,
  height = 7
) {
  svgpath <- tempfile(fileext = ".svg")
  grDevices::pdf(NULL, width = width, height = height)
  grid::grid.newpage()
  grid::grid.draw(gtable)
  grid::grid.force()
  exp <- gridSVG::grid.export(
    svgpath,
    exportCoords = "file",
    uniqueNames = TRUE,
    res = res
  )
  grDevices::dev.off()

  doc <- xml2::read_xml(svgpath)
  xml2::xml_ns_strip(doc)

  # gridSVG's coordinate <script> references an external file; drop it.
  for (s in xml2::xml_find_all(doc, ".//script")) {
    xml2::xml_remove(s)
  }

  # gridSVG renders text in a rotated/flipped viewport (axis titles, plot
  # title/subtitle/caption) as MathML in a fixed-size <foreignObject> that the
  # browser clips. Convert each to a plain <text>, keyed to the title's own
  # justification read from the gtable.
  aligns <- title_alignments(gtable)
  for (fo in xml2::xml_find_all(doc, ".//foreignObject")) {
    foreign_object_to_text(fo, aligns)
  }

  list(
    doc = doc,
    panels = panel_affines(exp, panel_ranges, res),
    symbols = symbol_table(doc)
  )
}

# hjust/vjust of each title grob (axis titles, plot title/subtitle/caption),
# keyed by the grob-name prefix shared with the exported SVG id.
title_alignments <- function(gtable) {
  out <- list()
  for (g in gtable$grobs) {
    if (!inherits(g, "titleGrob")) {
      next
    }
    key <- sub("\\.\\.titleGrob.*$", "", g$name %||% "")
    txt <- g$children[[1]]
    if (!is.null(txt$hjust) || !is.null(txt$vjust)) {
      out[[key]] <- list(hjust = txt$hjust, vjust = txt$vjust)
    }
  }
  out
}

# Replace a gridSVG MathML title (<switch><foreignObject><math>...) with a plain
# <text>. The enclosing translate/scale already positions the alignment anchor,
# so the <text> only needs the matching text-anchor and, for a rotated title,
# the foreignObject's rotate transform.
foreign_object_to_text <- function(fo, aligns) {
  mtext <- xml2::xml_find_first(fo, ".//mtext")
  if (inherits(mtext, "xml_missing")) {
    return(invisible())
  }

  target <- xml2::xml_parent(fo)
  if (!identical(xml2::xml_name(target), "switch")) {
    target <- fo
  }

  align <- foreign_object_align(fo, aligns)
  new <- xml2::xml_add_sibling(target, "text", .where = "after")
  xml2::xml_set_attr(new, "x", "0")
  xml2::xml_set_attr(new, "y", "0")
  xml2::xml_set_attr(new, "text-anchor", align$anchor)
  xml2::xml_set_attr(new, "dominant-baseline", align$baseline)
  for (a in c(
    "font-size",
    "font-family",
    "font-weight",
    "font-style",
    "fill",
    "fill-opacity"
  )) {
    v <- xml2::xml_attr(fo, a)
    if (!is.na(v)) {
      xml2::xml_set_attr(new, a, v)
    }
  }
  tr <- xml2::xml_attr(fo, "transform")
  if (!is.na(tr)) {
    xml2::xml_set_attr(new, "transform", tr)
  }
  xml2::xml_set_text(new, xml2::xml_text(mtext))

  xml2::xml_remove(target)
  invisible()
}

# text-anchor and dominant-baseline for a converted title, from its own
# hjust/vjust. gridSVG places the anchor at the (hjust, vjust) point of the text
# box, so the <text> aligns to it from the matching edge: hjust 0/0.5/1 ->
# start/middle/end, vjust 0/0.5/1 -> after-edge/central/before-edge. The rotate
# transform (kept from the foreignObject) orients a rotated title, so this
# reproduces e.g. a right-aligned y-axis title sitting at the top.
foreign_object_align <- function(fo, aligns) {
  ids <- xml2::xml_attr(xml2::xml_find_all(fo, "ancestor::g[@id]"), "id")
  for (id in rev(ids)) {
    a <- aligns[[sub("\\.\\.titleGrob.*$", "", id)]]
    if (!is.null(a)) {
      return(list(
        anchor = hjust_anchor(a$hjust),
        baseline = vjust_baseline(a$vjust)
      ))
    }
  }
  list(anchor = "middle", baseline = "central")
}

hjust_anchor <- function(hjust) {
  if (is.null(hjust)) {
    "middle"
  } else if (hjust <= 0.25) {
    "start"
  } else if (hjust >= 0.75) {
    "end"
  } else {
    "middle"
  }
}

vjust_baseline <- function(vjust) {
  if (is.null(vjust)) {
    "central"
  } else if (vjust <= 0.25) {
    "text-after-edge"
  } else if (vjust >= 0.75) {
    "text-before-edge"
  } else {
    "central"
  }
}

# Data->SVG affine per panel. The panel viewport's own scales are [0, 1] on
# ggplot2 4, so the map is built from the exported panel rectangle plus the data
# ranges: svg = origin + (data - min) / (max - min) * extent, both axes rising.
panel_affines <- function(exp, panel_ranges, res) {
  coords <- exp$coords
  # gridSVG's viewport counter changes the GRID.VP.<n>.<m> infixes each export,
  # so match the panel viewport structurally rather than by a fixed number.
  keys <- grep(
    "panel.*::GRID\\.VP\\.[0-9]+\\.[0-9]+$",
    names(coords),
    value = TRUE
  )
  if (length(keys) != 1L) {
    cli::cli_abort(c(
      "Expected exactly one panel in the exported SVG.",
      x = "Found {length(keys)}; faceted plots are not supported yet."
    ))
  }
  rect <- coords[[keys]]
  xr <- panel_ranges[["1"]]$x_range
  yr <- panel_ranges[["1"]]$y_range

  list(
    "1" = list(
      to_svg_x = function(dx) {
        rect$x + (dx - xr[1]) / (xr[2] - xr[1]) * rect$width
      },
      to_svg_y = function(dy) {
        rect$y + (dy - yr[1]) / (yr[2] - yr[1]) * rect$height
      },
      res = res
    )
  )
}

# Radius scaling per pch symbol: single-<circle> symbols carry a factor
# (circle r / viewBox width) so a point's SVG radius is factor * font-size.
symbol_table <- function(doc) {
  syms <- xml2::xml_find_all(doc, ".//symbol")
  tab <- list()
  for (s in syms) {
    id <- xml2::xml_attr(s, "id")
    vb <- xml2::xml_attr(s, "viewBox")
    circles <- xml2::xml_find_all(s, "./circle")
    if (length(circles) == 1L && !is.na(vb)) {
      vbw <- as.numeric(strsplit(trimws(vb), "\\s+")[[1]])[[3]]
      r <- as.numeric(xml2::xml_attr(circles[[1]], "r"))
      tab[[id]] <- list(circle = TRUE, factor = r / vbw)
    } else {
      tab[[id]] <- list(circle = FALSE, factor = NA_real_)
    }
  }
  tab
}

# Replace a <use> point with an equivalent <circle>, carrying its presentation
# attributes plus the animation id. `(x, y)` on the <use> is the visible centre.
inline_circle <- function(node, r, layer_index, id) {
  keep <- c("fill", "stroke", "fill-opacity", "stroke-opacity", "stroke-width")
  attrs <- list(
    cx = xml2::xml_attr(node, "x"),
    cy = xml2::xml_attr(node, "y"),
    r = format(r, trim = TRUE)
  )
  for (a in keep) {
    v <- xml2::xml_attr(node, a)
    if (!is.na(v)) {
      attrs[[a]] <- v
    }
  }
  attrs[["data-animejs-id"]] <- id
  attrs[["data-layer"]] <- as.character(layer_index)
  do.call(xml2::xml_add_sibling, c(list(node, "circle"), attrs))
  xml2::xml_remove(node)
}

# Data elements of a panel: the `<tag>` nodes inside the panel viewport group
# that are not grid lines. Axis ticks and legend keys live outside the panel
# group, so this returns exactly the drawn data, in document (= union) order.
# GeomPath and GeomPolygon share this because their grobs carry no geom-name id
# prefix (unlike points/rects) -- only panel membership distinguishes them.
panel_data_nodes <- function(doc, tag) {
  xpath <- sprintf(
    ".//%s[not(starts-with(@id, 'panel.grid')) and ancestor::g[contains(@id, 'panel.') and contains(@id, 'GRID.VP')]]",
    tag
  )
  xml2::xml_find_all(doc, xpath)
}

# Tag a node with the animation id without changing its element type.
set_element_id <- function(node, layer_index, id) {
  xml2::xml_set_attr(node, "data-animejs-id", id)
  xml2::xml_set_attr(node, "data-layer", as.character(layer_index))
  invisible(node)
}

# Serialise the document, dropping the fixed root width/height (keeping viewBox)
# for resolution independence. xml2 cannot remove attributes, so patch the tag.
finalize_svg <- function(doc) {
  # gridSVG indents the <tspan> inside each <text>, which is only harmless while
  # whitespace collapses. A host page can hand the widget `white-space: pre` --
  # pkgdown drops example output inside a <pre> -- and the preserved newline then
  # pushes every tspan down a line in Firefox, so tick labels, legend keys, and
  # the legend title all drift. Declare the mode the document was written for.
  xml2::xml_set_attr(
    xml2::xml_find_first(doc, "/svg"),
    "style",
    "white-space: normal"
  )
  s <- as.character(doc)
  s <- sub("(<svg\\b[^>]*?)\\s+width=\"[^\"]*\"", "\\1", s)
  s <- sub("(<svg\\b[^>]*?)\\s+height=\"[^\"]*\"", "\\1", s)
  s
}
