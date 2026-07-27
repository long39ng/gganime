# gridSVG export and the xml2 post-pass: build the data->SVG affine per panel,
# annotate data elements, inline single-shape pch as that shape, and tidy the
# root. All gridSVG contact is isolated here so a different backend could swap in.

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
    panels = panel_affines(exp, doc, panel_ranges, res),
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

# Data->SVG affine per panel, keyed by PANEL. The panel viewport's own scales are
# [0, 1] on ggplot2 4, so the map comes from the exported panel rectangle plus the
# data ranges: svg = origin + (data - min) / (max - min) * extent, both axes
# rising. Free scales need no extra code -- each panel has its own range.
#
# `to_svg_x`/`to_svg_y` scale a value onto the horizontal/vertical SVG axis;
# callers go through affine_xy() to pair a data column with an axis.
panel_affines <- function(exp, doc, panel_ranges, res) {
  groups <- panel_group_nodes(doc)
  absent <- setdiff(names(panel_ranges), names(groups))
  if (length(absent) > 0) {
    cli::cli_abort(c(
      "The exported SVG is missing a panel.",
      x = "No panel group for PANEL {.val {absent}}."
    ))
  }

  affines <- lapply(names(panel_ranges), function(panel) {
    rect <- panel_viewport_rect(exp, groups[[panel]], panel)
    xr <- panel_ranges[[panel]]$x_range
    yr <- panel_ranges[[panel]]$y_range
    list(
      to_svg_x = function(dx) {
        rect$x + (dx - xr[1]) / (xr[2] - xr[1]) * rect$width
      },
      to_svg_y = function(dy) {
        rect$y + (dy - yr[1]) / (yr[2] - yr[1]) * rect$height
      },
      flipped = isTRUE(panel_ranges[[panel]]$flipped),
      res = res
    )
  })
  names(affines) <- names(panel_ranges)
  affines
}

# Map data coordinates through a panel's affine to SVG (horizontal, vertical)
# positions, as a two-column matrix. Under `coord_flip()` the data x runs up the
# vertical axis and the data y across the horizontal one; ggplot2 implements the
# flip the same way, by swapping the two aesthetics before rescaling.
affine_xy <- function(affine, x, y) {
  if (isTRUE(affine$flipped)) {
    cbind(affine$to_svg_x(y), affine$to_svg_y(x))
  } else {
    cbind(affine$to_svg_x(x), affine$to_svg_y(y))
  }
}

# The gTree group of each panel, keyed by PANEL. ggplot2 names a panel's gTree
# "panel-<PANEL>", exported as `<g id="panel-<PANEL>.gTree.<n>.<m>">`. That name
# is the only reliable attribution: panels are drawn column-major
# (facet_grid(a ~ b) draws panel-1, panel-4, panel-2, ...) while PANEL numbers
# row-major, so document position does not identify a panel. The literal ".gTree"
# excludes the enclosing gtable viewport group and stops PANEL 1 from matching
# "panel-10.gTree".
panel_group_nodes <- function(doc) {
  nodes <- xml2::xml_find_all(doc, ".//g[starts-with(@id, 'panel-')]")
  ids <- xml2::xml_attr(nodes, "id")
  keep <- grepl("^panel-[0-9]+\\.gTree", ids)
  nodes <- as.list(nodes[keep])
  names(nodes) <- sub("^panel-([0-9]+)\\.gTree.*$", "\\1", ids[keep])
  nodes
}

# The exported rectangle of a panel. gridSVG names each viewport group after the
# viewport path and lists the same string in `exp$coords`, so the nearest
# enclosing panel viewport ancestor is the lookup key. The GRID.VP.<n>.<m>
# counter is session-global, so match it structurally.
panel_viewport_rect <- function(exp, group, panel) {
  ancestors <- xml2::xml_attr(
    xml2::xml_find_all(group, "ancestor::g[@id]"),
    "id"
  )
  keys <- ancestors[
    grepl("panel.*::GRID\\.VP\\.[0-9]+\\.[0-9]+$", ancestors) &
      ancestors %in% names(exp$coords)
  ]
  if (length(keys) == 0L) {
    cli::cli_abort(c(
      "Cannot locate the exported rectangle of a panel.",
      x = "PANEL {.val {panel}} has no panel viewport ancestor in the export."
    ))
  }
  exp$coords[[keys[[length(keys)]]]]
}

# Geometry per pch symbol, keyed by symbol id. gridSVG draws each pch inside a
# square viewBox scaled to the point's font size, so a symbol built from a single
# shape can be redrawn as that shape at the point's own position: record the kind
# and every coordinate divided by the viewBox width, and a point recovers user
# units by multiplying back by its font size. A composite pch (a circle and a
# cross, say) has no single shape to inline, so it keeps its `<use>`.
symbol_table <- function(doc) {
  tab <- list()
  for (s in xml2::xml_find_all(doc, ".//symbol")) {
    tab[[xml2::xml_attr(s, "id")]] <- symbol_entry(s)
  }
  tab
}

# A symbol that is left as a `<use>`: position and colour still animate, size
# does not.
frozen_symbol <- function() {
  list(kind = "use", geom = NULL, vbw = NA_real_)
}

symbol_is_frozen <- function(sym) {
  identical(sym$kind, "use")
}

symbol_entry <- function(symbol) {
  vb <- xml2::xml_attr(symbol, "viewBox")
  children <- xml2::xml_children(symbol)
  if (is.na(vb) || length(children) != 1L) {
    return(frozen_symbol())
  }
  vbw <- as.numeric(strsplit(trimws(vb), "\\s+")[[1]])[[3]]
  shape <- children[[1]]
  kind <- xml2::xml_name(shape)
  num <- function(a) as.numeric(xml2::xml_attr(shape, a)) / vbw
  geom <- switch(
    kind,
    circle = list(cx = num("cx"), cy = num("cy"), r = num("r")),
    rect = list(
      x = num("x"),
      y = num("y"),
      width = num("width"),
      height = num("height")
    ),
    polyline = ,
    polygon = list(
      points = parse_points(xml2::xml_attr(shape, "points")) / vbw
    ),
    NULL
  )
  if (is.null(geom) || anyNA(unlist(geom))) {
    return(frozen_symbol())
  }
  list(kind = kind, geom = geom, vbw = vbw)
}

# An SVG `points` string as a two-column vertex matrix.
parse_points <- function(s) {
  matrix(
    as.numeric(strsplit(trimws(s), "[,[:space:]]+")[[1]]),
    ncol = 2,
    byrow = TRUE
  )
}

# A symbol's attribute values for a point drawn at `(px, py)` with symbol scale
# `scale` (the `<use>`'s width). Vectorised over frames: `px`, `py` and `scale`
# may be length-nframes vectors, and each returned attribute matches. Absent
# frames come in as `NA` and go out as `NA`, for the caller to hold.
symbol_geometry <- function(sym, px, py, scale, precision = 3) {
  g <- sym$geom
  switch(
    sym$kind,
    circle = list(
      cx = px + g$cx * scale,
      cy = py + g$cy * scale,
      r = g$r * scale
    ),
    rect = list(
      x = px + g$x * scale,
      y = py + g$y * scale,
      width = g$width * scale,
      height = g$height * scale
    ),
    polyline = ,
    polygon = list(
      points = vapply(
        seq_along(px),
        function(f) {
          vertices_to_points(
            cbind(
              px[f] + g$points[, 1] * scale[f],
              py[f] + g$points[, 2] * scale[f]
            ),
            precision
          )
        },
        character(1)
      )
    ),
    # A frozen pch keeps the `<use>`, whose own (x, y) is the visible centre.
    list(x = px, y = py)
  )
}

# Replace a <use> point with the shape its symbol is built from, carrying the
# presentation attributes plus the animation id. `(x, y)` on the <use> is the
# visible centre and `width` is the symbol scale.
inline_symbol <- function(node, sym, layer_index, id) {
  scale <- as.numeric(xml2::xml_attr(node, "width"))
  geom <- symbol_geometry(
    sym,
    as.numeric(xml2::xml_attr(node, "x")),
    as.numeric(xml2::xml_attr(node, "y")),
    scale
  )
  attrs <- lapply(geom, function(v) {
    if (is.numeric(v)) format(v, trim = TRUE) else v
  })
  for (a in c("fill", "stroke", "fill-opacity", "stroke-opacity")) {
    v <- xml2::xml_attr(node, a)
    if (!is.na(v)) {
      attrs[[a]] <- v
    }
  }
  # gridSVG writes a <use>'s stroke-width pre-divided by the symbol scale,
  # because the viewBox transform scales it back up inside the symbol. Nothing
  # scales it outside, so multiply it back.
  sw <- as.numeric(xml2::xml_attr(node, "stroke-width"))
  if (!is.na(sw)) {
    attrs[["stroke-width"]] <- format(sw * scale / sym$vbw, trim = TRUE)
  }
  attrs[["data-animejs-id"]] <- id
  attrs[["data-layer"]] <- as.character(layer_index)
  do.call(xml2::xml_add_sibling, c(list(node, sym$kind), attrs))
  xml2::xml_remove(node)
}

# XPath predicate matching nodes inside one panel's group (see
# panel_group_nodes()).
in_panel_group <- function(panel) {
  sprintf("ancestor::g[starts-with(@id, 'panel-%s.gTree')]", panel)
}

# XPath predicate matching nodes inside one layer's group. name_layer_grobs()
# renames each panel's layer grobs, so the export carries a `<g>` per layer whose
# id is that name plus a counter suffix -- hence the trailing "." here.
in_layer_group <- function(layer_index) {
  sprintf(
    "ancestor::g[starts-with(@id, '%s.')]",
    layer_group_name(layer_index)
  )
}

# Data elements of one layer within one panel: the `<tag>` nodes inside both
# groups that are not grid lines. Axis ticks and legend keys are drawn outside
# the panel group, so this returns that layer's drawn data in document order.
# Panel membership alone is not enough -- grob names encode the geom, not the
# layer (and GeomPath supplies none), so two layers of the same family share one
# selector and a geom_ribbon layer's `stroke: none` outline polyline reaches the
# path adapter.
panel_data_nodes <- function(doc, tag, panel, layer_index) {
  xpath <- sprintf(
    ".//%s[not(starts-with(@id, 'panel.grid')) and %s and %s]",
    tag,
    in_panel_group(panel),
    in_layer_group(layer_index)
  )
  xml2::xml_find_all(doc, xpath)
}

# A layer's data nodes in union order, gathered panel by panel. `panels` is the
# PANEL of each element in union order and `select(doc, panel)` returns one
# panel's nodes in document order; within a panel the two orders agree, because
# ggplot2 draws a panel's rows in the order the union wrote them. Across panels
# they do not, hence the per-panel gather rather than one document-wide search.
ordered_data_nodes <- function(
  doc,
  select,
  layer_index,
  panels,
  element,
  tag,
  hint = NULL
) {
  positions <- split(seq_along(panels), factor(panels, levels = unique(panels)))
  nodes <- vector("list", length(panels))
  for (panel in names(positions)) {
    found <- select(doc, panel, layer_index)
    at <- positions[[panel]]
    if (length(found) != length(at)) {
      cli::cli_abort(c(
        "{element} element count does not match the union.",
        x = "Panel {panel}: found {length(found)} SVG {tag}{cli::qty(length(found))}{?s} but expected {length(at)}.",
        i = hint
      ))
    }
    nodes[at] <- as.list(found)
  }
  nodes
}

# Tag a node with the animation id without changing its element type.
set_element_id <- function(node, layer_index, id) {
  xml2::xml_set_attr(node, "data-animejs-id", id)
  xml2::xml_set_attr(node, "data-layer", as.character(layer_index))
  invisible(node)
}

# Write each track's first value onto its node.
#
# Anime.js tweens from an element's current attribute value into the first
# keyframe, so an authored attribute that disagrees with keyframe 1 shows for
# the first frame interval. Geometry agrees by construction: the reference is
# the element's first-appearance row, and hold_absent() back-fills the frames
# before it with that same row. Presence cannot -- gridSVG draws every union
# element, including the ones that only appear later, and nothing in the
# reference marks them as not yet drawn, so they would flash at full opacity
# before the first interval carried them to 0.
author_first_keyframes <- function(doc, elements) {
  nodes <- xml2::xml_find_all(doc, ".//*[@data-animejs-id]")
  ids <- vapply(elements, `[[`, character(1), "id")
  at <- match(ids, xml2::xml_attr(nodes, "data-animejs-id"))
  for (k in seq_along(elements)) {
    if (is.na(at[k])) {
      next
    }
    tracks <- elements[[k]]$tracks
    for (a in names(tracks)) {
      xml2::xml_set_attr(nodes[[at[k]]], a, svg_attr_value(tracks[[a]][[1]]))
    }
  }
  invisible(doc)
}

svg_attr_value <- function(x) {
  if (is.numeric(x)) format(x, trim = TRUE, scientific = FALSE) else x
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
