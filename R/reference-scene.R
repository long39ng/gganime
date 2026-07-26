# The union scene rendered once as the static reference SVG. Each animated
# layer's frame-1 data is replaced by its union data (every element that ever
# appears), so the exported SVG carries a grob for every element.

#' Render the union reference gtable
#'
#' @param built A built `gganim` object.
#' @param layer_union_data List over layers; a union data frame to substitute,
#'   or `NULL` to keep the layer's frame-1 data (static / unhandled layers).
#' @param dynamic_labels Names of the label elements (title/subtitle/caption)
#'   that vary across frames. These are coerced to plain character so gridSVG
#'   renders them as a `<text>` node (not a MathML `<foreignObject>`, which is
#'   what set_labels' expression labels export as) that anime_text can swap.
#' @return A `gtable`.
#' @noRd
render_union_gtable <- function(
  built,
  layer_union_data,
  dynamic_labels = character(0)
) {
  frame <- built$scene$get_frame(built, 1L)
  for (i in seq_along(layer_union_data)) {
    ud <- layer_union_data[[i]]
    if (!is.null(ud)) {
      frame$data[[i]] <- ud
    }
  }
  for (el in dynamic_labels) {
    if (!is.null(frame$plot$labels[[el]])) {
      frame$plot$labels[[el]] <- as.character(frame$plot$labels[[el]])
    }
  }
  name_layer_grobs(frame_gtable(frame), length(frame$data))
}

# The grob name given to layer `i` in every panel. gridSVG names a `<g>` after
# the grob it exports, so this becomes the layer's group id (see in_layer_group()).
layer_group_name <- function(layer_index) {
  paste0("gganime.L", layer_index)
}

# How many of a panel's children come before layer 1: normally the coord
# background (the grill) then facet_bg, but `panel.ontop = TRUE` draws the grill
# second to last instead, leaving facet_bg alone in front.
panel_layer_offset <- function(children) {
  n <- length(children)
  grill_last <- startsWith(children[[n - 1L]]$name %||% "", "grill")
  if (grill_last) 1L else 2L
}

# Rename each panel's per-layer grobs so the export carries a group per layer.
# ggplot2 builds a panel's children as `c(facet_bg, <one grob per layer, in layer
# order>, facet_fg)` (Facet$draw_panel_content) and Coord$draw_panel wraps that
# with one background and one foreground grob, so a panel has `n_layers + 4`
# children. Static layers still occupy a slot, so a child's position matches its
# index in `spec$layers`.
name_layer_grobs <- function(gtable, n_layers) {
  cells <- which(grepl("^panel", gtable$layout$name))
  for (cell in cells) {
    panel <- gtable$grobs[[cell]]
    # An empty facet_wrap grid cell is a zeroGrob: nothing is drawn there, so
    # there are no nodes to attribute.
    if (inherits(panel, "zeroGrob")) {
      next
    }
    children <- panel$children
    n <- length(children)
    if (is.null(children) || n != n_layers + 4L) {
      cli::cli_abort(c(
        "Cannot identify the layer grobs of a panel.",
        x = "{.val {gtable$layout$name[[cell]]}} has {n} child grob{?s}, expected {n_layers + 4}.",
        i = "This is a ggplot2 layout change; gganime needs updating."
      ))
    }
    offset <- panel_layer_offset(children)
    for (i in seq_len(n_layers)) {
      children[[offset + i]]$name <- layer_group_name(i)
    }
    # setChildren() re-derives names and childrenOrder from the grob names.
    gtable$grobs[[cell]] <- grid::setChildren(panel, children)
  }
  gtable
}

# Rewrap a gganim_built frame as a ggplot_built so ggplot_gtable() dispatches.
# Adapted from gganimate's render_frame().
frame_gtable <- function(frame) {
  ccb <- get0("class_ggplot_built", asNamespace("ggplot2"))
  if (is.function(ccb)) {
    frame <- ccb(data = frame$data, layout = frame$layout, plot = frame$plot)
  } else {
    class(frame) <- "ggplot_built"
  }
  # ggplot_gtable() resolves grob sizes, which needs an open graphics device; if
  # none is open it auto-opens the default one, leaving a stray Rplots.pdf at the
  # working directory. Draw on a throwaway null device instead.
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  ggplot2::ggplot_gtable(frame)
}
