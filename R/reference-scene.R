# The union scene rendered once as the static reference SVG. Each animated
# layer's frame-1 data is replaced by its union data (every element that ever
# appears), so the exported SVG carries a grob for every element.

#' Render the union reference gtable
#'
#' @param built A built `gganim` object.
#' @param layer_union_data List over layers; a union data frame to substitute,
#'   or `NULL` to keep the layer's frame-1 data (static / unhandled layers).
#' @return A `gtable`.
#' @noRd
render_union_gtable <- function(built, layer_union_data) {
  frame <- built$scene$get_frame(built, 1L)
  for (i in seq_along(layer_union_data)) {
    ud <- layer_union_data[[i]]
    if (!is.null(ud)) {
      frame$data[[i]] <- ud
    }
  }
  frame_gtable(frame)
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
  ggplot2::ggplot_gtable(frame)
}
