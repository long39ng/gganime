# The gganimate boundary: the only producer-side file that reads a built
# `gganim` object. Everything downstream consumes the `gganime_scene_spec` list.

#' Build a scene spec from a built gganim object
#'
#' @param built Result of [ggplot2::ggplot_build()] on a `gganim` plot.
#' @param fps Frames per second, used for the timeline duration.
#' @return A `gganime_scene_spec` list.
#' @noRd
build_scene_spec <- function(built, fps) {
  plot_layers <- built$plot$layers

  layers <- lapply(seq_along(built$data), function(i) {
    frames <- built$data[[i]]
    list(
      frames = frames,
      geom_class = class(plot_layers[[i]]$geom),
      static = length(frames) == 1L
    )
  })

  structure(
    list(
      layers = layers,
      frame_vars = built$scene$frame_vars,
      nframes = built$scene$nframes,
      fps = fps,
      panels = build_panel_ranges(built),
      built = built
    ),
    class = "gganime_scene_spec"
  )
}

# Per-panel data ranges, keyed by PANEL level. The SVG rectangle and the affine
# that pairs with them are added later, in export_scene_svg().
build_panel_ranges <- function(built) {
  pp <- built$layout$panel_params
  panels <- lapply(seq_along(pp), function(i) {
    list(x_range = pp[[i]]$x.range, y_range = pp[[i]]$y.range)
  })
  names(panels) <- as.character(seq_along(pp))
  panels
}
