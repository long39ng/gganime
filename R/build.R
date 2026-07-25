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
    geom_class <- class(plot_layers[[i]]$geom)
    list(
      frames = repair_frame_ids(frames, grouped = geom_is_grouped(geom_class)),
      geom_class = geom_class,
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
      labels = precompute_labels(built),
      shadows = build_shadow_spec(built),
      built = built
    ),
    class = "gganime_scene_spec"
  )
}

# Shadow spec: NULL unless the plot carries a shadow_mark(). ShadowMark's train
# hook has already stored the raw-phase rows (styled by the aesthetic dots) with
# a `.frame` column per layer in `built$scene$shadow_params$raw`, so the spec
# only forwards the consumable pieces. R/shadows.R turns these into elements.
build_shadow_spec <- function(built) {
  shadow <- built$plot$shadow
  if (is.null(shadow) || !inherits(shadow, "ShadowMark")) {
    return(NULL)
  }
  sp <- built$scene$shadow_params
  list(
    past = isTRUE(sp$past),
    future = isTRUE(sp$future),
    raw = sp$raw,
    nframes = sp$nframes
  )
}

# Per-panel data ranges, keyed by PANEL level. The SVG rectangle and the affine
# that pairs with them are added later, in export_scene_svg().
#
# `x.range`/`y.range` are ranges of the *screen* axes, not of the data columns:
# under `coord_flip()` ggplot2 sets them up from the swapped scales, so
# `x.range` is the range of the data `y`. The `flipped` flag tells the affine
# which data column feeds which axis; both ranges are used as they come.
build_panel_ranges <- function(built) {
  pp <- built$layout$panel_params
  flipped <- inherits(built$plot$coordinates, "CoordFlip")
  panels <- lapply(seq_along(pp), function(i) {
    list(
      x_range = pp[[i]]$x.range,
      y_range = pp[[i]]$y.range,
      flipped = flipped
    )
  })
  names(panels) <- as.character(seq_along(pp))
  panels
}
