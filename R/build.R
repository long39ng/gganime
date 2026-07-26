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
  discrete <- discrete_position_columns(built)

  layers <- lapply(seq_along(built$data), function(i) {
    frames <- built$data[[i]]
    geom_class <- class(plot_layers[[i]]$geom)
    # A layer that does not depend on the transition variable is built as a
    # single frame rather than one per frame. Repeating it across the timeline
    # lets it share the union and adapter path: every track then comes out
    # constant and is dropped, so it is drawn once and never animates.
    static <- length(frames) == 1L
    if (static) {
      frames <- rep(frames, built$scene$nframes)
    }
    list(
      frames = repair_frame_ids(
        frames,
        grouped = geom_is_grouped(geom_class),
        discrete = discrete
      ),
      geom_class = geom_class,
      static = static
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

# The data columns whose position comes from a discrete scale. A category keeps
# its mapped position across every state, so `repair_frame_ids()` can pair
# elements on it where no aesthetic tells them apart. A bar chart is the usual
# case. The frames hold data columns, so the scales are read unswapped even under
# `coord_flip()`.
discrete_position_columns <- function(built) {
  c(
    if (panel_scale_is_discrete(built$layout$panel_scales_x)) "x",
    if (panel_scale_is_discrete(built$layout$panel_scales_y)) "y"
  )
}

# Free scales give one scale per panel row or column; discreteness is a property
# of the mapped column, so any of them answers for all.
panel_scale_is_discrete <- function(scales) {
  length(scales) > 0L && isTRUE(scales[[1L]]$is_discrete())
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
