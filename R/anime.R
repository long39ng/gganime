#' Render a gganimate plot as an animated-SVG widget
#'
#' Builds a ggplot2 plot written with gganimate syntax and renders it as a
#' self-contained, resolution-independent animated-SVG htmlwidget powered by
#' Anime.js, instead of gganimate's frame-by-frame output.
#'
#' @param plot A `gganim` plot: a ggplot2 plot with a `transition_*()` added.
#' @param nframes Number of frames to sample the animation at.
#' @param fps Frames per second, setting the playback speed.
#' @param duration Optional total duration in seconds; overrides `fps`.
#' @param width,height Widget size in pixels. `NULL` fills the container.
#' @param precision Decimal places to round animated coordinates to.
#' @param loop Loop the animation: `TRUE`, `FALSE`, or a number of iterations.
#' @param controls Show a play/pause and scrub control bar.
#' @param elementId Optional htmlwidget element id.
#' @param ... Unused; reserved for future arguments.
#'
#' @return An `htmlwidget` of class `gganime`.
#' @export
anime <- function(
  plot,
  nframes = 100,
  fps = 10,
  duration = NULL,
  width = NULL,
  height = NULL,
  precision = 2,
  loop = TRUE,
  controls = TRUE,
  elementId = NULL,
  ...
) {
  if (!inherits(plot, "gganim")) {
    cli::cli_abort(c(
      "{.arg plot} must be a {.cls gganim} plot.",
      i = "Add a transition, e.g. {.code + transition_states(state)}, to a ggplot."
    ))
  }
  nframes <- check_count(nframes, "nframes")
  check_number(fps, "fps", min = 0)
  precision <- check_count(precision, "precision", min = 0L)
  if (!is.null(width)) {
    check_number(width, "width", min = 0)
  }
  if (!is.null(height)) {
    check_number(height, "height", min = 0)
  }
  check_bool(controls, "controls")
  if (!is.null(duration)) {
    check_number(duration, "duration", min = 0)
    fps <- nframes / duration
  }

  check_supported_prebuild(plot)

  plot$nframes <- nframes
  built <- ggplot2::ggplot_build(plot)
  nframes_real <- built$scene$nframes

  check_supported_postbuild(built)

  spec <- build_scene_spec(built, fps)

  unions <- lapply(spec$layers, function(layer) union_elements(layer$frames))
  layer_union_data <- lapply(unions, `[[`, "union_data")

  gtable <- render_union_gtable(built, layer_union_data)
  export <- export_scene_svg(gtable, spec$panels)
  affine <- export$panels[["1"]]

  elements <- list()
  for (i in seq_along(spec$layers)) {
    union <- unions[[i]]
    adapter <- geom_adapter(spec$layers[[i]]$geom_class)
    ids <- element_id(i, seq_len(ncol(union$presence)))

    gganime_annotate(
      adapter,
      doc = export$doc,
      layer_index = i,
      ids = ids,
      union_data = union$union_data,
      symbols = export$symbols
    )
    tracks <- gganime_element_tracks(
      adapter,
      union = union,
      frames = spec$layers[[i]]$frames,
      affine = affine,
      precision = precision,
      ids = ids,
      symbols = export$symbols
    )
    elements <- c(elements, tracks)
  }

  svg <- finalize_svg(export$doc)
  timeline <- build_timeline(elements, nframes_real, spec$fps, loop, controls)
  gganime_widget(timeline, svg, width, height, elementId)
}
