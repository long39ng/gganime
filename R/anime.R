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
#' @examples
#' library(ggplot2)
#' library(gganimate)
#'
#' p <- ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point(aes(colour = factor(cyl))) +
#'   transition_states(gear)
#'
#' \donttest{
#' anime(p, nframes = 20)
#' }
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

  unions <- lapply(spec$layers, function(layer) {
    union_elements(layer$frames, grouped = geom_is_grouped(layer$geom_class))
  })
  shadow_unions <- build_shadow_unions(spec)

  # Shadow marks are prepended to each layer's union so they render first,
  # behind the live elements, with their own opacity-only tracks.
  layer_union_data <- lapply(seq_along(unions), function(i) {
    combine_shadow_union_data(
      shadow_unions[[i]],
      unions[[i]]$union_data,
      grouped = geom_is_grouped(spec$layers[[i]]$geom_class)
    )
  })

  dynamic_labels <- dynamic_label_names(spec$labels)
  gtable <- render_union_gtable(built, layer_union_data, dynamic_labels)
  export <- export_scene_svg(gtable, spec$panels)
  affine <- export$panels[["1"]]

  elements <- list()
  for (i in seq_along(spec$layers)) {
    live_union <- unions[[i]]
    shadow <- shadow_unions[[i]]
    adapter <- geom_adapter(spec$layers[[i]]$geom_class)

    n_shadow <- if (is.null(shadow)) 0L else ncol(shadow$union$presence)
    n_live <- ncol(live_union$presence)
    ids <- element_id(i, seq_len(n_shadow + n_live))

    # One annotate pass over every node in document order (shadows first),
    # keyed by the combined union data.
    gganime_annotate(
      adapter,
      doc = export$doc,
      layer_index = i,
      ids = ids,
      union_data = layer_union_data[[i]],
      symbols = export$symbols
    )

    if (n_shadow > 0L) {
      shadow_tracks <- gganime_element_tracks(
        adapter,
        union = shadow$union,
        frames = shadow$frames,
        affine = affine,
        precision = precision,
        ids = ids[seq_len(n_shadow)],
        symbols = export$symbols
      )
      elements <- c(elements, shadow_tracks)
    }

    live_tracks <- gganime_element_tracks(
      adapter,
      union = live_union,
      frames = spec$layers[[i]]$frames,
      affine = affine,
      precision = precision,
      ids = ids[n_shadow + seq_len(n_live)],
      symbols = export$symbols
    )
    elements <- c(elements, live_tracks)
  }

  elements <- c(elements, annotate_labels(export$doc, spec$labels))

  svg <- finalize_svg(export$doc)
  timeline <- build_timeline(elements, nframes_real, spec$fps, loop, controls)
  gganime_widget(timeline, svg, width, height, elementId)
}
