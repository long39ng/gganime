# Timeline assembly through animejs's public constructors. gganime never touches
# the animejs config schema directly.

# Stable per-element id, shared by SVG annotation and keyframe assembly.
element_id <- function(layer_index, k) {
  sprintf("L%de%d", layer_index, k)
}

#' Assemble the animation timeline
#'
#' @param elements List of `list(id, tracks)`; `tracks` is a named list of
#'   length-nframes value vectors.
#' @param nframes,fps Timeline length.
#' @param loop,controls Playback options.
#' @return An `anime_timeline` with playback set.
#' @noRd
build_timeline <- function(elements, nframes, fps, loop, controls) {
  tl <- animejs::anime_timeline(
    duration = nframes / fps * 1000,
    ease = animejs::anime_easing("linear")
  )
  for (el in elements) {
    if (length(el$tracks) == 0L) {
      next
    }
    props <- lapply(el$tracks, function(v) {
      rlang::inject(animejs::anime_keyframes(!!!v))
    })
    tl <- animejs::anime_add(
      tl,
      selector = animejs::anime_target_id(el$id),
      props = props,
      offset = 0
    )
  }
  animejs::anime_playback(tl, loop = loop, controls = controls)
}
