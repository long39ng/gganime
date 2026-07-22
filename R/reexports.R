# Re-export the gganimate syntax so users' literal gganimate code runs unchanged
# after loading gganime.

#' @importFrom gganimate transition_states
#' @export
gganimate::transition_states

#' @importFrom gganimate transition_time
#' @export
gganimate::transition_time

#' @importFrom gganimate transition_reveal
#' @export
gganimate::transition_reveal

#' @importFrom gganimate ease_aes
#' @export
gganimate::ease_aes

#' @importFrom gganimate enter_fade
#' @export
gganimate::enter_fade

#' @importFrom gganimate enter_grow
#' @export
gganimate::enter_grow

#' @importFrom gganimate exit_fade
#' @export
gganimate::exit_fade

#' @importFrom gganimate exit_shrink
#' @export
gganimate::exit_shrink
