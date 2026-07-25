# Colour helpers shared by the geom adapters.
#
# A colour can carry its own alpha channel, independently of the `alpha`
# aesthetic: gganimate's fade transmuters write the `alpha` column only where it
# is already non-NA, and fold the fade into `colour`/`fill` otherwise. So each
# colour splits into an opaque hex for a `fill`/`stroke` track plus an opacity
# for the matching `*-opacity` track, where it can tween.

# Opaque "#RRGGBB" per colour; NA in, NA out.
to_hex <- function(x) {
  out <- rep(NA_character_, length(x))
  ok <- !is.na(x)
  if (any(ok)) {
    m <- grDevices::col2rgb(x[ok])
    out[ok] <- grDevices::rgb(m[1, ], m[2, ], m[3, ], maxColorValue = 255)
  }
  out
}

# A colour's alpha channel as an opacity in 0-1; NA for an absent colour.
colour_opacity <- function(x) {
  out <- rep(NA_real_, length(x))
  ok <- !is.na(x)
  if (any(ok)) {
    out[ok] <- grDevices::col2rgb(x[ok], alpha = TRUE)[4, ] / 255
  }
  out
}

# The opacity a channel paints with: the `alpha` aesthetic (NA meaning unset)
# times the colour's own alpha channel.
paint_opacity <- function(alpha, colour) {
  a <- ifelse(is.na(alpha), 1, alpha)
  co <- colour_opacity(colour)
  a * ifelse(is.na(co), 1, co)
}
