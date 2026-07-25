# Adapters take one affine per element, in union order. Unit tests that check
# track values rather than coordinate mapping pass the identity.

identity_affine <- function(flipped = FALSE) {
  list(
    to_svg_x = function(x) x,
    to_svg_y = function(y) y,
    flipped = flipped,
    res = 96
  )
}

identity_affines <- function(n) {
  rep(list(identity_affine()), n)
}
