skip_if_not_installed("gganimate")

library(ggplot2)
library(gganimate)

test_that("anime() rejects a plain ggplot", {
  p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
  expect_snapshot(anime(p), error = TRUE)
})

test_that("a gganim without a supported geom is gated pre-build", {
  p <- ggplot(economics, aes(date, unemploy)) +
    geom_line() +
    transition_reveal(date)
  expect_snapshot(check_supported_prebuild(p), error = TRUE)
})

test_that("a gganim with no transition is gated", {
  # enter_fade() makes it a gganim but installs no transition.
  p <- ggplot(mtcars, aes(mpg, wt)) + geom_point() + enter_fade()
  expect_snapshot(check_supported_prebuild(p), error = TRUE)
})
