skip_if_not_installed("gganimate")

library(ggplot2)

gganim_plot <- function() {
  ggplot(mtcars, aes(mpg, wt)) +
    geom_point() +
    transition_states(gear)
}

test_that("print.gganim renders via anime() when autoprint is on", {
  withr::local_options(gganime.autoprint = TRUE)
  called <- FALSE
  local_mocked_bindings(
    anime = function(plot, ...) {
      called <<- TRUE
      structure(list(), class = "gganime_stub")
    }
  )
  capture.output(print_gganim(gganim_plot()))
  expect_true(called)
})

test_that("print.gganim defers to gganimate when autoprint is off", {
  withr::local_options(gganime.autoprint = FALSE)
  called <- FALSE
  old <- gganime_env$prev_print
  withr::defer(gganime_env$prev_print <- old)
  gganime_env$prev_print <- function(x, ...) {
    called <<- TRUE
    invisible(x)
  }
  local_mocked_bindings(
    anime = function(plot, ...) stop("anime() must not be called")
  )
  print_gganim(gganim_plot())
  expect_true(called)
})
