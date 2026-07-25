test_that("point_radius_px matches the gridSVG font-size mapping", {
  # font-size = size*.pt + stroke*.stroke/2 points, converted at res/72 (big
  # points), scaled by the symbol factor (pch19: r 3.75 in a 10-unit viewBox).
  factor <- 3.75 / 10
  fontsize <- 1 * ggplot2::.pt + 0.5 * ggplot2::.stroke / 2
  expect_equal(
    point_radius_px(1, 0.5, factor, res = 96),
    factor * fontsize * 96 / 72
  )
  # exported width for this point was 5.05 px; radius is 0.375 of it
  expect_equal(
    point_radius_px(1, 0.5, factor, res = 96),
    0.375 * 5.05,
    tolerance = 1e-2
  )
})

test_that("point_paint follows how each pch family is drawn", {
  row <- function(shape) {
    data.frame(
      shape = shape,
      colour = "#112233",
      fill = "#445566",
      alpha = NA_real_
    )
  }
  channels <- function(shape) point_paint(row(shape))[c("fill", "stroke")]
  # 0-14 stroked only, 15-18 filled only, 19-20 both in colour, 21-25 filled
  # from the fill aesthetic and stroked in colour
  expect_equal(channels(1), list(fill = NA_character_, stroke = "#112233"))
  expect_equal(channels(15), list(fill = "#112233", stroke = NA_character_))
  expect_equal(channels(19), list(fill = "#112233", stroke = "#112233"))
  expect_equal(channels(21), list(fill = "#445566", stroke = "#112233"))
})

test_that("point_paint leaves an unset colour as NA", {
  row <- data.frame(shape = 21, colour = "#112233", fill = NA, alpha = NA_real_)
  expect_equal(point_paint(row)$fill, NA_character_)
  expect_equal(point_paint(row)$stroke, "#112233")
})

test_that("point_paint combines the alpha aesthetic with the colour's alpha", {
  row <- function(colour, fill, alpha) {
    data.frame(shape = 21, colour = colour, fill = fill, alpha = alpha)
  }
  # no alpha anywhere
  paint <- point_paint(row("#112233", "#445566", NA_real_))
  expect_equal(paint$fill_opacity, 1)
  expect_equal(paint$stroke_opacity, 1)
  # the alpha aesthetic alone applies to both channels
  paint <- point_paint(row("#112233", "#445566", 0.5))
  expect_equal(paint$fill_opacity, 0.5)
  expect_equal(paint$stroke_opacity, 0.5)
  # a colour-borne alpha channel is per channel, and multiplies with the
  # aesthetic
  paint <- point_paint(row("#11223380", "#445566", NA_real_))
  expect_equal(paint$fill_opacity, 1)
  expect_equal(paint$stroke_opacity, 128 / 255)
  paint <- point_paint(row("#112233", "#44556680", 0.5))
  expect_equal(paint$fill_opacity, 0.5 * 128 / 255)
  expect_equal(paint$stroke_opacity, 0.5)
})
