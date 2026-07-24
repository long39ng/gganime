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
    data.frame(shape = shape, colour = "#112233", fill = "#445566")
  }
  # 0-14 stroked only, 15-18 filled only, 19-20 both in colour, 21-25 filled
  # from the fill aesthetic and stroked in colour
  expect_equal(
    point_paint(row(1)),
    list(fill = NA_character_, stroke = "#112233")
  )
  expect_equal(
    point_paint(row(15)),
    list(fill = "#112233", stroke = NA_character_)
  )
  expect_equal(point_paint(row(19)), list(fill = "#112233", stroke = "#112233"))
  expect_equal(point_paint(row(21)), list(fill = "#445566", stroke = "#112233"))
})

test_that("point_paint leaves an unset colour as NA", {
  row <- data.frame(shape = 21, colour = "#112233", fill = NA)
  expect_equal(point_paint(row), list(fill = NA_character_, stroke = "#112233"))
})

test_that("to_hex normalises names and hex to #RRGGBB", {
  expect_equal(to_hex("black"), "#000000")
  expect_equal(to_hex(c("red", "white")), c("#FF0000", "#FFFFFF"))
  expect_equal(to_hex("#F8766D"), "#F8766D")
})
