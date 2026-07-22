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

test_that("point_fill picks fill for pch 21-25 and colour otherwise", {
  solid <- data.frame(shape = 19, colour = "#112233", fill = "#445566")
  filled <- data.frame(shape = 21, colour = "#112233", fill = "#445566")
  expect_equal(point_fill(solid), "#112233")
  expect_equal(point_fill(filled), "#445566")
})

test_that("to_hex normalises names and hex to #RRGGBB", {
  expect_equal(to_hex("black"), "#000000")
  expect_equal(to_hex(c("red", "white")), c("#FF0000", "#FFFFFF"))
  expect_equal(to_hex("#F8766D"), "#F8766D")
})
