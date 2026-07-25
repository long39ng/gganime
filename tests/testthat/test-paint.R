test_that("to_hex normalises names and hex to #RRGGBB", {
  expect_equal(to_hex("black"), "#000000")
  expect_equal(to_hex(c("red", "white")), c("#FF0000", "#FFFFFF"))
  expect_equal(to_hex("#F8766D"), "#F8766D")
})

test_that("to_hex drops the alpha channel and passes NA through", {
  expect_equal(to_hex("#F8766D40"), "#F8766D")
  expect_equal(to_hex(NA), NA_character_)
  expect_equal(to_hex(c("#F8766D40", NA)), c("#F8766D", NA))
})

test_that("colour_opacity reads a colour's alpha channel", {
  expect_equal(colour_opacity("#F8766D"), 1)
  expect_equal(colour_opacity("#F8766D40"), 64 / 255)
  expect_equal(colour_opacity("transparent"), 0)
  expect_equal(colour_opacity(c("red", NA)), c(1, NA))
})

test_that("paint_opacity multiplies the alpha aesthetic by the colour's", {
  expect_equal(paint_opacity(NA_real_, "#F8766D"), 1)
  expect_equal(paint_opacity(0.4, "#F8766D"), 0.4)
  expect_equal(paint_opacity(NA_real_, "#F8766D80"), 128 / 255)
  expect_equal(paint_opacity(0.5, "#F8766D80"), 0.5 * 128 / 255)
  # an absent colour paints nothing, so only the aesthetic is left
  expect_equal(paint_opacity(0.4, NA), 0.4)
})
