test_that("hold_absent fills absent frames from the nearest present frame", {
  present <- c(FALSE, TRUE, FALSE, TRUE, FALSE)
  values <- c(NA, 20, NA, 40, NA)
  # frame 1 -> 2, frame 3 ties (2 vs 4) -> earlier, frame 5 -> 4
  expect_equal(hold_absent(values, present), c(20, 20, 20, 40, 40))
})

test_that("hold_absent holds entry geometry before appearance and exit after", {
  present <- c(FALSE, FALSE, TRUE, TRUE)
  expect_equal(hold_absent(c(NA, NA, 5, 7), present), c(5, 5, 5, 7))

  present <- c(TRUE, TRUE, FALSE, FALSE)
  expect_equal(hold_absent(c(5, 7, NA, NA), present), c(5, 7, 7, 7))
})

test_that("hold_absent is a no-op when all or none present", {
  expect_equal(hold_absent(c(1, 2, 3), c(TRUE, TRUE, TRUE)), c(1, 2, 3))
  expect_equal(
    hold_absent(c(NA, NA), c(FALSE, FALSE)),
    c(NA, NA)
  )
})

test_that("presence_opacity is 1 present / 0 absent", {
  expect_equal(presence_opacity(c(TRUE, FALSE, TRUE)), c(1, 0, 1))
})

test_that("round_track rounds numerics and leaves characters", {
  expect_equal(round_track(c(1.234, 5.678), 1), c(1.2, 5.7))
  expect_equal(round_track(c("#fff", "#000"), 2), c("#fff", "#000"))
})

test_that("track_is_constant detects unchanging tracks", {
  expect_true(track_is_constant(c(3, 3, 3)))
  expect_true(track_is_constant(3))
  expect_true(track_is_constant(c("a", "a")))
  expect_false(track_is_constant(c(3, 4, 3)))
})

test_that("drop_constant_tracks keeps varying tracks and any named keeps", {
  tracks <- list(
    cx = c(1, 2, 3),
    r = c(5, 5, 5),
    opacity = c(1, 1, 1)
  )
  expect_named(drop_constant_tracks(tracks), "cx")
  expect_named(
    drop_constant_tracks(tracks, keep = "opacity"),
    c("cx", "opacity")
  )
})
