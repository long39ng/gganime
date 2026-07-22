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

test_that("normalise_vertices pads to the max arity by repeating the last vertex", {
  verts <- list(
    matrix(c(0, 1, 0, 1), ncol = 2), # 2 vertices: (0,0), (1,1)
    matrix(c(0, 1, 2, 0, 1, 2), ncol = 2) # 3 vertices
  )
  out <- normalise_vertices(verts)
  expect_equal(nrow(out[[1]]), 3L)
  expect_equal(out[[1]][3, ], out[[1]][2, ]) # padded row repeats the last
  expect_equal(nrow(out[[2]]), 3L) # already at max, unchanged
  expect_equal(out[[2]], verts[[2]])
})

test_that("normalise_vertices leaves absent (NULL) frames NULL", {
  verts <- list(
    matrix(c(0, 1, 0, 1), ncol = 2),
    NULL,
    matrix(c(0, 1, 2, 3, 4, 5), ncol = 2)
  )
  out <- normalise_vertices(verts)
  expect_null(out[[2]])
  expect_equal(nrow(out[[1]]), 3L)
})

test_that("vertices_to_points formats and rounds to a points string", {
  m <- matrix(c(1.234, 5.678, 9.111, 2), ncol = 2, byrow = TRUE)
  expect_equal(vertices_to_points(m, 1), "1.2,5.7 9.1,2")
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
