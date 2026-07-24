# Hand-built frames: a stable point (.id 1), an exit (.id 2, gone after frame 2),
# and an entrant (.id 3, appears at frame 3).
make_frames <- function() {
  frame <- function(ids, xs) {
    data.frame(
      PANEL = factor(rep(1, length(ids))),
      .id = ids,
      x = xs,
      y = xs * 10,
      stringsAsFactors = FALSE
    )
  }
  list(
    frame(c(1, 2), c(1, 2)),
    frame(c(1, 2), c(3, 4)),
    frame(c(1, 3), c(5, 6))
  )
}

test_that("union_elements orders by first appearance then .id", {
  u <- union_elements(make_frames())
  expect_equal(u$keys$.id, c(1, 2, 3))
  expect_equal(u$keys$first_frame, c(1, 1, 3))
})

test_that("presence bitmap records per-frame membership", {
  u <- union_elements(make_frames())
  # columns follow key order (.id 1, 2, 3)
  expect_equal(u$presence[, 1], c(TRUE, TRUE, TRUE)) # stable
  expect_equal(u$presence[, 2], c(TRUE, TRUE, FALSE)) # exits
  expect_equal(u$presence[, 3], c(FALSE, FALSE, TRUE)) # enters
})

test_that("union_data carries the first-appearance row per element", {
  u <- union_elements(make_frames())
  expect_equal(nrow(u$union_data), 3)
  expect_equal(u$union_data$.id, c(1, 2, 3))
  # .id 3 first appears in frame 3 with x = 6
  expect_equal(u$union_data$x[u$union_data$.id == 3], 6)
})

test_that("rows with no .id get one union slot each, keyed by position", {
  frames <- make_frames()
  # the wrap frame of a transition_states() loop: tweenr identifies the rows it
  # could match and leaves the rest NA
  frames[[3]] <- data.frame(
    PANEL = factor(rep(1, 4)),
    .id = c(1, NA, NA, NA),
    x = c(5, 6, 7, 8),
    y = c(50, 60, 70, 80),
    stringsAsFactors = FALSE
  )
  u <- expect_silent(union_elements(frames))
  # .id 1 and 2 from the earlier frames, plus one slot per unidentified row
  expect_equal(ncol(u$presence), 5)
  expect_equal(rowSums(u$presence), c(2, 2, 4))
  expect_equal(u$keys$.id, c(1, 2, NA, NA, NA))
  # each unidentified row keeps its own geometry
  expect_equal(sort(u$union_data$x[is.na(u$union_data$.id)]), c(6, 7, 8))
})

test_that("frame_index maps keys to their row within each frame", {
  u <- union_elements(make_frames())
  # frame 3 has .id 1 (row 1) and .id 3 (row 2); .id 2 absent (NA)
  idx <- u$frame_index[[3]]
  expect_equal(unname(idx[u$order == "1\r1"]), 1L)
  expect_true(is.na(idx[["1\r2"]]))
})

# Grouped mode: two lines, each a row-group of vertices. Line .id 1 grows
# (2 -> 3 rows), line .id 2 is constant (2 rows) and exits after frame 2.
make_line_frames <- function() {
  grp <- function(id, x) {
    data.frame(
      PANEL = factor(1),
      .id = id,
      x = x,
      y = x,
      group = id,
      stringsAsFactors = FALSE
    )
  }
  list(
    rbind(grp(1, c(0, 1)), grp(2, c(0, 1))),
    rbind(grp(1, c(0, 1, 2)), grp(2, c(0, 1))),
    grp(1, c(0, 1, 2))
  )
}

test_that("grouped frame_index carries every vertex row of a key", {
  u <- union_elements(make_line_frames(), grouped = TRUE)
  # frame 2: .id 1 is rows 1:3, .id 2 is rows 4:5
  expect_equal(u$frame_index[[2]][["1\r1"]], 1:3)
  expect_equal(u$frame_index[[2]][["1\r2"]], 4:5)
  # frame 3: .id 2 absent -> empty
  expect_length(u$frame_index[[3]][["1\r2"]], 0L)
})

test_that("grouped union_data pads each element's first frame to its max arity", {
  u <- union_elements(make_line_frames(), grouped = TRUE)
  # .id 1 peaks at 3 vertices, .id 2 at 2 -> 5 rows total
  expect_equal(nrow(u$union_data), 5L)
  expect_equal(as.integer(table(u$union_data$.id)), c(3L, 2L))
  # .id 1 has 2 vertices in frame 1, so the third repeats the last one
  expect_equal(u$union_data$x[u$union_data$.id == 1], c(0, 1, 1))
  # group reassigned to a unique integer per element in keys order
  expect_equal(unique(u$union_data$group), c(1, 2))
})

test_that("grouped presence tracks the element, not each vertex", {
  u <- union_elements(make_line_frames(), grouped = TRUE)
  expect_equal(u$presence[, 1], c(TRUE, TRUE, TRUE))
  expect_equal(u$presence[, 2], c(TRUE, TRUE, FALSE))
})
