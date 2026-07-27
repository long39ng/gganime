# Synthetic raw rows shaped like shadow_params$raw[[layer]]: one row per mark
# (points) with a `.frame` column giving the frame each originates from.
make_raw_points <- function() {
  data.frame(
    .frame = c(1, 2, 3),
    PANEL = factor(1),
    .id = 1,
    group = c(1, 2, 3),
    x = c(10, 20, 30),
    y = c(1, 2, 3),
    shape = 19,
    size = 2,
    colour = "grey",
    alpha = NA_real_,
    stroke = 0.5,
    stringsAsFactors = FALSE
  )
}

# Two 3-vertex lines, one per frame, as a grouped raw layer.
make_raw_lines <- function() {
  data.frame(
    .frame = rep(c(1, 2), each = 3),
    PANEL = factor(1),
    .id = -1,
    group = rep(c(1, 2), each = 3),
    x = c(1, 2, 3, 1, 2, 3),
    y = c(1, 2, 1, 2, 3, 2),
    colour = "grey",
    alpha = NA_real_,
    stringsAsFactors = FALSE
  )
}

# The same line in two panels: ggplot2 numbers `group` across the whole layer,
# so both panels' lines share group 1.
make_faceted_raw_lines <- function() {
  data.frame(
    .frame = 1,
    PANEL = factor(rep(c(1, 2), each = 3)),
    .id = -1,
    group = 1,
    x = rep(c(1, 2, 3), 2),
    y = c(1, 2, 1, 3, 4, 3),
    colour = "grey",
    alpha = NA_real_,
    stringsAsFactors = FALSE
  )
}

# The visibility rule shadow_layer_union() takes, as build_shadow_unions() reads
# it off the spec.
mark_rule <- function(past = TRUE, future = FALSE) {
  shadow_presence_rule("mark", list(past = past, future = future))
}

trail_rule <- function(max_frames = Inf) {
  shadow_presence_rule("trail", list(max_frames = max_frames))
}

test_that("a grouped shadow keeps the panels of a shared group apart", {
  u <- shadow_layer_union(
    make_faceted_raw_lines(),
    mark_rule(),
    nframes = 2,
    grouped = TRUE
  )

  # One element per panel, not one merged element spanning both.
  expect_equal(ncol(u$union$presence), 2L)
  expect_equal(u$union$panels, c("1", "2"))
  expect_equal(u$union$frame_index[[1]][[1]], 1:3)
  expect_equal(u$union$frame_index[[1]][[2]], 4:6)
  expect_equal(u$union$union_data$group, rep(c(1L, 2L), each = 3))
})

test_that("shadow_presence encodes the past rule", {
  m <- shadow_presence(c(1, 2, 3), nframes = 3, past = TRUE, future = FALSE)
  expect_equal(m[, 1], c(FALSE, TRUE, TRUE))
  expect_equal(m[, 2], c(FALSE, FALSE, TRUE))
  expect_equal(m[, 3], c(FALSE, FALSE, FALSE))
})

test_that("shadow_presence encodes the future rule", {
  m <- shadow_presence(c(1, 2, 3), nframes = 3, past = FALSE, future = TRUE)
  expect_equal(m[, 1], c(FALSE, FALSE, FALSE))
  expect_equal(m[, 2], c(TRUE, FALSE, FALSE))
  expect_equal(m[, 3], c(TRUE, TRUE, FALSE))
})

test_that("shadow_presence with past and future shows every frame but the mark's own", {
  m <- shadow_presence(c(1, 2, 3), nframes = 3, past = TRUE, future = TRUE)
  expect_equal(diag(m), c(FALSE, FALSE, FALSE))
  expect_equal(m[, 2], c(TRUE, FALSE, TRUE))
})

test_that("trail_presence shows every earlier sample", {
  m <- trail_presence(c(1, 3, 5), nframes = 6, max_frames = Inf)
  expect_equal(m[, 1], c(FALSE, rep(TRUE, 5)))
  expect_equal(m[, 2], c(rep(FALSE, 3), TRUE, TRUE, TRUE))
  expect_equal(m[, 3], c(rep(FALSE, 5), TRUE))
})

test_that("trail_presence drops a sample once max_frames newer ones exist", {
  m <- trail_presence(c(1, 3, 5), nframes = 8, max_frames = 1)
  expect_equal(m[, 1], c(FALSE, TRUE, TRUE, rep(FALSE, 5)))
  expect_equal(m[, 2], c(rep(FALSE, 3), TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(m[, 3], c(rep(FALSE, 5), TRUE, TRUE, TRUE))
})

test_that("trail_presence counts frames, not elements, in the window", {
  # Two elements per sampled frame, so both elements of the older frame leave
  # together once the newer frame is sampled.
  m <- trail_presence(c(1, 1, 3, 3), nframes = 4, max_frames = 1)
  expect_equal(m[3, ], c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(m[4, ], c(FALSE, FALSE, TRUE, TRUE))
})

test_that("a trail element is shown for a stretch of frames and then hidden", {
  u <- shadow_layer_union(
    make_raw_points(),
    trail_rule(max_frames = 1),
    nframes = 4,
    grouped = FALSE
  )
  tracks <- gganime_element_tracks(
    geom_adapter("GeomPoint"),
    union = u$union,
    frames = u$frames,
    affines = identity_affines(ncol(u$union$presence)),
    precision = 2,
    ids = element_id(1, seq_len(ncol(u$union$presence))),
    symbols = pch_symbols(19)
  )
  # Geometry is as constant as a mark's, so opacity is again the only track.
  expect_equal(names(tracks[[1]]$tracks), "opacity")
  expect_equal(tracks[[1]]$tracks$opacity, c(0, 1, 0, 0))
  expect_equal(tracks[[2]]$tracks$opacity, c(0, 0, 1, 0))
})

test_that("build_shadow_unions reads the trail rule off the spec", {
  spec <- list(
    layers = list(list(geom_class = "GeomPoint")),
    nframes = 4,
    shadows = list(
      type = "trail",
      raw = list(make_raw_points()),
      params = list(max_frames = 1)
    )
  )
  u <- build_shadow_unions(spec)[[1]]
  expect_equal(u$union$presence[, 1], c(FALSE, TRUE, FALSE, FALSE))
})

test_that("build_shadow_unions is all-NULL for a mark showing neither side", {
  spec <- list(
    layers = list(list(geom_class = "GeomPoint")),
    nframes = 3,
    shadows = list(
      type = "mark",
      raw = list(make_raw_points()),
      params = list(past = FALSE, future = FALSE)
    )
  )
  expect_equal(build_shadow_unions(spec), list(NULL))
})

test_that("shadow_layer_union drops marks that are never shown", {
  u <- shadow_layer_union(
    make_raw_points(),
    mark_rule(),
    nframes = 3,
    grouped = FALSE
  )
  # The frame-3 mark has no later frame to appear on under `past`.
  expect_equal(ncol(u$union$presence), 2L)
  expect_equal(nrow(u$union$union_data), 2L)
  expect_equal(u$union$union_data$.frame, c(1, 2))
})

test_that("shadow_layer_union returns NULL when nothing is ever shown", {
  raw <- make_raw_points()[1, , drop = FALSE]
  expect_null(shadow_layer_union(
    raw,
    mark_rule(),
    nframes = 1,
    grouped = FALSE
  ))
})

test_that("shadow_layer_union frame_index is an integer vector for single geoms", {
  u <- shadow_layer_union(
    make_raw_points(),
    mark_rule(past = FALSE, future = TRUE),
    nframes = 3,
    grouped = FALSE
  )
  expect_type(u$union$frame_index[[1]], "integer")
})

test_that("shadow_layer_union groups grouped geoms by (.frame, group)", {
  u <- shadow_layer_union(
    make_raw_lines(),
    mark_rule(),
    nframes = 2,
    grouped = TRUE
  )
  # Only the frame-1 line is ever shown (on frame 2) under `past`.
  expect_equal(ncol(u$union$presence), 1L)
  expect_type(u$union$frame_index[[1]], "list")
  expect_equal(u$union$frame_index[[1]][[1]], 1:3)
  # group is reassigned to the element index for the reference polyline.
  expect_equal(unique(u$union$union_data$group), 1L)
})

test_that("a visible shadow element yields constant geometry and an opacity track", {
  u <- shadow_layer_union(
    make_raw_points(),
    mark_rule(),
    nframes = 3,
    grouped = FALSE
  )
  tracks <- gganime_element_tracks(
    geom_adapter("GeomPoint"),
    union = u$union,
    frames = u$frames,
    affines = identity_affines(ncol(u$union$presence)),
    precision = 2,
    ids = element_id(1, seq_len(ncol(u$union$presence))),
    symbols = pch_symbols(19)
  )
  # Geometry is constant across frames, so only opacity is kept per element.
  expect_equal(names(tracks[[1]]$tracks), "opacity")
  expect_equal(tracks[[1]]$tracks$opacity, c(0, 1, 1))
})

test_that("combine_shadow_union_data prepends shadow rows and offsets live groups", {
  shadow <- shadow_layer_union(
    make_raw_lines(),
    mark_rule(),
    nframes = 2,
    grouped = TRUE
  )
  live <- data.frame(group = c(1, 1, 2, 2), x = 1:4, y = 1:4, .id = 1)
  combined <- combine_shadow_union_data(shadow, live, grouped = TRUE)
  # One shadow element (group 1) then the two live elements (groups 2, 3).
  expect_equal(nrow(combined), 3L + 4L)
  expect_equal(combined$group, c(1L, 1L, 1L, 2L, 2L, 3L, 3L))
})

test_that("build_shadow_unions is all-NULL without a shadow_mark", {
  spec <- list(
    layers = list(list(geom_class = "GeomPoint")),
    shadows = NULL,
    nframes = 5
  )
  expect_equal(build_shadow_unions(spec), list(NULL))
})

test_that("shadow shape snapshot", {
  skip_if_not_installed("jsonlite")
  u <- shadow_layer_union(
    make_raw_points(),
    mark_rule(future = TRUE),
    nframes = 3,
    grouped = FALSE
  )
  expect_snapshot(
    cat(jsonlite::toJSON(
      list(
        frame = u$union$union_data$.frame,
        presence = u$union$presence
      ),
      auto_unbox = TRUE,
      pretty = TRUE
    ))
  )
})

# --- integration -----------------------------------------------------------

library(ggplot2)

shadow_point_plot <- function(past = TRUE, future = FALSE) {
  df <- data.frame(
    x = c(1, 2, 3, 1, 2, 3),
    y = c(1, 2, 1, 3, 1, 2),
    s = rep(c("a", "b"), each = 3)
  )
  ggplot(df, aes(x, y)) +
    geom_point(size = 4) +
    transition_states(s, transition_length = 1, state_length = 1) +
    shadow_mark(colour = "grey", size = 2, past = past, future = future)
}

test_that("anime() prepends shadow marks that animate only opacity", {
  w <- anime(shadow_point_plot(), nframes = 6, fps = 10)
  segs <- w$x$config$segments

  opacity_only <- vapply(
    segs,
    function(s) identical(names(s$props), "opacity"),
    logical(1)
  )
  # The shadow marks lead, each carrying a presence-only opacity track.
  expect_gt(sum(opacity_only), 0)
  expect_true(all(opacity_only[seq_len(sum(opacity_only))]))

  # Every element (shadow + live) is tagged in the SVG.
  n_ids <- lengths(gregexpr("data-animejs-id", w$x$svg))
  expect_equal(n_ids, length(segs))
})

test_that("a shadow mark is hidden before its frame and shown after (past)", {
  w <- anime(shadow_point_plot(past = TRUE), nframes = 6, fps = 10)
  segs <- w$x$config$segments
  shadow <- segs[[1]]
  opacity <- unlist(shadow$props$opacity)
  # Past shadows start hidden and end shown; the transition is monotone.
  expect_equal(opacity[[1]], 0)
  expect_equal(opacity[[length(opacity)]], 1)
  expect_false(is.unsorted(opacity))
})

shadow_trail_plot <- function(distance = 0.25, max_frames = Inf) {
  df <- data.frame(
    x = c(1, 2, 3, 1, 2, 3, 2, 3, 4),
    y = c(1, 2, 1, 3, 1, 2, 2, 3, 1),
    s = rep(c("a", "b", "c"), each = 3)
  )
  ggplot(df, aes(x, y)) +
    geom_point(size = 4) +
    transition_states(s, transition_length = 1, state_length = 1) +
    shadow_trail(
      distance = distance,
      max_frames = max_frames,
      colour = "grey",
      size = 2
    )
}

test_that("anime() prepends trail elements that animate only opacity", {
  w <- anime(shadow_trail_plot(), nframes = 8, fps = 10)
  segs <- w$x$config$segments

  opacity_only <- vapply(
    segs,
    function(s) identical(names(s$props), "opacity"),
    logical(1)
  )
  # One trail element per sampled frame and point, leading the live elements.
  expect_gt(sum(opacity_only), 3)
  expect_true(all(opacity_only[seq_len(sum(opacity_only))]))

  opacity <- unlist(segs[[1]]$props$opacity)
  expect_equal(opacity[[1]], 0)
  expect_equal(opacity[[length(opacity)]], 1)
})

test_that("max_frames hides a trail element again", {
  w <- anime(shadow_trail_plot(max_frames = 1), nframes = 8, fps = 10)
  opacity <- unlist(w$x$config$segments[[1]]$props$opacity)
  # The oldest sample is dropped as soon as a newer one is taken.
  expect_equal(opacity[[1]], 0)
  expect_equal(opacity[[length(opacity)]], 0)
  expect_true(any(opacity == 1))
})

test_that("anime() renders a line plot with a trail", {
  la <- data.frame(
    day = rep(1:4, 3),
    temp = c(1, 3, 2, 4, 2, 4, 3, 5, 5, 3, 4, 2),
    mon = rep(1:3, each = 4)
  )
  p <- ggplot(la, aes(day, temp)) +
    geom_line(colour = "red") +
    transition_time(mon) +
    shadow_trail(distance = 0.4, colour = "grey")
  w <- anime(p, nframes = 5, fps = 10)

  segs <- w$x$config$segments
  trail <- vapply(
    segs,
    function(s) identical(names(s$props), "opacity"),
    logical(1)
  )
  # A sampled frame's row-groups each become their own polyline element.
  expect_gt(sum(trail), 0)
  expect_true(all(trail[seq_len(sum(trail))]))
})

test_that("a trail whose distance rounds to no frames is rejected", {
  expect_snapshot(
    anime(shadow_trail_plot(distance = 0.001), nframes = 8),
    error = TRUE
  )
})

test_that("anime() renders a line plot with future shadows", {
  la <- data.frame(
    day = rep(1:4, 3),
    temp = c(1, 3, 2, 4, 2, 4, 3, 5, 5, 3, 4, 2),
    mon = rep(1:3, each = 4)
  )
  p <- ggplot(la, aes(day, temp)) +
    geom_line(colour = "red") +
    transition_time(mon) +
    shadow_mark(colour = "grey", past = FALSE, future = TRUE)
  w <- anime(p, nframes = 5, fps = 10)

  segs <- w$x$config$segments
  shadow_polylines <- vapply(
    segs,
    function(s) identical(names(s$props), "opacity"),
    logical(1)
  )
  expect_gt(sum(shadow_polylines), 0)
  expect_s3_class(w, "gganime")
})
