# Hand-built frames in the shape tweenr produces: two states, a tween between
# them, and the mislabelled arrival frame. State A holds two red points; state B
# holds three, of which the two red ones continue and the blue one arrives. The
# ids tweenr writes on the arrival frame (1, 2, NA) put the continuing ids on the
# first two rows, but the second continuation is row 3.
make_tweened <- function(
  arrival_ids = c(1, 2, NA),
  arrival_colour = c("red", "blue", "red")
) {
  state_a <- function(phase) {
    data.frame(
      PANEL = factor(c(1, 1)),
      x = c(1, 2),
      colour = c("red", "red"),
      group = c(1L, 1L),
      .id = c(1, 2),
      .phase = phase
    )
  }
  state_b <- function(phase) {
    data.frame(
      PANEL = factor(c(1, 1, 1)),
      x = c(10, 20, 30),
      colour = arrival_colour,
      group = c(3L, 4L, 3L),
      .id = arrival_ids,
      .phase = phase
    )
  }
  list(
    state_a("static"),
    state_a("raw"),
    data.frame(
      PANEL = factor(c(1, 1)),
      x = c(5.5, 16),
      colour = c("red", "red"),
      group = c(2L, 2L),
      .id = c(1, 2),
      .phase = "transition"
    ),
    state_b("raw"),
    state_b("static")
  )
}

test_that("repair_frame_ids moves the continuing ids onto the rows that continue", {
  out <- repair_frame_ids(make_tweened())
  # row 3 of the arrival frame continues .id 2; row 2 is new, so it gets a fresh
  # id past every id tweenr used
  expect_equal(out[[4]]$.id, c(1, 3, 2))
  # the whole block is relabelled, not just the arrival frame
  expect_equal(out[[5]]$.id, c(1, 3, 2))
  # the earlier block is untouched
  expect_equal(out[[1]]$.id, c(1, 2))
  expect_equal(out[[3]]$.id, c(1, 2))
})

test_that("the repaired ids give every element a continuous colour", {
  out <- repair_frame_ids(make_tweened())
  u <- union_elements(out)
  colour_of <- function(f, k) out[[f]]$colour[u$frame_index[[f]][k]]
  continued <- which(u$presence[2, ] & u$presence[4, ])
  expect_length(continued, 2)
  for (k in continued) {
    expect_equal(colour_of(2, k), colour_of(4, k))
  }
})

test_that("repair_frame_ids leaves the ids alone when the tuple cannot stand in for group", {
  # two groups sharing every aesthetic: occurrence order within a tuple no longer
  # reproduces occurrence order within a group
  frames <- make_tweened(arrival_colour = c("red", "red", "red"))
  expect_equal(repair_frame_ids(frames), frames)
})

# The shape tweenr produces when one element leaves at the same boundary another
# arrives at: the arriving element takes the `.id` the leaving one vacated, so
# every frame holds the ids 1 and 2 and only `.phase` marks the boundary.
make_reused_ids <- function() {
  state <- function(colour, phase) {
    data.frame(
      PANEL = factor(c(1, 1)),
      x = c(1, 2),
      colour = c("red", colour),
      group = c(1L, 2L),
      .id = c(1, 2),
      .phase = phase
    )
  }
  list(
    state("green", "static"),
    state("green", "raw"),
    data.frame(
      PANEL = factor(c(1, 1)),
      x = c(1, 2),
      colour = c("red", "green"),
      group = c(3L, 4L),
      .id = c(1, 2),
      .phase = "transition"
    ),
    state("blue", "raw"),
    state("blue", "static")
  )
}

test_that("repair_frame_ids splits a block where tweenr reused a vacated id", {
  out <- repair_frame_ids(make_reused_ids())

  # The green element left and the blue one arrived, so the blue one gets an id
  # past every id tweenr used instead of the one green vacated.
  expect_equal(out[[4]]$.id, c(1, 3))
  expect_equal(out[[5]]$.id, c(1, 3))
  expect_equal(out[[2]]$.id, c(1, 2))

  # Three elements, and only the red one is drawn in every frame.
  u <- union_elements(out)
  expect_equal(ncol(u$presence), 3L)
  expect_equal(colSums(u$presence), c(5L, 3L, 2L))
})

test_that("id_blocks starts a new block on the frame a tween lands on", {
  expect_equal(id_blocks(make_reused_ids()), list(1:3, 4:5))
})

test_that("repair_frame_ids leaves grouped layers and untweened frames alone", {
  frames <- make_tweened()
  expect_equal(repair_frame_ids(frames, grouped = TRUE), frames)
  expect_equal(repair_frame_ids(frames[1]), frames[1])

  bare <- lapply(frames, function(d) d[setdiff(names(d), ".phase")])
  expect_equal(repair_frame_ids(bare), bare)
})

# A path layer as gganimate hands it over: `keep_state()` runs before the first
# `transform_path()`, so the held frames of the first state are labelled one id
# per vertex, and every later frame one id per element.
make_path_frames <- function() {
  vertices <- data.frame(
    PANEL = factor(rep(1, 5)),
    x = c(1, 2, 3, 4, 5),
    y = c(1, 2, 3, 4, 5),
    group = c(1L, 1L, 2L, 2L, 2L)
  )
  frame <- function(id, phase, group) {
    cbind(
      vertices["PANEL"],
      vertices[c("x", "y")],
      group = group,
      .id = id,
      .phase = phase
    )
  }
  list(
    frame(1:5, "static", c(1L, 1L, 2L, 2L, 2L)),
    frame(1:5, "static", c(3L, 3L, 4L, 4L, 4L)),
    frame(c(1, 1, 2, 2, 2), "raw", c(5L, 5L, 6L, 6L, 6L))
  )
}

test_that("repair_frame_ids relabels vertex-labelled leading frames per element", {
  out <- repair_frame_ids(make_path_frames(), grouped = TRUE)
  expect_equal(out[[1]]$.id, c(1, 1, 2, 2, 2))
  expect_equal(out[[2]]$.id, c(1, 1, 2, 2, 2))
  expect_equal(out[[3]]$.id, c(1, 1, 2, 2, 2))
  # two lines, not five single-vertex elements plus two lines
  u <- union_elements(out, grouped = TRUE)
  expect_equal(ncol(u$presence), 2L)
  expect_true(all(u$presence))
})

test_that("repair_frame_ids leaves leading frames alone when they hold other data", {
  frames <- make_path_frames()
  frames[[1]]$x <- frames[[1]]$x + 1
  expect_equal(repair_frame_ids(frames, grouped = TRUE)[[1]]$.id, 1:5)
})

# Two panels of one red point each. Panel 1 gains a second red point in state B;
# panel 2 does not. ggplot2 numbers `group` across the whole layer, so both
# panels' reds share group 1 and the aesthetic tuple cannot tell them apart --
# pairing the frames whole would match panel 1's new row against panel 2's row.
make_two_panel_frames <- function() {
  frame <- function(panel, x, id, phase) {
    data.frame(
      PANEL = factor(panel, levels = c(1, 2)),
      x = x,
      colour = "red",
      group = 1L,
      .id = id,
      .phase = phase
    )
  }
  state_b <- function(phase) {
    frame(c(1, 1, 2), c(2, 3, 20), c(1, 2, NA), phase)
  }
  list(
    frame(c(1, 2), c(1, 10), c(1, 1), "raw"),
    state_b("raw"),
    state_b("static")
  )
}

test_that("the id repair pairs rows within a panel", {
  out <- repair_frame_ids(make_two_panel_frames())

  # Panel 1's two rows keep separate ids and panel 2's row stays paired with the
  # row it continues. Pairing across the whole frame would give both panel 1
  # rows the same id and hand panel 2 a fresh one.
  expect_equal(out[[2]]$.id, c(1, 3, 1))
  expect_equal(out[[3]]$.id, c(1, 3, 1))
  expect_equal(out[[1]]$.id, c(1, 1))

  # Three elements, and panel 2's element is present in both states.
  u <- union_elements(out)
  expect_equal(ncol(u$presence), 3L)
  expect_equal(u$panels, c("1", "2", "1"))
  expect_true(all(u$presence[, u$panels == "2"]))
})

test_that("the id repair of one panel matches repairing it alone", {
  two <- make_two_panel_frames()
  one <- lapply(two, function(d) {
    d <- d[d$PANEL == "2", , drop = FALSE]
    rownames(d) <- NULL
    d
  })

  repaired <- repair_frame_ids(two)
  expect_equal(
    lapply(repaired, function(d) d$.id[d$PANEL == "2"]),
    lapply(repair_frame_ids(one), `[[`, ".id")
  )
  # Only `.id` is rewritten; row order and every other column are untouched.
  expect_equal(
    lapply(repaired, function(d) d[setdiff(names(d), ".id")]),
    lapply(two, function(d) d[setdiff(names(d), ".id")])
  )
})

test_that("tuple_pairs pairs the j-th occurrence of each aesthetic tuple", {
  frames <- make_tweened()
  expect_equal(tuple_pairs(frames[[2]], frames[[4]]), c(1L, NA, 2L))
})

test_that("group_constant_columns drops aesthetics that vary within a group", {
  from <- data.frame(
    colour = c("red", "red"),
    size = c(1, 2),
    group = c(1L, 1L)
  )
  to <- data.frame(colour = c("red", "red"), size = c(3, 4), group = c(2L, 2L))
  expect_equal(group_constant_columns(from, to), "colour")
})

test_that("discrete_position_columns names the axes a category is mapped onto", {
  bars <- ggplot2::ggplot_build(
    ggplot2::ggplot(
      data.frame(g = c("a", "b"), v = c(1, 2)),
      ggplot2::aes(g, v)
    ) +
      ggplot2::geom_col()
  )
  points <- ggplot2::ggplot_build(
    ggplot2::ggplot(
      data.frame(x = c(1, 2), y = c(1, 2)),
      ggplot2::aes(x, y)
    ) +
      ggplot2::geom_point()
  )

  expect_equal(discrete_position_columns(bars), "x")
  expect_null(discrete_position_columns(points))
})

test_that("group_constant_columns keeps a position only where its scale is discrete", {
  # A bar's `ymax` moves between states and its `x` does not, but only the scale
  # behind them distinguishes the two.
  from <- data.frame(
    x = c(1, 2),
    ymax = c(3, 4),
    fill = c("grey35", "grey35"),
    group = c(1L, 2L)
  )
  to <- data.frame(
    x = c(1, 2),
    ymax = c(5, 6),
    fill = c("grey35", "grey35"),
    group = c(1L, 2L)
  )

  expect_equal(group_constant_columns(from, to), "fill")
  expect_equal(group_constant_columns(from, to, discrete = "x"), c("x", "fill"))
  # Without a column that tells the two bars apart there is no pairing to make.
  expect_null(tuple_pairs(from, to))
  expect_equal(tuple_pairs(from, to, discrete = "x"), c(1L, 2L))
})
