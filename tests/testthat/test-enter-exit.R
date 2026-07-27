library(ggplot2)

# A transmuter modifies the tweened rows, so an adapter reads its effect through
# the ordinary per-frame read and needs no code of its own. Each family is
# asserted on the track that separates it from the others: fade on the paint
# opacity, grow on the size, drift and fly on the position, recolour on the paint.
# gganime re-exports four transmuters; the rest need a `gganimate::` prefix.

# Points 1 to 4 run through both states. Point 5 leaves as point 6 arrives, and
# point 5 arrives again when the state loop wraps, so three of the seven union
# elements are transient.
swap_points <- function() {
  df <- data.frame(
    id = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 6),
    state = rep(c("a", "b"), each = 5),
    x = c(1, 2, 3, 4, 5, 5, 4, 3, 2, 1),
    y = c(2, 4, 1, 5, 3, 5, 1, 4, 2, 3)
  )
  ggplot(df, aes(x, y, group = id, colour = factor(id))) +
    geom_point(size = 4) +
    transition_states(state, transition_length = 1, state_length = 1)
}

# Bars a and b run through both states; c leaves as d arrives.
swap_bars <- function() {
  df <- data.frame(
    g = c("a", "b", "c", "a", "b", "d"),
    state = rep(c("s1", "s2"), each = 3),
    v = c(10, 90, 40, 60, 20, 80)
  )
  ggplot(df, aes(g, v)) +
    geom_col() +
    transition_states(state, transition_length = 1, state_length = 1)
}

# Lines p and q run through both states; r belongs to the second one only, so it
# arrives at the first boundary and leaves again when the loop wraps.
swap_lines <- function() {
  data.frame(
    grp = c(rep("p", 6), rep("q", 6), rep("r", 3)),
    state = c(
      rep(c("s1", "s2"), each = 3),
      rep(c("s1", "s2"), each = 3),
      rep("s2", 3)
    ),
    x = c(rep(1:3, 4), 1:3),
    y = c(1, 3, 2, 3, 1, 2, 2, 1, 3, 1, 2, 3, 2, 2, 1)
  )
}

# Only an element that enters or exits has a presence channel, which is how the
# transient ones are found.
transient_segments <- function(widget) {
  Filter(
    function(s) "opacity" %in% names(s$props),
    widget$x$config$segments
  )
}

# The props animated on the transient elements, as one sorted set.
transient_props <- function(widget) {
  animated_props(transient_segments(widget))
}

# The elements that arrive, in union order. An arriving element is authored
# invisible, so its presence channel starts at 0.
entering_segments <- function(widget) {
  Filter(
    function(s) identical(s$props$opacity[[1]]$to, 0),
    widget$x$config$segments
  )
}

# The element that arrives last. Its tracks run from the transmuted value to the
# data value, so the assertions below compare its first keyframe with its last.
last_entering <- function(widget) {
  entering <- entering_segments(widget)
  entering[[length(entering)]]
}

test_that("the default transmuters give a transient point a presence channel only", {
  w <- anime(swap_points(), nframes = 20, fps = 10)

  expect_length(transient_segments(w), 3L)
  # Nothing is transmuted, so the elements appear and disappear where they are:
  # the vacated element holds its position instead of moving to the arriving one.
  expect_equal(transient_props(w), "opacity")
})

test_that("enter_appear() and exit_disappear() are the defaults spelled out", {
  w <- anime(
    swap_points() + gganimate::enter_appear() + gganimate::exit_disappear(),
    nframes = 20,
    fps = 10
  )

  expect_length(transient_segments(w), 3L)
  expect_equal(transient_props(w), "opacity")
})

test_that("enter_reset() clears a transmuter added before it", {
  w <- anime(
    swap_points() + enter_fade() + gganimate::enter_reset(),
    nframes = 20,
    fps = 10
  )

  expect_equal(transient_props(w), "opacity")
})

test_that("fade animates the paint opacity of a transient point", {
  w <- anime(swap_points() + enter_fade() + exit_fade(), nframes = 20, fps = 10)

  expect_equal(
    transient_props(w),
    c("fill-opacity", "opacity", "stroke-opacity")
  )
  # A point tweens both channels: pch 19 paints its disc from `fill` and its
  # outline from `stroke`, and ggplot2 sends `alpha` to both.
  fill <- numeric_track(last_entering(w), "fill-opacity")
  expect_lt(min(fill), 0.5)
  expect_equal(max(fill), 1)
  expect_equal(
    numeric_track(last_entering(w), "stroke-opacity"),
    fill
  )
})

test_that("grow animates the radius of a transient point", {
  w <- anime(
    swap_points() + enter_grow() + exit_shrink(),
    nframes = 20,
    fps = 10
  )

  # grow scales `stroke` alongside `size`, so the outline width tweens with the
  # radius.
  expect_equal(transient_props(w), c("opacity", "r", "stroke-width"))
  r <- numeric_track(last_entering(w), "r")
  expect_gt(max(r), 2 * min(r))
  expect_equal(r[[length(r)]], max(r))
  sw <- numeric_track(last_entering(w), "stroke-width")
  expect_lt(min(sw), max(sw) / 2)
  expect_equal(sw[[length(sw)]], max(sw))
})

test_that("drift animates a transient point in from an offset position", {
  w <- anime(
    swap_points() +
      gganimate::enter_drift(x_mod = -2) +
      gganimate::exit_drift(x_mod = 2),
    nframes = 20,
    fps = 10
  )

  expect_equal(transient_props(w), c("cx", "opacity"))
  # x_mod = -2 puts the arriving point two data units to the left of where it
  # lands, and both SVG axes rise with their range.
  cx <- numeric_track(last_entering(w), "cx")
  expect_lt(cx[[1]], cx[[length(cx)]])
})

test_that("fly animates a transient point in from a fixed location", {
  w <- anime(
    swap_points() +
      gganimate::enter_fly(x_loc = 0, y_loc = 0) +
      gganimate::exit_fly(x_loc = 6, y_loc = 6),
    nframes = 20,
    fps = 10
  )

  expect_equal(transient_props(w), c("cx", "cy", "opacity"))
  # The fly location is outside the panel's expanded range of 0.8 to 5.2, so the
  # first point to arrive starts left of and below the panel rectangle. The tween
  # is sampled at the frames the element exists in, so a point that arrives on the
  # animation's last frame is already part of the way in by its first keyframe.
  bounds <- panel_bounds(widget_doc(w))[[1]]
  arriving <- entering_segments(w)[[1]]
  expect_lt(numeric_track(arriving, "cx")[[1]], bounds[[1]])
  expect_lt(numeric_track(arriving, "cy")[[1]], bounds[[3]])
})

test_that("recolour animates the paint of a transient point", {
  # `fill` is `NA` for every pch that does not paint one, and setting it to a
  # colour makes tweenr coerce a logical column to numeric. Pass `NA` for the
  # channel the shape leaves alone -- pch 19 takes both from `colour`.
  w <- expect_no_warning(anime(
    swap_points() +
      gganimate::enter_recolour(colour = "white", fill = NA) +
      gganimate::exit_recolour(colour = "black", fill = NA),
    nframes = 20,
    fps = 10
  ))

  expect_equal(transient_props(w), c("fill", "opacity", "stroke"))
  fill <- track(last_entering(w), "fill")
  # The arriving point starts at a colour blended towards white and ends at its own.
  expect_false(identical(fill[[1]], fill[[length(fill)]]))
  expect_equal(fill[[length(fill)]], "#619CFF")
})

test_that("a manual transmuter animates whatever it rewrites", {
  w <- anime(
    swap_points() +
      gganimate::enter_manual(function(x) {
        x$y <- 0
        x
      }),
    nframes = 20,
    fps = 10
  )

  expect_equal(transient_props(w), c("cy", "opacity"))
  cy <- numeric_track(last_entering(w), "cy")
  expect_lt(cy[[1]], cy[[length(cy)]])
})

test_that("a bar that leaves and one that arrives stay separate elements", {
  w <- anime(swap_bars(), nframes = 20, fps = 10)
  segments <- w$x$config$segments
  transient <- vapply(
    segments,
    function(s) "opacity" %in% names(s$props),
    logical(1)
  )

  # a and b are drawn in every frame; c, d and c again on the wrap are not.
  expect_equal(sum(!transient), 2L)
  expect_length(transient_segments(w), 3L)

  doc <- widget_doc(w)
  authored_x <- function(segment) {
    id <- sub("^\\[data-animejs-id='(.*)'\\]$", "\\1", segment$selector)
    node <- xml2::xml_find_first(
      doc,
      sprintf(".//*[@data-animejs-id='%s']", id)
    )
    xml2::xml_attr(node, "x")
  }
  leaving <- segments[transient][[1]]
  arriving <- segments[transient][[2]]

  # The arriving bar is drawn in its own category's slot. Sharing a union slot
  # with the vacated one would slide it across the panel over one frame interval.
  expect_false(identical(authored_x(leaving), authored_x(arriving)))
  expect_equal(track(leaving, "opacity")[[1]], "1")
  expect_equal(track(arriving, "opacity")[[1]], "0")
})

test_that("bars that run through a state where another leaves keep their identity", {
  df <- data.frame(
    g = c("a", "b", "c", "a", "b"),
    state = c("s1", "s1", "s1", "s2", "s2"),
    v = c(10, 90, 40, 60, 20)
  )
  p <- ggplot(df, aes(g, v)) +
    geom_col() +
    transition_states(state, transition_length = 1, state_length = 1)

  w <- anime(p, nframes = 20, fps = 10)

  # a and b continue, c leaves and arrives again on the wrap. A bar chart has no
  # aesthetic that tells its bars apart, so the pairing across the boundary uses
  # `x`. Without it every bar counts as new and blinks out and back in.
  expect_length(w$x$config$segments, 4L)
  expect_length(transient_segments(w), 2L)
})

test_that("fade animates the fill opacity of a transient bar", {
  w <- anime(swap_bars() + enter_fade() + exit_fade(), nframes = 20, fps = 10)

  expect_equal(transient_props(w), c("fill-opacity", "opacity"))
  fill <- numeric_track(last_entering(w), "fill-opacity")
  expect_lt(min(fill), 0.5)
  expect_equal(max(fill), 1)
})

test_that("grow animates the height of a transient bar", {
  w <- anime(swap_bars() + enter_grow() + exit_shrink(), nframes = 20, fps = 10)

  expect_equal(transient_props(w), c("height", "opacity"))
  # A bar grows out of the axis: `y` stays at the zero baseline throughout, so
  # only its height animates.
  height <- numeric_track(last_entering(w), "height")
  expect_lt(min(height), 0.2 * max(height))
  expect_equal(height[[length(height)]], max(height))
})

test_that("fade animates the stroke opacity of a transient line", {
  p <- ggplot(swap_lines(), aes(x, y, group = grp, colour = grp)) +
    geom_line(linewidth = 1) +
    transition_states(state, transition_length = 1, state_length = 1) +
    enter_fade() +
    exit_fade()

  w <- anime(p, nframes = 20, fps = 10)

  expect_length(transient_segments(w), 1L)
  expect_equal(transient_props(w), c("opacity", "stroke-opacity"))
})

test_that("grow animates the vertices of a transient line", {
  p <- ggplot(swap_lines(), aes(x, y, group = grp, colour = grp)) +
    geom_line(linewidth = 1) +
    transition_states(state, transition_length = 1, state_length = 1) +
    enter_grow() +
    exit_shrink()

  w <- anime(p, nframes = 20, fps = 10)

  # A path has no size to animate, so grow collapses its vertices onto their
  # mean and the line grows out of a point.
  expect_equal(transient_props(w), c("opacity", "points"))
  expect_within_panels(w)
})

test_that("fade animates the fill opacity of a transient ribbon", {
  p <- ggplot(
    swap_lines(),
    aes(x, ymin = y - 0.4, ymax = y + 0.4, group = grp, fill = grp)
  ) +
    geom_ribbon() +
    transition_states(state, transition_length = 1, state_length = 1) +
    enter_fade() +
    exit_fade()

  w <- anime(p, nframes = 20, fps = 10)

  expect_length(transient_segments(w), 1L)
  expect_equal(transient_props(w), c("fill-opacity", "opacity"))
  fill <- numeric_track(transient_segments(w)[[1]], "fill-opacity")
  expect_lt(min(fill), 0.5)
  expect_equal(max(fill), 1)
})
