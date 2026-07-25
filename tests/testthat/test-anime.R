skip_if_not_installed("gganimate")
skip_if_not_installed("gridSVG")

library(ggplot2)

# Five points, all present in both states.
all_present_plot <- function() {
  df <- data.frame(
    id = rep(1:5, 2),
    state = rep(c("a", "b"), each = 5),
    x = c(1, 2, 3, 4, 5, 5, 4, 3, 2, 1),
    y = c(2, 4, 1, 5, 3, 5, 1, 4, 2, 3)
  )
  ggplot(df, aes(x, y, group = id)) +
    geom_point(size = 4) +
    transition_states(state, transition_length = 1, state_length = 1)
}

# id 5 exits, id 6 enters, with fade + grow/shrink.
enter_exit_plot <- function() {
  df <- data.frame(
    id = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 6),
    state = rep(c("a", "b"), each = 5),
    x = c(1, 2, 3, 4, 5, 5, 4, 3, 2, 1),
    y = c(2, 4, 1, 5, 3, 5, 1, 4, 2, 3)
  )
  ggplot(df, aes(x, y, group = id, colour = factor(id))) +
    geom_point(size = 4) +
    transition_states(state, transition_length = 1, state_length = 1) +
    enter_fade() +
    enter_grow() +
    exit_fade() +
    exit_shrink()
}

test_that("anime() produces a gganime widget with one segment per element", {
  w <- anime(all_present_plot(), nframes = 10, fps = 10)

  expect_s3_class(w, "gganime")
  expect_s3_class(w, "htmlwidget")

  segs <- w$x$config$segments
  ids <- vapply(segs, function(s) s$selector, character(1))
  expect_equal(ids, sprintf("[data-animejs-id='L1e%d']", 1:5))

  # every point is present throughout, so none carries a presence channel
  has_opacity <- vapply(
    segs,
    function(s) "opacity" %in% names(s$props),
    logical(1)
  )
  expect_false(any(has_opacity))
})

test_that("anime() encodes absence as an opacity channel on entering/exiting points", {
  w <- anime(enter_exit_plot(), nframes = 20, fps = 10)
  segs <- w$x$config$segments

  # circles are inlined and carry a data-animejs-id
  expect_match(w$x$svg, "circle[^>]*data-animejs-id", perl = TRUE)

  has_opacity <- vapply(
    segs,
    function(s) "opacity" %in% names(s$props),
    logical(1)
  )
  # id 5 exits; two fresh ids enter across the state loop
  expect_equal(sum(has_opacity), 3)

  # an entering point animates radius (grow) while holding position
  entering <- segs[has_opacity][[2]]
  expect_true("r" %in% names(entering$props))
  expect_false("cx" %in% names(entering$props))
})

test_that("the reference SVG is authored at each track's first keyframe", {
  # Anime.js tweens from the authored attribute into keyframe 1, so any
  # disagreement between the two shows for the first frame interval.
  w <- anime(enter_exit_plot(), nframes = 20, fps = 10)
  doc <- widget_doc(w)

  for (segment in w$x$config$segments) {
    id <- sub("^\\[data-animejs-id='(.*)'\\]$", "\\1", segment$selector)
    node <- xml2::xml_find_first(
      doc,
      sprintf(".//*[@data-animejs-id='%s']", id)
    )
    for (attr in names(segment$props)) {
      first <- segment$props[[attr]][[1]]$to
      authored <- xml2::xml_attr(node, attr)
      if (is.numeric(first)) {
        authored <- as.numeric(authored)
      }
      expect_equal(authored, first, info = paste(id, attr))
    }
  }
})

test_that("an element absent from the first frame is authored invisible", {
  w <- anime(enter_exit_plot(), nframes = 20, fps = 10)
  doc <- widget_doc(w)

  starts_hidden <- Filter(
    function(s) identical(s$props$opacity[[1]]$to, 0),
    w$x$config$segments
  )
  expect_gt(length(starts_hidden), 0)

  for (segment in starts_hidden) {
    id <- sub("^\\[data-animejs-id='(.*)'\\]$", "\\1", segment$selector)
    node <- xml2::xml_find_first(
      doc,
      sprintf(".//*[@data-animejs-id='%s']", id)
    )
    expect_equal(xml2::xml_attr(node, "opacity"), "0", info = id)
  }
})

test_that("precision controls coordinate rounding", {
  w <- anime(all_present_plot(), nframes = 6, fps = 10, precision = 0)
  cx <- unlist(w$x$config$segments[[1]]$props$cx)
  expect_equal(cx, round(cx))
})
