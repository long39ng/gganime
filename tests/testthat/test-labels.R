test_that("label_string collapses characters, expressions, and absence", {
  expect_equal(label_string("hello"), "hello")
  expect_identical(label_string(NULL), NA_character_)
  expect_identical(label_string(character(0)), NA_character_)
  expect_equal(label_string(as.expression("Year: 1969")), "Year: 1969")
  expect_equal(label_string(c("line one", "line two")), "line one\nline two")
})

test_that("label_is_static and dynamic_label_names separate varying labels", {
  labels <- list(
    title = list(values = c("a", "b", "c"), dynamic = TRUE),
    caption = list(values = c("x", "x", "x"), dynamic = FALSE)
  )
  expect_true(label_is_static(c("x", "x", "x")))
  expect_false(label_is_static(c("a", "b", "a")))
  expect_equal(dynamic_label_names(labels), "title")
})

test_that("label_lines splits each frame's label into a fixed line count", {
  expect_equal(label_lines(c("a", "b")), matrix(c("a", "b"), nrow = 1))
  expect_equal(
    label_lines(c("one\nx", "one\ny")),
    matrix(c("one", "x", "one", "y"), nrow = 2)
  )
  expect_null(label_lines(c("a", "a\nb")))
})

test_that("label_id namespaces the element and its lines", {
  expect_equal(label_id("title"), "label_title")
  expect_equal(label_id("subtitle"), "label_subtitle")
  expect_equal(label_id("title", 2), "label_title_2")
})

test_that("annotate_labels freezes a label whose lines and tspans disagree", {
  doc <- xml2::read_xml(
    "<svg><g id='plot.title.1'><text><tspan>Year 1</tspan></text></g></svg>"
  )
  labels <- list(
    title = list(values = c("Year 1\nA", "Year 2\nB"), dynamic = TRUE)
  )

  expect_snapshot(elements <- annotate_labels(doc, labels))
  expect_length(elements, 0L)
})

test_that("build_timeline emits an anime_text segment for a label element", {
  skip_if_not_installed("jsonlite")

  elements <- list(
    list(id = "label_title", text = c("Year: 1967", "Year: 1968", "Year: 1969"))
  )
  tl <- build_timeline(
    elements,
    nframes = 3,
    fps = 10,
    loop = TRUE,
    controls = TRUE
  )
  w <- animejs::anime_render(tl, svg = "<svg></svg>")

  expect_snapshot(
    cat(jsonlite::toJSON(
      w$x$config$segments,
      auto_unbox = TRUE,
      pretty = TRUE,
      digits = NA
    ))
  )
})

# --- integration -----------------------------------------------------------

library(ggplot2)

# The selectors of the timeline segments that carry a discrete text swap.
text_segment_selectors <- function(w) {
  segs <- w$x$config$segments
  is_text <- vapply(
    segs,
    function(s) {
      any(vapply(
        s$props,
        function(p) is.list(p) && identical(p$type, "text"),
        logical(1)
      ))
    },
    logical(1)
  )
  vapply(segs[is_text], `[[`, character(1), "selector")
}

time_plot <- function(title = "Year: {frame_time}", ...) {
  df <- data.frame(
    t = rep(1:3, each = 2),
    x = c(1, 2, 2, 3, 3, 4),
    y = c(1, 2, 3, 2, 1, 3),
    g = rep(1:2, 3)
  )
  ggplot(df, aes(x, y, group = g)) +
    geom_line() +
    labs(title = title, ...) +
    transition_time(t)
}

test_that("precompute_labels flags varying labels dynamic and constant labels static", {
  built <- ggplot2::ggplot_build(`$<-`(
    time_plot(caption = "fixed"),
    "nframes",
    5
  ))
  labels <- precompute_labels(built)

  expect_true(labels$title$dynamic)
  expect_false(labels$caption$dynamic)
  expect_length(labels$title$values, built$scene$nframes)
})

test_that("anime() animates varying labels and leaves constant ones static", {
  w <- anime(
    time_plot(subtitle = "Frame {frame}", caption = "fixed"),
    nframes = 5
  )

  expect_setequal(
    text_segment_selectors(w),
    c("[data-animejs-id='label_title']", "[data-animejs-id='label_subtitle']")
  )
  # the tspan is annotated in place, keeping its positioning attributes
  expect_match(
    w$x$svg,
    "<tspan[^>]*data-animejs-id=\"label_title\"",
    perl = TRUE
  )
})

test_that("anime() drives a multi-line label one line at a time", {
  w <- anime(time_plot(title = "Year {frame_time}\nFrame {frame}"), nframes = 4)

  expect_setequal(
    text_segment_selectors(w),
    c("[data-animejs-id='label_title_1']", "[data-animejs-id='label_title_2']")
  )
  # each line's tspan carries its own id, and its first-frame text
  expect_match(
    w$x$svg,
    "<tspan[^>]*data-animejs-id=\"label_title_1\"[^>]*>Year 1<",
    perl = TRUE
  )
  expect_match(
    w$x$svg,
    "<tspan[^>]*data-animejs-id=\"label_title_2\"[^>]*>Frame 1<",
    perl = TRUE
  )
})

test_that("anime() leaves a constant line of a multi-line label alone", {
  w <- anime(time_plot(title = "Line one\nYear {frame_time}"), nframes = 4)

  expect_equal(
    text_segment_selectors(w),
    "[data-animejs-id='label_title_2']"
  )
  expect_no_match(w$x$svg, "label_title_1", fixed = TRUE)
})

test_that("anime() freezes a label whose line count varies", {
  expect_snapshot(
    invisible(anime(
      time_plot(title = "Year {frame_time}{ifelse(frame > 2, '\nmore', '')}"),
      nframes = 4
    ))
  )
})
