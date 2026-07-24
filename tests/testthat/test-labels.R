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

test_that("label_id namespaces the element", {
  expect_equal(label_id("title"), "label_title")
  expect_equal(label_id("subtitle"), "label_subtitle")
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

skip_if_not_installed("gganimate")
skip_if_not_installed("gridSVG")

library(ggplot2)

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

  segs <- w$x$config$segments
  text_selectors <- vapply(
    segs,
    function(s) {
      is_text <- vapply(
        s$props,
        function(p) is.list(p) && identical(p$type, "text"),
        logical(1)
      )
      if (any(is_text)) s$selector else NA_character_
    },
    character(1)
  )
  text_selectors <- text_selectors[!is.na(text_selectors)]

  expect_setequal(
    text_selectors,
    c("[data-animejs-id='label_title']", "[data-animejs-id='label_subtitle']")
  )
  # the tspan is annotated in place, keeping its positioning attributes
  expect_match(
    w$x$svg,
    "<tspan[^>]*data-animejs-id=\"label_title\"",
    perl = TRUE
  )
})

test_that("anime() freezes a multi-line label with a warning", {
  expect_snapshot(
    invisible(anime(
      time_plot(title = "Line one\nYear {frame_time}"),
      nframes = 4
    ))
  )
})
