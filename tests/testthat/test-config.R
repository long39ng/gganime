test_that("element_id is stable and layer-qualified", {
  expect_equal(element_id(1, 1:3), c("L1e1", "L1e2", "L1e3"))
  expect_equal(element_id(2, 5), "L2e5")
})

test_that("build_timeline serialises to a stable config shape", {
  skip_if_not_installed("jsonlite")

  elements <- list(
    list(
      id = "L1e1",
      tracks = list(
        cx = c(0, 10, 20),
        cy = c(0, 5, 0),
        opacity = c(1, 1, 0)
      )
    ),
    list(id = "L1e2", tracks = list(r = c(2, 3, 4)))
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
      w$x$config,
      auto_unbox = TRUE,
      pretty = TRUE,
      digits = NA
    ))
  )
})

test_that("build_timeline skips elements with no tracks", {
  elements <- list(list(id = "L1e1", tracks = list()))
  tl <- build_timeline(
    elements,
    nframes = 2,
    fps = 10,
    loop = FALSE,
    controls = FALSE
  )
  expect_length(tl$segments, 0)
})
