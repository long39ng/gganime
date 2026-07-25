library(ggplot2)

# A narrow x range against a wide y one, so mapping a column onto the wrong axis
# lands far outside the panel rather than merely off by a little. x rises within
# each state, so the two states of a path are different shapes even after
# GeomPath sorts them -- otherwise the `points` track is constant and dropped.
coord_points <- function() {
  data.frame(
    state = rep(c("a", "b"), each = 4),
    x = rep(1:4, 2),
    y = c(10, 90, 40, 70, 70, 40, 90, 10)
  )
}

# Export a static plot, returning the document plus the drawn data. ggplot2
# applies the coord itself when it draws, so the exported attributes are an
# independent check on the affine gganime builds for the same export.
export_static <- function(plot) {
  built <- ggplot2::ggplot_build(plot)
  res <- 96
  export <- export_scene_svg(
    ggplot2::ggplot_gtable(built),
    build_panel_ranges(built),
    res = res,
    width = 640 / res,
    height = 480 / res
  )
  list(export = export, data = built$data[[1]])
}

# gridSVG rounds its output, so agreement to ~0.05 user units is exact.
expect_same_position <- function(drawn, mapped, label = NULL) {
  testthat::expect_lt(max(abs(drawn - mapped)), 0.05, label = label)
}

node_attr <- function(nodes, attr) {
  as.numeric(vapply(nodes, xml2::xml_attr, character(1), attr))
}

test_that("affine_xy swaps the axes only for a flipped coord", {
  expect_equal(
    affine_xy(identity_affine(), c(1, 2), c(10, 20)),
    cbind(c(1, 2), c(10, 20))
  )
  expect_equal(
    affine_xy(identity_affine(flipped = TRUE), c(1, 2), c(10, 20)),
    cbind(c(10, 20), c(1, 2))
  )
})

test_that("build_panel_ranges flags a flipped coord", {
  d <- coord_points()
  plain <- ggplot2::ggplot_build(ggplot(d, aes(x, y)) + geom_point())
  flipped <- ggplot2::ggplot_build(
    ggplot(d, aes(x, y)) + geom_point() + coord_flip()
  )

  expect_false(build_panel_ranges(plain)[["1"]]$flipped)
  expect_true(build_panel_ranges(flipped)[["1"]]$flipped)
  # `x.range`/`y.range` are the screen axes, so the flip already swapped them.
  expect_equal(
    build_panel_ranges(flipped)[["1"]]$x_range,
    build_panel_ranges(plain)[["1"]]$y_range
  )
})

test_that("points land where each affine coord draws them", {
  coords <- list(
    cartesian = coord_cartesian(),
    fixed = coord_fixed(ratio = 0.05),
    equal = coord_equal(ratio = 0.05),
    flip = coord_flip(),
    flip_limits = coord_flip(xlim = c(1.5, 3.5))
  )

  for (name in names(coords)) {
    static <- export_static(
      ggplot(coord_points(), aes(x, y)) + geom_point(size = 3) + coords[[name]]
    )
    nodes <- point_nodes(static$export$doc, rep("1", nrow(static$data)))
    expect_same_position(
      cbind(node_attr(nodes, "x"), node_attr(nodes, "y")),
      affine_xy(static$export$panels[["1"]], static$data$x, static$data$y),
      label = name
    )
  }
})

test_that("a flipped bar keeps its origin corner and positive extents", {
  static <- export_static(
    ggplot(data.frame(g = c("a", "b", "c"), v = c(10, 90, 40)), aes(g, v)) +
      geom_col() +
      coord_flip()
  )
  nodes <- rect_nodes(static$export$doc, rep("1", nrow(static$data)))
  affine <- static$export$panels[["1"]]
  low <- affine_xy(affine, static$data$xmin, static$data$ymin)
  high <- affine_xy(affine, static$data$xmax, static$data$ymax)

  expect_same_position(node_attr(nodes, "x"), low[, 1])
  expect_same_position(node_attr(nodes, "y"), low[, 2])
  expect_same_position(node_attr(nodes, "width"), high[, 1] - low[, 1])
  expect_same_position(node_attr(nodes, "height"), high[, 2] - low[, 2])
})

test_that("an affine coord animates every geom inside its panel", {
  d <- coord_points()
  bars <- data.frame(
    state = rep(c("a", "b"), each = 3),
    g = rep(c("a", "b", "c"), 2),
    v = c(10, 90, 40, 70, 20, 60)
  )
  band <- data.frame(
    state = rep(c("a", "b"), each = 4),
    x = rep(1:4, 2),
    lo = c(10, 20, 15, 25, 30, 20, 35, 25),
    hi = c(60, 80, 70, 90, 50, 70, 60, 80)
  )
  plots <- list(
    point = ggplot(d, aes(x, y)) +
      geom_point(size = 4) +
      coord_flip() +
      transition_states(state, transition_length = 1, state_length = 1),
    line = ggplot(d, aes(x, y)) +
      geom_line() +
      coord_flip() +
      transition_states(state, transition_length = 1, state_length = 0),
    col = ggplot(bars, aes(g, v)) +
      geom_col() +
      coord_flip() +
      transition_states(state, transition_length = 1, state_length = 1),
    ribbon = ggplot(band, aes(x, ymin = lo, ymax = hi)) +
      geom_ribbon() +
      coord_flip() +
      transition_states(state, transition_length = 1, state_length = 0),
    fixed = ggplot(d, aes(x, y)) +
      geom_point(size = 4) +
      coord_fixed(ratio = 0.05) +
      transition_states(state, transition_length = 1, state_length = 1)
  )

  for (name in names(plots)) {
    expect_within_panels(anime(plots[[name]], nframes = 8))
  }
})

test_that("a faceted flipped plot keeps each panel's elements in place", {
  d <- coord_points()
  d$facet <- rep(c("p", "q"), 4)
  w <- anime(
    ggplot(d, aes(x, y)) +
      geom_point(size = 4) +
      facet_wrap(~facet) +
      coord_flip() +
      transition_states(state, transition_length = 1, state_length = 1),
    nframes = 8
  )

  expect_balanced_panels(w, 2L)
  expect_within_panels(w)
})
