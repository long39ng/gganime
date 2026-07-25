library(ggplot2)

# A faceted export, in the shape gridSVG produces: one viewport group per panel
# (named after the gtable cell, `panel-<col>-<row>`) wrapping one gTree group
# named after the PANEL. A 2x2 facet_grid is drawn column-major, so the document
# order of the panel groups is PANEL 1, 3, 2, 4.
facet_grid_doc <- function() {
  doc <- xml2::read_xml(
    '<svg xmlns="http://www.w3.org/2000/svg">
       <g id="layout::panel-1-1.10-7-10-7::GRID.VP.13.1">
         <g id="panel-1.gTree.219.1">
           <polyline id="panel.grid.major.y..polyline.1.1"/>
           <use id="geom_point.points.194.1.1"/>
           <use id="geom_point.points.194.1.2"/>
         </g>
       </g>
       <g id="layout::panel-1-2.12-7-12-7::GRID.VP.16.1">
         <g id="panel-3.gTree.264.1">
           <use id="geom_point.points.239.1.1"/>
         </g>
       </g>
       <g id="layout::panel-2-1.10-9-10-9::GRID.VP.14.1">
         <g id="panel-2.gTree.234.1">
           <use id="geom_point.points.209.1.1"/>
         </g>
       </g>
       <g id="layout::panel-2-2.12-9-12-9::GRID.VP.17.1">
         <g id="panel-4.gTree.279.1"/>
       </g>
     </svg>'
  )
  xml2::xml_ns_strip(doc)
  doc
}

# The exported rectangles: left column at x 10, right at x 110; lower row at
# y 10, upper row at y 110 (gridSVG space is y-up).
facet_grid_export <- function() {
  rect <- function(x, y) list(x = x, y = y, width = 80, height = 80)
  list(
    coords = list(
      "layout::panel-1-1.10-7-10-7::GRID.VP.13.1" = rect(10, 110),
      "layout::panel-1-2.12-7-12-7::GRID.VP.16.1" = rect(10, 10),
      "layout::panel-2-1.10-9-10-9::GRID.VP.14.1" = rect(110, 110),
      "layout::panel-2-2.12-9-12-9::GRID.VP.17.1" = rect(110, 10)
    )
  )
}

unit_ranges <- function(panels) {
  out <- rep(list(list(x_range = c(0, 1), y_range = c(0, 1))), length(panels))
  names(out) <- panels
  out
}

test_that("panel_group_nodes keys the gTree groups by PANEL", {
  groups <- panel_group_nodes(facet_grid_doc())

  # Keyed by PANEL, not by document position, and the gtable viewport groups
  # ("panel-1-1...") are not mistaken for panel groups.
  expect_equal(names(groups), c("1", "3", "2", "4"))
  expect_equal(
    xml2::xml_attr(groups[["3"]], "id"),
    "panel-3.gTree.264.1"
  )
})

test_that("panel_group_nodes does not confuse PANEL 1 with PANEL 10", {
  doc <- xml2::read_xml(
    '<svg xmlns="http://www.w3.org/2000/svg">
       <g id="panel-10.gTree.9.1"/>
     </svg>'
  )
  xml2::xml_ns_strip(doc)
  expect_equal(names(panel_group_nodes(doc)), "10")
  expect_length(
    xml2::xml_find_all(doc, sprintf(".//g[%s]", in_panel_group("1"))),
    0L
  )
})

test_that("each panel gets the affine of its own exported rectangle", {
  affines <- panel_affines(
    facet_grid_export(),
    facet_grid_doc(),
    unit_ranges(c("1", "2", "3", "4")),
    res = 96
  )

  # PANEL 2 is the second column of the first row and PANEL 3 the first column
  # of the second row. Reading the rectangles in document order would swap them.
  expect_equal(affines[["1"]]$to_svg_x(0), 10)
  expect_equal(affines[["1"]]$to_svg_y(0), 110)
  expect_equal(affines[["2"]]$to_svg_x(0), 110)
  expect_equal(affines[["2"]]$to_svg_y(0), 110)
  expect_equal(affines[["3"]]$to_svg_x(0), 10)
  expect_equal(affines[["3"]]$to_svg_y(0), 10)
  expect_equal(affines[["4"]]$to_svg_x(1), 190)
  expect_equal(affines[["4"]]$to_svg_y(1), 90)
})

test_that("free scales give each panel its own mapping", {
  ranges <- list(
    "1" = list(x_range = c(0, 10), y_range = c(0, 1)),
    "2" = list(x_range = c(0, 100), y_range = c(0, 1))
  )
  affines <- panel_affines(
    facet_grid_export(),
    facet_grid_doc(),
    ranges,
    res = 96
  )

  # The same data value lands at a different offset in each panel.
  expect_equal(affines[["1"]]$to_svg_x(10), 90)
  expect_equal(affines[["2"]]$to_svg_x(10), 118)
})

test_that("panel_affines reports a panel that is missing from the export", {
  expect_snapshot(
    panel_affines(
      facet_grid_export(),
      facet_grid_doc(),
      unit_ranges(c("1", "9")),
      res = 96
    ),
    error = TRUE
  )
})

test_that("panel_affines reports a panel with no exported rectangle", {
  expect_snapshot(
    panel_affines(
      list(coords = list()),
      facet_grid_doc(),
      unit_ranges("1"),
      res = 96
    ),
    error = TRUE
  )
})

test_that("panel_data_nodes selects within one panel only", {
  doc <- facet_grid_doc()
  expect_length(panel_data_nodes(doc, "use", "1"), 2L)
  expect_length(panel_data_nodes(doc, "use", "3"), 1L)
  # Grid lines are excluded even though they are inside the panel group.
  expect_length(panel_data_nodes(doc, "polyline", "1"), 0L)
})

test_that("ordered_data_nodes interleaves panels back into union order", {
  # Union order is first appearance then PANEL, so a panel's elements need not
  # be contiguous: element 2 is in PANEL 3, elements 1 and 3 in PANEL 1.
  nodes <- point_nodes(facet_grid_doc(), panels = c("1", "3", "1"))

  expect_equal(
    vapply(nodes, function(n) xml2::xml_attr(n, "id"), character(1)),
    c(
      "geom_point.points.194.1.1",
      "geom_point.points.239.1.1",
      "geom_point.points.194.1.2"
    )
  )
})

test_that("a per-panel count mismatch names the panel", {
  expect_snapshot(
    point_nodes(facet_grid_doc(), panels = c("1", "1", "1")),
    error = TRUE
  )
})

test_that("the union keeps elements of different panels apart", {
  # gganimate tweens each panel separately, so `.id` restarts per panel.
  frames <- list(
    data.frame(
      PANEL = factor(c(1, 1, 2)),
      .id = c(1, 2, 1),
      x = c(1, 2, 3),
      y = c(1, 2, 3)
    ),
    data.frame(
      PANEL = factor(c(1, 1, 2)),
      .id = c(1, 2, 1),
      x = c(4, 5, 6),
      y = c(4, 5, 6)
    )
  )
  union <- union_elements(frames)

  expect_equal(ncol(union$presence), 3L)
  expect_equal(union$panels, c("1", "1", "2"))
  expect_equal(union$keys$.id, c(1, 2, 1))
  # The reference row of each element comes from its own panel.
  expect_equal(union$union_data$x, c(1, 2, 3))
})

test_that("anime() renders every panel of a facet_wrap", {
  skip_if_not_installed("gganimate")
  skip_if_not_installed("gridSVG")

  df <- data.frame(
    facet = rep(c("a", "b"), each = 4),
    state = rep(c("s1", "s2"), 4),
    x = c(1, 2, 3, 4, 1, 2, 3, 4),
    y = c(1, 4, 2, 3, 4, 1, 3, 2)
  )
  p <- ggplot(df, aes(x, y)) +
    geom_point(size = 4) +
    facet_wrap(~facet) +
    transition_states(state, transition_length = 1, state_length = 1)

  w <- anime(p, nframes = 6, fps = 10)
  expect_s3_class(w, "gganime")

  # Two points per state per panel, animated in their own panel group.
  expect_equal(panel_of_elements(w), c("1", "1", "2", "2"))
  expect_true(all(elements_within_panels(w)))
})

test_that("a facet_grid element animates inside its own panel", {
  skip_if_not_installed("gganimate")
  skip_if_not_installed("gridSVG")

  df <- data.frame(
    row = rep(c("r1", "r2"), each = 8),
    col = rep(rep(c("c1", "c2"), each = 4), 2),
    state = rep(c("s1", "s2"), 8),
    x = rep(c(1, 2, 3, 4), 4),
    y = rep(c(1, 4, 2, 3), 4)
  )
  p <- ggplot(df, aes(x, y)) +
    geom_point(size = 4) +
    facet_grid(row ~ col) +
    transition_states(state, transition_length = 1, state_length = 1)

  w <- anime(p, nframes = 6, fps = 10)

  # The panels are drawn column-major (PANEL 1, 3, 2, 4 for a 2x2 grid) while
  # PANEL numbers row-major. Reading that document order as PANEL order would
  # put panels 2 and 3 in each other's place, which the containment check below
  # then catches.
  expect_equal(panel_of_elements(w), rep(c("1", "3", "2", "4"), each = 2))
  expect_true(all(elements_within_panels(w)))
})

test_that("free scales animate each panel on its own range", {
  skip_if_not_installed("gganimate")
  skip_if_not_installed("gridSVG")

  df <- data.frame(
    facet = rep(c("a", "b"), each = 4),
    state = rep(c("s1", "s2"), 4),
    # Panel b spans ten times the x range of panel a.
    x = c(1, 2, 3, 4, 10, 20, 30, 40),
    y = c(1, 4, 2, 3, 4, 1, 3, 2)
  )
  p <- ggplot(df, aes(x, y)) +
    geom_point(size = 4) +
    facet_wrap(~facet, scales = "free") +
    transition_states(state, transition_length = 1, state_length = 1)

  w <- anime(p, nframes = 6, fps = 10)
  expect_true(all(elements_within_panels(w)))
})

test_that("free panel space animates each panel on its own rectangle", {
  skip_if_not_installed("gganimate")
  skip_if_not_installed("gridSVG")

  # Panel b holds twice as many categories as panel a, so space = "free" gives
  # the two panels different widths.
  df <- data.frame(
    facet = c(rep("a", 4), rep("b", 8)),
    cat = c("p", "q", "p", "q", letters[1:4], letters[1:4]),
    state = c(rep(c("s1", "s2"), each = 2), rep(c("s1", "s2"), each = 4)),
    v = c(1, 2, 3, 4, 1, 2, 3, 4, 4, 3, 2, 1)
  )
  p <- ggplot(df, aes(cat, v)) +
    geom_col() +
    facet_grid(~facet, scales = "free_x", space = "free_x") +
    transition_states(state, transition_length = 1, state_length = 1)

  w <- anime(p, nframes = 6, fps = 10)
  bounds <- panel_bounds(widget_doc(w))
  width <- function(panel) bounds[[panel]][2] - bounds[[panel]][1]

  expect_gt(width("2"), width("1"))
  expect_true(all(elements_within_panels(w)))
})

test_that("a facet_wrap with an empty grid cell renders", {
  skip_if_not_installed("gganimate")
  skip_if_not_installed("gridSVG")

  # Five facets in a 2x3 grid, so the last cell is empty.
  df <- data.frame(
    facet = rep(letters[1:5], each = 4),
    state = rep(c("s1", "s2"), 10),
    x = rep(c(1, 2), 10),
    y = rep(c(1, 4, 2, 3), 5)
  )
  p <- ggplot(df, aes(x, y)) +
    geom_point(size = 4) +
    facet_wrap(~facet) +
    transition_states(state, transition_length = 1, state_length = 1)

  w <- anime(p, nframes = 6, fps = 10)
  expect_setequal(panel_of_elements(w), as.character(1:5))
  expect_true(all(elements_within_panels(w)))
})

test_that("faceted bars, lines and ribbons stay in their panels", {
  skip_if_not_installed("gganimate")
  skip_if_not_installed("gridSVG")

  bars <- data.frame(
    facet = rep(c("a", "b"), each = 6),
    state = rep(c("s1", "s2", "s3"), 4),
    cat = rep(c("p", "q"), 6),
    v = c(1, 2, 3, 4, 5, 6, 6, 5, 4, 3, 2, 1)
  )
  w <- anime(
    ggplot(bars, aes(cat, v)) +
      geom_col() +
      facet_wrap(~facet) +
      transition_states(state, transition_length = 1, state_length = 1),
    nframes = 6,
    fps = 10
  )
  expect_balanced_panels(w, 2L)
  expect_true(all(elements_within_panels(w)))

  paths <- data.frame(
    facet = rep(c("a", "b"), each = 6),
    state = rep(c("s1", "s2"), each = 3, times = 2),
    x = rep(1:3, 4),
    y = c(1, 2, 3, 3, 2, 1, 2, 1, 3, 1, 3, 2)
  )
  w <- anime(
    ggplot(paths, aes(x, y)) +
      geom_line() +
      facet_wrap(~facet) +
      transition_states(state, transition_length = 1, state_length = 0),
    nframes = 6,
    fps = 10
  )
  expect_balanced_panels(w, 2L)
  expect_true(all(elements_within_panels(w)))

  w <- anime(
    ggplot(paths, aes(x, ymin = y - 0.5, ymax = y + 0.5)) +
      geom_ribbon() +
      facet_grid(~facet) +
      transition_states(state, transition_length = 1, state_length = 0),
    nframes = 6,
    fps = 10
  )
  expect_balanced_panels(w, 2L)
  expect_true(all(elements_within_panels(w)))
})

test_that("a faceted shadow_mark keeps its marks in their own panels", {
  skip_if_not_installed("gganimate")
  skip_if_not_installed("gridSVG")

  df <- data.frame(
    facet = rep(c("a", "b"), each = 6),
    state = rep(c("s1", "s2", "s3"), 4),
    x = c(1, 2, 3, 4, 5, 6, 6, 5, 4, 3, 2, 1),
    y = c(1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6)
  )
  p <- ggplot(df, aes(x, y)) +
    geom_point(size = 4) +
    facet_wrap(~facet) +
    transition_states(state, transition_length = 1, state_length = 1) +
    shadow_mark(past = TRUE)

  w <- anime(p, nframes = 6, fps = 10)
  panels <- panel_of_elements(w)

  # Shadow marks come first in each panel, so both panels appear twice over.
  expect_setequal(panels, c("1", "2"))
  expect_true(all(elements_within_panels(w)))
})

test_that("a faceted plot keeps its per-frame title", {
  skip_if_not_installed("gganimate")
  skip_if_not_installed("gridSVG")

  df <- data.frame(
    facet = rep(c("a", "b"), each = 4),
    state = rep(c("s1", "s2"), 4),
    x = c(1, 2, 3, 4, 1, 2, 3, 4),
    y = c(1, 4, 2, 3, 4, 1, 3, 2)
  )
  p <- ggplot(df, aes(x, y)) +
    geom_point() +
    facet_wrap(~facet) +
    labs(title = "{closest_state}") +
    transition_states(state, transition_length = 1, state_length = 1)

  w <- anime(p, nframes = 6, fps = 10)
  labels <- Filter(
    function(s) "label" %in% names(s$props),
    w$x$config$segments
  )
  expect_length(labels, 1L)
  expect_true(any(grepl("s1", unlist(labels[[1]]$props$label))))
})
