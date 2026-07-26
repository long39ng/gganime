library(ggplot2)

# Multi-layer plots. Node selection is scoped to one layer's exported group, so
# two layers of the same geom family no longer share a selector and a ribbon's
# `stroke: none` outline polyline no longer reaches the path adapter.

# A panel gTree in ggplot2's shape: coord background, facet_bg, one grob per
# layer, facet_fg, coord foreground.
panel_children <- function(n_layers, ontop = FALSE) {
  layers <- lapply(seq_len(n_layers), function(i) {
    grid::nullGrob(name = paste0("GRID.layer.", i))
  })
  grill <- grid::nullGrob(name = "grill.gTree.20")
  border <- grid::nullGrob(name = "panel.border..zeroGrob.8")
  bg <- grid::nullGrob(name = "NULL")
  parts <- if (ontop) {
    c(list(bg), layers, list(bg, grill, border))
  } else {
    c(list(grill, bg), layers, list(bg, border))
  }
  do.call(grid::gList, parts)
}

# name_layer_grobs() reads only `$layout$name` and `$grobs`, so a plain list
# stands in for a gtable and keeps this a unit test.
panel_gtable <- function(n_layers, ontop = FALSE) {
  panel <- grid::gTree(
    name = "panel-1",
    children = panel_children(n_layers, ontop)
  )
  list(
    layout = data.frame(name = "panel", stringsAsFactors = FALSE),
    grobs = list(panel)
  )
}

child_names <- function(gtable) {
  vapply(gtable$grobs[[1]]$children, function(g) g$name, character(1))
}

test_that("name_layer_grobs renames the layer grobs and nothing else", {
  gt <- name_layer_grobs(panel_gtable(2L), 2L)
  expect_equal(
    unname(child_names(gt)),
    c(
      "grill.gTree.20",
      "NULL",
      "gganime.L1",
      "gganime.L2",
      "NULL",
      "panel.border..zeroGrob.8"
    )
  )
  # setChildren() keeps childrenOrder in step, which is what grid draws by.
  expect_equal(unname(gt$grobs[[1]]$childrenOrder), unname(child_names(gt)))
})

test_that("name_layer_grobs follows the panel.ontop child order", {
  # panel.ontop = TRUE draws the grill second to last, so layer 1 is child 2.
  gt <- name_layer_grobs(panel_gtable(2L, ontop = TRUE), 2L)
  expect_equal(
    unname(child_names(gt)),
    c(
      "NULL",
      "gganime.L1",
      "gganime.L2",
      "NULL",
      "grill.gTree.20",
      "panel.border..zeroGrob.8"
    )
  )
})

test_that("an unexpected panel child count aborts", {
  expect_snapshot(name_layer_grobs(panel_gtable(2L), 3L), error = TRUE)
})

test_that("in_layer_group requires the counter separator", {
  # gridSVG appends its own counter, so the predicate ends in "." -- otherwise
  # layer 1 would also match layer 10.
  expect_equal(
    in_layer_group(1L),
    "ancestor::g[starts-with(@id, 'gganime.L1.')]"
  )
})

# --- integration -----------------------------------------------------------

two_state <- function(df) {
  transition_states(state, transition_length = 1, state_length = 1)
}

layer_df <- function() {
  data.frame(
    state = rep(c("s1", "s2"), each = 4),
    x = rep(1:4, 2),
    y = c(1, 4, 2, 3, 4, 1, 3, 2)
  )
}

# Which layer each animated node belongs to, in document order.
layer_of_elements <- function(widget) {
  doc <- widget_doc(widget)
  nodes <- xml2::xml_find_all(doc, ".//*[@data-animejs-id]")
  vapply(
    nodes,
    function(node) {
      ids <- xml2::xml_attr(xml2::xml_find_all(node, "ancestor::g[@id]"), "id")
      hit <- grep("^gganime\\.L[0-9]+\\.", ids, value = TRUE)
      if (length(hit) == 0L) {
        NA_character_
      } else {
        sub("^gganime\\.(L[0-9]+)\\..*$", "\\1", hit[[1]])
      }
    },
    character(1)
  )
}

# The SVG tag of every animated node, in document order.
tag_of_elements <- function(widget) {
  nodes <- xml2::xml_find_all(widget_doc(widget), ".//*[@data-animejs-id]")
  vapply(nodes, xml2::xml_name, character(1))
}

test_that("an area and a line layer animate as separate layers", {
  # The regression this whole change is for: GeomRibbon draws a `stroke: none`
  # outline polyline beside its polygon, which the panel-scoped path selector
  # matched alongside the line layer's own polyline, aborting on the count.
  df <- layer_df()
  p <- ggplot(df, aes(x, y)) +
    geom_area() +
    geom_line() +
    two_state(df)

  w <- anime(p, nframes = 6, fps = 10)
  expect_s3_class(w, "gganime")

  # The area animates polygons, the line polylines, and no element is attributed
  # to the other layer.
  layers <- layer_of_elements(w)
  tags <- tag_of_elements(w)
  expect_setequal(unname(layers), c("L1", "L2"))
  expect_true(all(tags[layers == "L1"] == "polygon"))
  expect_true(all(tags[layers == "L2"] == "polyline"))

  # The ribbon's outline polylines are drawn but left unannotated: there are more
  # polylines in the panel than the line layer has elements.
  doc <- widget_doc(w)
  expect_gt(
    length(xml2::xml_find_all(
      doc,
      ".//g[starts-with(@id,'panel-1.gTree')]//polyline"
    )),
    sum(layers == "L2")
  )
  expect_within_panels(w)
})

test_that("two layers of the same geom keep their own elements", {
  df <- layer_df()
  p <- ggplot(df, aes(x, y)) +
    geom_point(size = 4) +
    geom_point(aes(y = y + 1), size = 2) +
    two_state(df)

  w <- anime(p, nframes = 6, fps = 10)
  # Four points per layer, each in its own layer group.
  expect_equal(unname(layer_of_elements(w)), rep(c("L1", "L2"), each = 4))
  expect_within_panels(w)
})

test_that("two bar layers keep their own rects", {
  df <- data.frame(
    state = rep(c("s1", "s2"), each = 3),
    g = rep(c("a", "b", "c"), 2),
    v = c(10, 90, 40, 60, 20, 80)
  )
  p <- ggplot(df, aes(g, v)) +
    geom_col() +
    geom_col(aes(y = v / 2), width = 0.4) +
    transition_states(state, transition_length = 1, state_length = 1)

  w <- anime(p, nframes = 6, fps = 10)
  expect_equal(unname(layer_of_elements(w)), rep(c("L1", "L2"), each = 3))
  expect_within_panels(w)
})

test_that("a point layer and a line layer animate together", {
  df <- layer_df()
  p <- ggplot(df, aes(x, y)) +
    geom_line() +
    geom_point(size = 4) +
    two_state(df)

  w <- anime(p, nframes = 6, fps = 10)
  layers <- layer_of_elements(w)
  tags <- tag_of_elements(w)
  expect_true(all(tags[layers == "L1"] == "polyline"))
  # Circular pch inline to <circle>, so the point layer's nodes are circles.
  expect_true(all(tags[layers == "L2"] == "circle"))
  expect_equal(sum(layers == "L2"), 4L)
  expect_within_panels(w)
})

test_that("a static layer keeps its child slot without taking the other's nodes", {
  # A layer with no transition variable is static; it still occupies a panel
  # child slot, so the animated layer's index must count it.
  df <- layer_df()
  p <- ggplot(df, aes(x, y)) +
    geom_point(data = data.frame(x = 2, y = 2), size = 6) +
    geom_point(size = 4) +
    two_state(df)

  w <- anime(p, nframes = 6, fps = 10)
  expect_true(all(layer_of_elements(w) %in% c("L1", "L2")))
  expect_within_panels(w)
})

test_that("layer scoping composes with facets", {
  df <- data.frame(
    facet = rep(c("a", "b"), each = 4),
    state = rep(c("s1", "s2"), 4),
    x = rep(c(1, 2), 4),
    y = c(1, 4, 2, 3, 4, 1, 3, 2)
  )
  p <- ggplot(df, aes(x, y)) +
    geom_line() +
    geom_point(size = 4) +
    facet_wrap(~facet) +
    transition_states(state, transition_length = 1, state_length = 1)

  w <- anime(p, nframes = 6, fps = 10)
  expect_setequal(unname(layer_of_elements(w)), c("L1", "L2"))
  expect_equal(sort(unique(panel_of_elements(w))), c("1", "2"))
  expect_within_panels(w)
})
