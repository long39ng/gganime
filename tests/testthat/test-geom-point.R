library(ggplot2)

# Three points that swap position and size across two states, so every geometry
# track of whatever shape they are drawn as varies.
shape_data <- function() {
  data.frame(
    id = rep(1:3, 2),
    state = rep(c("a", "b"), each = 3),
    x = c(1, 2, 3, 3, 2, 1),
    y = c(1, 2, 3, 3, 2, 1),
    sz = c(2, 4, 6, 6, 4, 2)
  )
}

shape_plot <- function(shape) {
  ggplot(shape_data(), aes(x, y, group = id, size = sz)) +
    geom_point(shape = shape, colour = "red", fill = "steelblue", stroke = 2) +
    transition_states(state, transition_length = 1, state_length = 1)
}

# The element type every animated node was drawn as, and the props animated on
# them, for one pch.
shape_render <- function(shape) {
  widget <- anime(shape_plot(shape), nframes = 8, fps = 10)
  nodes <- xml2::xml_find_all(widget_doc(widget), ".//*[@data-animejs-id]")
  list(
    widget = widget,
    tags = unique(xml2::xml_name(nodes)),
    props = animated_props(widget$x$config$segments)
  )
}

test_that("point_symbol_px matches the gridSVG font-size mapping", {
  # font-size = size*.pt + stroke*.stroke/2 points, converted at res/72 (big
  # points). The <use> exported for this point was 5.05 px wide.
  expect_equal(
    point_symbol_px(1, 0.5, res = 96),
    (1 * ggplot2::.pt + 0.5 * ggplot2::.stroke / 2) * 96 / 72
  )
  expect_equal(point_symbol_px(1, 0.5, res = 96), 5.05, tolerance = 1e-2)
  # pch19 carries r 3.75 in a 10-unit viewBox, so its radius is 0.375 of that.
  expect_equal(
    pch_symbols(19)[["gridSVG.pch19"]]$geom$r * point_symbol_px(1, 0.5, 96),
    0.375 * 5.05,
    tolerance = 1e-2
  )
})

test_that("point_stroke_px matches the width gridSVG writes on a <use>", {
  # grid measures lwd in 1/96 inch, one user unit is 1/res inch. A size 3,
  # stroke 1 point exported at res 96 as stroke-width 1.36 on a 13.9-wide <use>,
  # which is 1.36 * 13.9 / 10 once the symbol scale is undone.
  expect_equal(point_stroke_px(1, res = 96), ggplot2::.stroke / 2)
  expect_equal(point_stroke_px(1, res = 96), 1.36 * 13.9 / 10, tolerance = 1e-2)
  expect_equal(point_stroke_px(1, res = 192), 2 * point_stroke_px(1, res = 96))
})

test_that("symbol_table records the kind and normalised geometry of each pch", {
  symbols <- pch_symbols(0, 2, 8, 19, 22, 23)

  expect_equal(
    symbols[["gridSVG.pch19"]],
    list(kind = "circle", geom = list(cx = 0, cy = 0, r = 0.375), vbw = 10)
  )
  expect_equal(
    symbols[["gridSVG.pch0"]],
    list(
      kind = "rect",
      geom = list(x = -0.375, y = -0.375, width = 0.75, height = 0.75),
      vbw = 10
    )
  )
  # A triangle keeps its <polyline> and a diamond its <polygon>, so each inlines
  # as the element gridSVG drew, with the same closing vertex.
  expect_equal(symbols[["gridSVG.pch2"]]$kind, "polyline")
  expect_equal(
    symbols[["gridSVG.pch2"]]$geom$points,
    rbind(c(0, 0.583), c(0.505, -0.292), c(-0.505, -0.292), c(0, 0.583))
  )
  expect_equal(symbols[["gridSVG.pch23"]]$kind, "polygon")
  expect_equal(nrow(symbols[["gridSVG.pch23"]]$geom$points), 5L)
  # pch 22's square is smaller than pch 0's, so the factor is read per symbol.
  expect_equal(symbols[["gridSVG.pch22"]]$geom$width, 0.664)
  # A composite pch has no single shape to inline.
  expect_equal(
    symbols[["gridSVG.pch8"]],
    list(kind = "use", geom = NULL, vbw = NA_real_)
  )
})

test_that("symbol_geometry scales a symbol onto a point's position", {
  symbols <- pch_symbols(0, 19, 23, 8)

  expect_equal(
    symbol_geometry(symbols[["gridSVG.pch19"]], px = 10, py = 20, scale = 8),
    list(cx = 10, cy = 20, r = 3)
  )
  expect_equal(
    symbol_geometry(symbols[["gridSVG.pch0"]], px = 10, py = 20, scale = 8),
    list(x = 7, y = 17, width = 6, height = 6)
  )
  # A frozen pch keeps the <use>, whose own x/y is already the visible centre.
  expect_equal(
    symbol_geometry(symbols[["gridSVG.pch8"]], px = 10, py = 20, scale = 8),
    list(x = 10, y = 20)
  )
  # The vertex string is built per frame, so vector input gives one string each.
  points <- symbol_geometry(
    symbols[["gridSVG.pch23"]],
    px = c(10, 20),
    py = c(0, 0),
    scale = c(10, 10),
    precision = 2
  )$points
  expect_equal(points[[1]], "5.3,0 10,4.7 14.7,0 10,-4.7 5.3,0")
  expect_equal(points[[2]], "15.3,0 20,4.7 24.7,0 20,-4.7 15.3,0")
})

test_that("point_paint follows how each pch family is drawn", {
  row <- function(shape) {
    data.frame(
      shape = shape,
      colour = "#112233",
      fill = "#445566",
      alpha = NA_real_
    )
  }
  channels <- function(shape) point_paint(row(shape))[c("fill", "stroke")]
  # 0-14 stroked only, 15-18 filled only, 19-20 both in colour, 21-25 filled
  # from the fill aesthetic and stroked in colour
  expect_equal(channels(1), list(fill = NA_character_, stroke = "#112233"))
  expect_equal(channels(15), list(fill = "#112233", stroke = NA_character_))
  expect_equal(channels(19), list(fill = "#112233", stroke = "#112233"))
  expect_equal(channels(21), list(fill = "#445566", stroke = "#112233"))
})

test_that("point_paint leaves an unset colour as NA", {
  row <- data.frame(shape = 21, colour = "#112233", fill = NA, alpha = NA_real_)
  expect_equal(point_paint(row)$fill, NA_character_)
  expect_equal(point_paint(row)$stroke, "#112233")
})

test_that("point_paint combines the alpha aesthetic with the colour's alpha", {
  row <- function(colour, fill, alpha) {
    data.frame(shape = 21, colour = colour, fill = fill, alpha = alpha)
  }
  # no alpha anywhere
  paint <- point_paint(row("#112233", "#445566", NA_real_))
  expect_equal(paint$fill_opacity, 1)
  expect_equal(paint$stroke_opacity, 1)
  # the alpha aesthetic alone applies to both channels
  paint <- point_paint(row("#112233", "#445566", 0.5))
  expect_equal(paint$fill_opacity, 0.5)
  expect_equal(paint$stroke_opacity, 0.5)
  # a colour-borne alpha channel is per channel, and multiplies with the
  # aesthetic
  paint <- point_paint(row("#11223380", "#445566", NA_real_))
  expect_equal(paint$fill_opacity, 1)
  expect_equal(paint$stroke_opacity, 128 / 255)
  paint <- point_paint(row("#112233", "#44556680", 0.5))
  expect_equal(paint$fill_opacity, 0.5 * 128 / 255)
  expect_equal(paint$stroke_opacity, 0.5)
})

test_that("a circular pch inlines as <circle> and animates its radius", {
  out <- shape_render(19)
  expect_equal(out$tags, "circle")
  expect_equal(out$props, c("cx", "cy", "r"))
})

test_that("a square pch inlines as <rect> and animates its extent", {
  out <- shape_render(22)
  expect_equal(out$tags, "rect")
  expect_equal(out$props, c("height", "width", "x", "y"))
})

test_that("a triangular pch inlines as <polyline> and animates its vertices", {
  # gridSVG draws pch 24 as an open polyline that repeats its first vertex, so
  # inlining keeps the polyline rather than closing it into a polygon.
  out <- shape_render(24)
  expect_equal(out$tags, "polyline")
  expect_equal(out$props, "points")
  points <- track(out$widget$x$config$segments[[1]], "points")
  expect_gt(length(unique(points)), 1L)
  expect_true(all(lengths(strsplit(points, " ")) == 4L))
})

test_that("a diamond pch inlines as <polygon> and animates its vertices", {
  out <- shape_render(23)
  expect_equal(out$tags, "polygon")
  expect_equal(out$props, "points")
})

test_that("an inlined point's outline width undoes the symbol scale", {
  # gridSVG divides a <use>'s stroke-width by the symbol scale, because the
  # viewBox transform scales it back up inside the symbol. Nothing scales it
  # outside, so the inlined shape carries the width in user units.
  w <- anime(shape_plot(21), nframes = 8, fps = 10)
  node <- xml2::xml_find_first(widget_doc(w), ".//circle[@data-animejs-id]")
  expect_equal(
    as.numeric(xml2::xml_attr(node, "stroke-width")),
    point_stroke_px(2, res = 96),
    tolerance = 1e-2
  )
})

test_that("a mapped stroke animates the outline width", {
  df <- data.frame(
    id = rep(1:3, 2),
    state = rep(c("a", "b"), each = 3),
    x = rep(1:3, 2),
    y = rep(1:3, 2),
    st = c(1, 2, 3, 3, 2, 1)
  )
  p <- ggplot(df, aes(x, y, group = id, stroke = st)) +
    geom_point(shape = 21, size = 6) +
    transition_states(state, transition_length = 1, state_length = 1)
  w <- anime(p, nframes = 8, fps = 10)

  sw <- track(w$x$config$segments[[1]], "stroke-width")
  expect_equal(
    as.numeric(sw[[1]]),
    point_stroke_px(1, res = 96),
    tolerance = 1e-2
  )
  expect_gt(max(as.numeric(sw)), min(as.numeric(sw)))
})

test_that("a composite pch keeps its <use> and warns once per layer", {
  # The warning used to fire once per gganime_element_tracks() call, so a shadow
  # doubled it. It now lives in the single annotate pass.
  w <- NULL
  warnings <- capture_warnings(
    w <- anime(shape_plot(8) + shadow_mark(), nframes = 8, fps = 10)
  )
  expect_length(warnings, 1L)
  expect_match(warnings, "Affected pch: 8")

  nodes <- xml2::xml_find_all(widget_doc(w), ".//*[@data-animejs-id]")
  expect_equal(unique(xml2::xml_name(nodes)), "use")
  expect_equal(animated_props(w$x$config$segments), c("opacity", "x", "y"))
})

test_that("a layer mixing frozen and inlined pch names both in one warning", {
  p <- ggplot(shape_data(), aes(x, y, group = id, shape = factor(id))) +
    geom_point(size = 6) +
    scale_shape_manual(values = c(19, 8, 13)) +
    transition_states(state, transition_length = 1, state_length = 1)
  w <- NULL
  warnings <- capture_warnings(w <- anime(p, nframes = 8, fps = 10))
  expect_length(warnings, 1L)
  expect_match(warnings, "8.*13")

  tags <- xml2::xml_name(xml2::xml_find_all(
    widget_doc(w),
    ".//*[@data-animejs-id]"
  ))
  expect_equal(sort(tags), c("circle", "use", "use"))
})

test_that("an inlined polygon point does not reach another layer's selector", {
  # A diamond pch inlines as <polygon>, the same tag an area layer's element
  # exports as. Layer-scoped selection keeps them apart whichever draws first.
  df <- data.frame(
    x = rep(1:4, 2),
    y = rep(c(1, 3, 2, 4), 2),
    state = rep(c("a", "b"), each = 4)
  )
  p <- ggplot(df, aes(x, y)) +
    geom_point(shape = 23, size = 6, fill = "steelblue") +
    geom_area(alpha = 0.3) +
    transition_states(state, transition_length = 1, state_length = 1)
  w <- anime(p, nframes = 8, fps = 10)

  nodes <- xml2::xml_find_all(widget_doc(w), ".//*[@data-animejs-id]")
  layers <- xml2::xml_attr(nodes, "data-layer")
  expect_true(all(xml2::xml_name(nodes) == "polygon"))
  expect_equal(sum(layers == "1"), 4L)
  # Each point is a five-vertex diamond; the area ring has more, so a swapped
  # selection would show up here rather than only in the element count.
  vertices <- lengths(strsplit(xml2::xml_attr(nodes, "points"), " "))
  expect_true(all(vertices[layers == "1"] == 5L))
  expect_true(all(vertices[layers == "2"] > 5L))
})
