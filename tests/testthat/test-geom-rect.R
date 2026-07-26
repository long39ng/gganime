# Hand-built bar frames on an identity affine: a bar that changes height
# (.id 1), an exit (.id 2, gone after frame 2), and an entrant (.id 3, appears
# frame 3). to_svg_x/to_svg_y are identity, so x = xmin, width = xmax - xmin,
# y = ymin, height = ymax - ymin.
make_bar_frames <- function() {
  bar <- function(id, xmin, xmax, ymax, fill) {
    data.frame(
      PANEL = factor(1),
      .id = id,
      xmin = xmin,
      xmax = xmax,
      ymin = 0,
      ymax = ymax,
      fill = fill,
      alpha = NA_real_,
      stringsAsFactors = FALSE
    )
  }
  list(
    rbind(bar(1, 0, 1, 3, "#111111"), bar(2, 1, 2, 5, "#222222")),
    rbind(bar(1, 0, 1, 4, "#111111"), bar(2, 1, 2, 5, "#222222")),
    rbind(bar(1, 0, 1, 5, "#111111"), bar(3, 2, 3, 2, "#333333"))
  )
}

tracks_for <- function(frames) {
  union <- union_elements(frames)
  n <- ncol(union$presence)
  gganime_element_tracks(
    geom_adapter("GeomRect"),
    union = union,
    frames = frames,
    affines = identity_affines(n),
    precision = 2,
    ids = element_id(1, seq_len(n))
  )
}

test_that("a bar that changes height animates only height", {
  el <- tracks_for(make_bar_frames())[[1]]
  # x, y, width, fill are constant across frames and drop out.
  expect_named(el$tracks, "height")
  expect_equal(el$tracks$height, c(3, 4, 5))
})

test_that("an exiting bar keeps a presence channel and holds geometry", {
  el <- tracks_for(make_bar_frames())[[2]] # .id 2, absent in frame 3
  expect_equal(el$tracks$opacity, c(1, 1, 0))
  # geometry held at the last present frame, so every other track is constant.
  expect_equal(names(el$tracks), "opacity")
})

test_that("an entering bar is invisible until it appears", {
  el <- tracks_for(make_bar_frames())[[3]] # .id 3, appears in frame 3
  expect_equal(el$tracks$opacity, c(0, 0, 1))
  expect_equal(names(el$tracks), "opacity")
})

test_that("rect geometry follows the y-up affine: y = ymin edge, height > 0", {
  # A bar that shifts and grows across two frames, on an offset/scaled affine,
  # so every geometry track survives the constant-drop and can be checked.
  frame <- function(xmin, xmax, ymin, ymax) {
    data.frame(
      PANEL = factor(1),
      .id = 1,
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = "#abcdef",
      alpha = NA_real_,
      stringsAsFactors = FALSE
    )
  }
  frames <- list(frame(2, 5, 0, 4), frame(3, 7, 1, 6))
  affine <- list(
    to_svg_x = function(x) 10 + x * 2,
    to_svg_y = function(y) 100 + y * 3,
    res = 96
  )
  union <- union_elements(frames)
  el <- gganime_element_tracks(
    geom_adapter("GeomRect"),
    union = union,
    frames = frames,
    affines = list(affine),
    precision = 2,
    ids = element_id(1, 1L)
  )[[1]]
  expect_equal(el$tracks$x, c(14, 16))
  expect_equal(el$tracks$y, c(100, 103)) # the smaller (ymin) edge, not ymax
  expect_equal(el$tracks$width, c(6, 8))
  expect_equal(el$tracks$height, c(12, 15)) # positive, top edge minus bottom
})

test_that("rect_nodes selects only its own layer's data rects, in document order", {
  # Two geom_col layers: without layer scoping both selectors match all four
  # rects and the union count check aborts.
  doc <- xml2::read_xml(
    '<svg xmlns="http://www.w3.org/2000/svg">
       <g id="panel-1.gTree.16.1">
         <g id="grill.gTree.3"><rect id="panel.background.1"/></g>
         <g id="gganime.L1.2.1">
           <rect id="gganime.L1.2.1.1"/>
           <rect id="gganime.L1.2.1.2"/>
         </g>
         <g id="gganime.L2.4.1">
           <rect id="gganime.L2.4.1.1"/>
           <rect id="gganime.L2.4.1.2"/>
         </g>
       </g>
       <rect id="key-1-1-bg"/>
     </svg>'
  )
  xml2::xml_ns_strip(doc)
  ids <- function(nodes) {
    vapply(nodes, function(n) xml2::xml_attr(n, "id"), character(1))
  }
  expect_equal(
    ids(rect_nodes(doc, 1L, panels = c("1", "1"))),
    c("gganime.L1.2.1.1", "gganime.L1.2.1.2")
  )
  expect_equal(
    ids(rect_nodes(doc, 2L, panels = c("1", "1"))),
    c("gganime.L2.4.1.1", "gganime.L2.4.1.2")
  )
})
