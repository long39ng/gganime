# Hand-built line frames on an identity affine, so points strings are the data
# coordinates directly. Each element is a row-group of vertices sharing (.id),
# so the union is built grouped.
line_row <- function(id, x, y, colour = "#111111", alpha = NA_real_) {
  data.frame(
    PANEL = factor(1),
    .id = id,
    x = x,
    y = y,
    colour = colour,
    alpha = alpha,
    stringsAsFactors = FALSE
  )
}

identity_affine <- function() {
  list(to_svg_x = function(x) x, to_svg_y = function(y) y, res = 96)
}

path_tracks_for <- function(frames) {
  union <- union_elements(frames, grouped = TRUE)
  ids <- element_id(1, seq_len(ncol(union$presence)))
  gganime_element_tracks(
    geom_adapter("GeomPath"),
    union = union,
    frames = frames,
    affine = identity_affine(),
    precision = 2,
    ids = ids
  )
}

test_that("a line whose vertices move animates only points", {
  frames <- list(
    line_row(1, c(0, 1, 2), c(0, 0, 0)),
    line_row(1, c(0, 1, 2), c(1, 1, 1)),
    line_row(1, c(0, 1, 2), c(2, 2, 2))
  )
  el <- path_tracks_for(frames)[[1]]
  expect_named(el$tracks, "points")
  expect_equal(
    el$tracks$points,
    c("0,0 1,0 2,0", "0,1 1,1 2,1", "0,2 1,2 2,2")
  )
})

test_that("a line that reveals grows from its tip via pad-to-max", {
  # Vertex count grows 2 -> 3 -> 4; shorter frames pad by repeating the last
  # vertex, so the padding rides at the tail and unfolds one vertex per frame.
  frames <- list(
    line_row(1, c(0, 1), c(0, 1)),
    line_row(1, c(0, 1, 2), c(0, 1, 2)),
    line_row(1, c(0, 1, 2, 3), c(0, 1, 2, 3))
  )
  el <- path_tracks_for(frames)[[1]]
  expect_equal(
    el$tracks$points,
    c(
      "0,0 1,1 1,1 1,1",
      "0,0 1,1 2,2 2,2",
      "0,0 1,1 2,2 3,3"
    )
  )
})

test_that("an exiting line keeps a presence channel and holds geometry", {
  frames <- list(
    rbind(line_row(1, c(0, 1), c(0, 0)), line_row(2, c(0, 1), c(2, 2))),
    rbind(line_row(1, c(0, 1), c(0, 0)), line_row(2, c(0, 1), c(2, 2))),
    line_row(1, c(0, 1), c(0, 0))
  )
  els <- path_tracks_for(frames)
  exiting <- els[[2]] # .id 2, absent in frame 3
  expect_equal(exiting$tracks$opacity, c(1, 1, 0))
  expect_equal(names(exiting$tracks), "opacity")
})

test_that("colour maps to stroke and alpha to stroke-opacity", {
  frames <- list(
    line_row(1, c(0, 1), c(0, 0), colour = "#ff0000", alpha = 1),
    line_row(1, c(0, 1), c(0, 0), colour = "#00ff00", alpha = 0.5)
  )
  el <- path_tracks_for(frames)[[1]]
  expect_equal(el$tracks$stroke, c("#FF0000", "#00FF00"))
  expect_equal(el$tracks[["stroke-opacity"]], c(1, 0.5))
  expect_false("points" %in% names(el$tracks)) # geometry unchanged, dropped
})

test_that("path_nodes selects only panel data polylines, in document order", {
  doc <- xml2::read_xml(
    '<svg xmlns="http://www.w3.org/2000/svg">
       <g id="layout::panel.9-7-9-7::GRID.VP.1.1">
         <g id="panel-1.gTree.16.1">
           <polyline id="panel.grid.major.y..polyline.8.1.1"/>
           <g id="GRID.polyline.1.1">
             <polyline id="GRID.polyline.1.1.1"/>
             <polyline id="GRID.polyline.1.1.2"/>
           </g>
         </g>
       </g>
       <g id="layout::axis-b.10-7-10-7::GRID.VP.2">
         <polyline id="GRID.polyline.17.1.1"/>
       </g>
     </svg>'
  )
  xml2::xml_ns_strip(doc)
  nodes <- path_nodes(doc)
  expect_equal(
    vapply(nodes, function(n) xml2::xml_attr(n, "id"), character(1)),
    c("GRID.polyline.1.1.1", "GRID.polyline.1.1.2")
  )
})
