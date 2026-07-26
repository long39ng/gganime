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

path_tracks_for <- function(frames) {
  union <- union_elements(frames, grouped = TRUE)
  n <- ncol(union$presence)
  gganime_element_tracks(
    geom_adapter("GeomPath"),
    union = union,
    frames = frames,
    affines = identity_affines(n),
    precision = 2,
    ids = element_id(1, seq_len(n))
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

# A two-layer export, in the shape gridSVG produces once name_layer_grobs() has
# renamed each panel's layer grobs: one `gganime.L<i>` group per layer inside the
# panel gTree. Layer 1 is a geom_area (a filled <polygon> plus the `stroke: none`
# outline <polyline> GeomRibbon draws), layer 2 a geom_line.
two_layer_doc <- function() {
  doc <- xml2::read_xml(
    '<svg xmlns="http://www.w3.org/2000/svg">
       <g id="layout::panel.9-7-9-7::GRID.VP.1.1">
         <g id="panel-1.gTree.16.1">
           <polyline id="panel.grid.major.y..polyline.8.1.1"/>
           <g id="gganime.L1.1">
             <polygon id="gganime.L1.1.1"/>
             <polyline id="gganime.L1.1.2"/>
           </g>
           <g id="gganime.L2.1">
             <polyline id="gganime.L2.1.1"/>
             <polyline id="gganime.L2.1.2"/>
           </g>
         </g>
       </g>
       <g id="layout::axis-b.10-7-10-7::GRID.VP.2">
         <polyline id="GRID.polyline.17.1.1"/>
       </g>
     </svg>'
  )
  xml2::xml_ns_strip(doc)
  doc
}

node_ids <- function(nodes) {
  vapply(nodes, function(n) xml2::xml_attr(n, "id"), character(1))
}

test_that("path_nodes selects only its own layer's polylines, in document order", {
  nodes <- path_nodes(two_layer_doc(), 2L, panels = c("1", "1"))
  expect_equal(node_ids(nodes), c("gganime.L2.1.1", "gganime.L2.1.2"))
})

test_that("a ribbon layer's stroke-none outline polyline stays out of the path layer", {
  # The outline polyline sits inside the ribbon layer's own group, and only the
  # polygon adapter runs there, so scoping to the layer keeps the two apart
  # without any sibling-<polygon> heuristic. Before layer scoping the path
  # selector matched all three polylines in the panel.
  doc <- two_layer_doc()
  expect_equal(node_ids(polygon_nodes(doc, 1L, panels = "1")), "gganime.L1.1.1")
  expect_equal(node_ids(path_nodes(doc, 1L, panels = "1")), "gganime.L1.1.2")
  expect_length(
    xml2::xml_find_all(
      doc,
      ".//g[starts-with(@id, 'panel-1.gTree')]//polyline"
    ),
    4L
  )
})

test_that("a layer index does not match a longer one with the same prefix", {
  doc <- xml2::read_xml(
    '<svg xmlns="http://www.w3.org/2000/svg">
       <g id="layout::panel.9-7-9-7::GRID.VP.1.1">
         <g id="panel-1.gTree.16.1">
           <g id="gganime.L10.1"><polyline id="gganime.L10.1.1"/></g>
         </g>
       </g>
     </svg>'
  )
  xml2::xml_ns_strip(doc)
  expect_length(path_nodes(doc, 1L, panels = character(0)), 0L)
  expect_equal(node_ids(path_nodes(doc, 10L, "1")), "gganime.L10.1.1")
})
