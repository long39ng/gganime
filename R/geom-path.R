# Path adapter, shared by geom_line and geom_path (GeomLine draws through
# GeomPath). A line exports as one <polyline> per group, so the `points` string
# animates alongside stroke and stroke-opacity. Each element is a row-group of
# vertices; the vertex count is normalised across frames so `points` strings
# tween at a constant arity. Vertices are (to_svg_x(x), to_svg_y(y)) in the same
# y-up space as rects (no extra flip).

# --- GeomPath --------------------------------------------------------------

#' @exportS3Method
gganime_annotate.GeomPath <- function(
  geom,
  doc,
  layer_index,
  ids,
  ...
) {
  nodes <- path_nodes(doc)
  if (length(nodes) != length(ids)) {
    cli::cli_abort(c(
      "Line element count does not match the union.",
      x = "Found {length(nodes)} SVG polyline{?s} but expected {length(ids)}.",
      i = "A line needs at least two points in every frame to be drawn."
    ))
  }
  for (k in seq_along(nodes)) {
    set_element_id(nodes[[k]], layer_index, ids[k])
  }
  invisible(doc)
}

#' @exportS3Method
gganime_element_tracks.GeomPath <- function(
  geom,
  union,
  frames,
  affine,
  precision,
  ids,
  ...
) {
  nframes <- nrow(union$presence)
  lapply(seq_len(ncol(union$presence)), function(k) {
    present <- union$presence[, k]

    verts <- vector("list", nframes)
    stroke <- rep(NA_character_, nframes)
    stroke_op <- rep(NA_real_, nframes)
    for (f in which(present)) {
      row <- frames[[f]][union$frame_index[[f]][[k]], , drop = FALSE]
      verts[[f]] <- cbind(
        affine$to_svg_x(row$x),
        affine$to_svg_y(row$y)
      )
      stroke[f] <- to_hex(row$colour[1])
      stroke_op[f] <- if (is.na(row$alpha[1])) 1 else row$alpha[1]
    }

    verts <- normalise_vertices(verts)
    points <- rep(NA_character_, nframes)
    for (f in which(present)) {
      points[f] <- vertices_to_points(verts[[f]], precision)
    }

    tracks <- list()
    tracks$points <- hold_absent(points, present)
    tracks$stroke <- hold_absent(stroke, present)
    tracks[["stroke-opacity"]] <- round_track(
      hold_absent(stroke_op, present),
      precision
    )
    tracks$opacity <- presence_opacity(present)

    tracks <- drop_constant_tracks(tracks, keep = "opacity"[!all(present)])
    list(id = ids[k], tracks = tracks)
  })
}

# --- path helpers ----------------------------------------------------------

# Ordered <polyline> data nodes, in document (= union) order.
path_nodes <- function(doc) {
  panel_data_nodes(doc, "polyline")
}
