# Polygon adapter for geom_area, geom_ribbon (both draw through GeomRibbon), and
# geom_polygon. Each element exports as one filled <polygon>; ribbon/area also
# emit an outline <polyline> whose stroke is `none` by default, so only the
# polygon is animated. The `points` string animates alongside fill and
# fill-opacity, at a constant vertex arity per element.
#
# A ribbon/area ring is built from ymin/ymax: forward along the top edge, back
# along the bottom. A raw polygon's rows are already the ring. Vertices are
# (to_svg_x, to_svg_y) in the same y-up space as rects.

# --- GeomRibbon (geom_area / geom_ribbon) ----------------------------------

#' @exportS3Method
gganime_annotate.GeomRibbon <- function(geom, doc, layer_index, ids, ...) {
  annotate_polygons(doc, layer_index, ids)
}

#' @exportS3Method
gganime_element_tracks.GeomRibbon <- function(
  geom,
  union,
  frames,
  affine,
  precision,
  ids,
  ...
) {
  polygon_tracks(union, frames, affine, precision, ids, ribbon_ring)
}

# --- GeomPolygon (geom_polygon) --------------------------------------------

#' @exportS3Method
gganime_annotate.GeomPolygon <- function(geom, doc, layer_index, ids, ...) {
  annotate_polygons(doc, layer_index, ids)
}

#' @exportS3Method
gganime_element_tracks.GeomPolygon <- function(
  geom,
  union,
  frames,
  affine,
  precision,
  ids,
  ...
) {
  polygon_tracks(union, frames, affine, precision, ids, polygon_ring)
}

# --- polygon helpers -------------------------------------------------------

# Ordered <polygon> data nodes, in document (= union) order.
polygon_nodes <- function(doc) {
  panel_data_nodes(doc, "polygon")
}

annotate_polygons <- function(doc, layer_index, ids) {
  nodes <- polygon_nodes(doc)
  if (length(nodes) != length(ids)) {
    cli::cli_abort(c(
      "Polygon element count does not match the union.",
      x = "Found {length(nodes)} SVG polygon{?s} but expected {length(ids)}."
    ))
  }
  for (k in seq_along(nodes)) {
    set_element_id(nodes[[k]], layer_index, ids[k])
  }
  invisible(doc)
}

# A ribbon/area ring in data coordinates: top edge left-to-right, then the
# bottom edge right-to-left, closing the band.
ribbon_ring <- function(row) {
  o <- order(row$x)
  x <- row$x[o]
  cbind(c(x, rev(x)), c(row$ymax[o], rev(row$ymin[o])))
}

# A raw polygon's rows are already its ring, in order.
polygon_ring <- function(row) {
  cbind(row$x, row$y)
}

polygon_tracks <- function(union, frames, affine, precision, ids, ring) {
  nframes <- nrow(union$presence)
  lapply(seq_len(ncol(union$presence)), function(k) {
    present <- union$presence[, k]

    verts <- vector("list", nframes)
    fill <- rep(NA_character_, nframes)
    fill_op <- rep(NA_real_, nframes)
    for (f in which(present)) {
      row <- frames[[f]][union$frame_index[[f]][[k]], , drop = FALSE]
      xy <- ring(row)
      verts[[f]] <- cbind(affine$to_svg_x(xy[, 1]), affine$to_svg_y(xy[, 2]))
      fill[f] <- to_hex(row$fill[1])
      fill_op[f] <- if (is.na(row$alpha[1])) 1 else row$alpha[1]
    }

    verts <- normalise_vertices(verts)
    points <- rep(NA_character_, nframes)
    for (f in which(present)) {
      points[f] <- vertices_to_points(verts[[f]], precision)
    }

    tracks <- list()
    tracks$points <- hold_absent(points, present)
    tracks$fill <- hold_absent(fill, present)
    tracks[["fill-opacity"]] <- round_track(
      hold_absent(fill_op, present),
      precision
    )
    tracks$opacity <- presence_opacity(present)

    tracks <- drop_constant_tracks(tracks, keep = "opacity"[!all(present)])
    list(id = ids[k], tracks = tracks)
  })
}
