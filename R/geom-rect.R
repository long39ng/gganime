# Rect adapter, shared by geom_col and geom_bar (both draw through GeomRect).
# A bar exports as a plain <rect>, so x/y/width/height animate directly. gridSVG
# wraps the whole document in a single translate/scale-1,-1 flip, so the rect
# attributes live in the same y-up space as the data->SVG affine: the rect's y
# is to_svg_y(ymin) (the smaller edge) and its height is positive.

# --- GeomRect --------------------------------------------------------------

#' @exportS3Method
gganime_annotate.GeomRect <- function(
  geom,
  doc,
  layer_index,
  ids,
  ...
) {
  nodes <- rect_nodes(doc)
  if (length(nodes) != length(ids)) {
    cli::cli_abort(c(
      "Rect element count does not match the union.",
      x = "Found {length(nodes)} SVG rect{?s} but expected {length(ids)}."
    ))
  }
  for (k in seq_along(nodes)) {
    set_element_id(nodes[[k]], layer_index, ids[k])
  }
  invisible(doc)
}

#' @exportS3Method
gganime_element_tracks.GeomRect <- function(
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

    x <- y <- w <- h <- fill_op <- rep(NA_real_, nframes)
    fill <- rep(NA_character_, nframes)
    for (f in seq_len(nframes)) {
      if (!present[f]) {
        next
      }
      row <- frames[[f]][union$frame_index[[f]][k], , drop = FALSE]
      x0 <- affine$to_svg_x(row$xmin)
      y0 <- affine$to_svg_y(row$ymin)
      x[f] <- x0
      y[f] <- y0
      w[f] <- affine$to_svg_x(row$xmax) - x0
      h[f] <- affine$to_svg_y(row$ymax) - y0
      fill[f] <- to_hex(row$fill)
      fill_op[f] <- if (is.na(row$alpha)) 1 else row$alpha
    }

    tracks <- list()
    tracks$x <- round_track(hold_absent(x, present), precision)
    tracks$y <- round_track(hold_absent(y, present), precision)
    tracks$width <- round_track(hold_absent(w, present), precision)
    tracks$height <- round_track(hold_absent(h, present), precision)
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

# --- rect helpers ----------------------------------------------------------

# Ordered <rect> data nodes, in document order. GeomCol/GeomBar/GeomRect all
# draw a grob named "geom_rect", so one id prefix covers the three; theme and
# legend rects carry other ids.
rect_nodes <- function(doc) {
  xml2::xml_find_all(doc, ".//rect[starts-with(@id,'geom_rect')]")
}
