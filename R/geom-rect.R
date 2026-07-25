# Rect adapter, shared by geom_col and geom_bar (both draw through GeomRect).
# A bar exports as a plain <rect>, so x/y/width/height animate directly. gridSVG
# wraps the whole document in a single translate/scale-1,-1 flip, so the rect
# attributes live in the same y-up space as the data->SVG affine: the rect's
# origin is the (xmin, ymin) corner and its extents are positive.

# --- GeomRect --------------------------------------------------------------

#' @exportS3Method
gganime_annotate.GeomRect <- function(
  geom,
  doc,
  layer_index,
  ids,
  panels,
  ...
) {
  nodes <- rect_nodes(doc, panels)
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
  affines,
  precision,
  ids,
  ...
) {
  nframes <- nrow(union$presence)
  lapply(seq_len(ncol(union$presence)), function(k) {
    present <- union$presence[, k]
    affine <- affines[[k]]

    x <- y <- w <- h <- fill_op <- rep(NA_real_, nframes)
    fill <- rep(NA_character_, nframes)
    for (f in seq_len(nframes)) {
      if (!present[f]) {
        next
      }
      row <- frames[[f]][union$frame_index[[f]][k], , drop = FALSE]
      # Both SVG axes rise with their data range, so the (xmin, ymin) corner is
      # the rect's origin whether or not the coord is flipped, and the extents
      # come out positive either way.
      corners <- affine_xy(
        affine,
        c(row$xmin, row$xmax),
        c(row$ymin, row$ymax)
      )
      x[f] <- corners[1, 1]
      y[f] <- corners[1, 2]
      w[f] <- corners[2, 1] - corners[1, 1]
      h[f] <- corners[2, 2] - corners[1, 2]
      fill[f] <- to_hex(row$fill)
      fill_op[f] <- paint_opacity(row$alpha, row$fill)
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

# The layer's <rect> data nodes in union order, gathered per panel.
# GeomCol/GeomBar/GeomRect all draw a grob named "geom_rect", so one id prefix
# covers the three; theme and legend rects have other ids.
rect_nodes <- function(doc, panels) {
  ordered_data_nodes(doc, select_rect_nodes, panels, "Rect", "rect")
}

select_rect_nodes <- function(doc, panel) {
  xml2::xml_find_all(
    doc,
    sprintf(
      ".//rect[starts-with(@id,'geom_rect') and %s]",
      in_panel_group(panel)
    )
  )
}
