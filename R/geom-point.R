# Point adapter. Generics dispatch on the geom's S3 class vector, so later geoms
# add methods without touching the pipeline.

# --- generics --------------------------------------------------------------

# Annotate a layer's SVG nodes: assign animation ids and, for circular pch,
# inline the <use> as a <circle> so radius can tween.
gganime_annotate <- function(geom, ...) {
  UseMethod("gganime_annotate")
}

# Build per-element keyframe tracks for a layer.
gganime_element_tracks <- function(geom, ...) {
  UseMethod("gganime_element_tracks")
}

# Construct a dispatch object carrying the geom's S3 class vector.
geom_adapter <- function(geom_class) {
  structure(list(), class = geom_class)
}

# --- GeomPoint -------------------------------------------------------------

#' @exportS3Method
gganime_annotate.GeomPoint <- function(
  geom,
  doc,
  layer_index,
  ids,
  union_data,
  symbols,
  panels,
  ...
) {
  nodes <- point_nodes(doc, layer_index, panels)
  info <- point_symbol_info(union_data, symbols)
  for (k in seq_along(nodes)) {
    if (info[[k]]$is_circle) {
      w <- as.numeric(xml2::xml_attr(nodes[[k]], "width"))
      inline_circle(nodes[[k]], info[[k]]$factor * w, layer_index, ids[k])
    } else {
      set_element_id(nodes[[k]], layer_index, ids[k])
    }
  }
  invisible(doc)
}

#' @exportS3Method
gganime_element_tracks.GeomPoint <- function(
  geom,
  union,
  frames,
  affines,
  precision,
  ids,
  symbols,
  ...
) {
  info <- point_symbol_info(union$union_data, symbols)
  if (!all(vapply(info, `[[`, logical(1), "is_circle"))) {
    cli::cli_warn(
      "Non-circular point shapes keep a fixed size; only circular pch animate size."
    )
  }

  nframes <- nrow(union$presence)
  lapply(seq_len(ncol(union$presence)), function(k) {
    present <- union$presence[, k]
    affine <- affines[[k]]
    is_circle <- info[[k]]$is_circle
    # The reference row decides which colour channels this element paints with;
    # `shape` is constant per element, so the channel set is too.
    channels <- point_paint(union$union_data[k, , drop = FALSE])

    cx <- cy <- r <- fill_op <- stroke_op <- rep(NA_real_, nframes)
    fill <- stroke <- rep(NA_character_, nframes)
    for (f in seq_len(nframes)) {
      if (!present[f]) {
        next
      }
      row <- frames[[f]][union$frame_index[[f]][k], , drop = FALSE]
      centre <- affine_xy(affine, row$x, row$y)
      cx[f] <- centre[, 1]
      cy[f] <- centre[, 2]
      r[f] <- point_radius_px(
        row$size,
        row$stroke,
        info[[k]]$factor,
        affine$res
      )
      paint <- point_paint(row)
      fill[f] <- paint$fill
      stroke[f] <- paint$stroke
      fill_op[f] <- paint$fill_opacity
      stroke_op[f] <- paint$stroke_opacity
    }

    xy <- if (is_circle) c("cx", "cy") else c("x", "y")
    tracks <- list()
    tracks[[xy[1]]] <- round_track(hold_absent(cx, present), precision)
    tracks[[xy[2]]] <- round_track(hold_absent(cy, present), precision)
    if (is_circle) {
      tracks$r <- round_track(hold_absent(r, present), precision)
    }
    # Animate only the channels the shape paints with. The default pch 19 draws
    # its disc *and* its outline in `colour`, so leaving `stroke` behind holds a
    # stale ring around a point whose fill has already tweened on; conversely,
    # tweening `fill` on an open pch would paint over its `none`. pch 21-25 take
    # their two channels from different aesthetics, so the opacities are tracked
    # separately.
    if (!is.na(channels$fill)) {
      tracks$fill <- hold_absent(fill, present)
      tracks[["fill-opacity"]] <- round_track(
        hold_absent(fill_op, present),
        precision
      )
    }
    if (!is.na(channels$stroke)) {
      tracks$stroke <- hold_absent(stroke, present)
      tracks[["stroke-opacity"]] <- round_track(
        hold_absent(stroke_op, present),
        precision
      )
    }
    tracks$opacity <- presence_opacity(present)

    tracks <- drop_constant_tracks(tracks, keep = "opacity"[!all(present)])
    list(id = ids[k], tracks = tracks)
  })
}

# --- point helpers ---------------------------------------------------------

# The layer's <use> point nodes in union order, gathered per panel.
point_nodes <- function(doc, layer_index, panels) {
  ordered_data_nodes(
    doc,
    select_point_nodes,
    layer_index,
    panels,
    "Point",
    "point"
  )
}

select_point_nodes <- function(doc, panel, layer_index) {
  panel_data_nodes(doc, "use", panel, layer_index)
}

# Per-element pch symbol resolution: circular pch inline to <circle>.
point_symbol_info <- function(union_data, symbols) {
  shapes <- as.integer(union_data$shape)
  lapply(shapes, function(sh) {
    sym <- symbols[[sprintf("gridSVG.pch%d", sh)]]
    if (!is.null(sym) && isTRUE(sym$circle)) {
      list(is_circle = TRUE, factor = sym$factor)
    } else {
      list(is_circle = FALSE, factor = NA_real_)
    }
  })
}

# SVG radius in user units. font-size = size * .pt + stroke * .stroke/2 (points);
# the device converts at res/72 (big points); factor = symbol r / viewBox width.
point_radius_px <- function(size, stroke, factor, res) {
  fontsize <- size * ggplot2::.pt + stroke * ggplot2::.stroke / 2
  factor * fontsize * res / 72
}

# The colours a point is painted with, plus each channel's opacity, matching
# how the device draws each pch: 0-14 are stroked in `colour` and unfilled,
# 15-18 are filled in `colour` with no outline, 19-20 are both, and 21-25 are
# stroked in `colour` and filled from the `fill` aesthetic. `NA` marks a channel
# the shape leaves at `none`.
point_paint <- function(row) {
  shape <- row$shape
  filled <- !is.na(shape) && shape >= 15L
  outlined <- is.na(shape) || shape < 15L || shape > 18L
  fill <- if (!filled) {
    NA_character_
  } else if (shape >= 21L) {
    row$fill
  } else {
    row$colour
  }
  stroke <- if (outlined) row$colour else NA_character_
  list(
    fill = to_hex(fill),
    stroke = to_hex(stroke),
    fill_opacity = paint_opacity(row$alpha, fill),
    stroke_opacity = paint_opacity(row$alpha, stroke)
  )
}
