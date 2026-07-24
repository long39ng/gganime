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
  ...
) {
  nodes <- point_nodes(doc)
  if (length(nodes) != length(ids)) {
    cli::cli_abort(c(
      "Point element count does not match the union.",
      x = "Found {length(nodes)} SVG point{?s} but expected {length(ids)}."
    ))
  }
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
  affine,
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
    is_circle <- info[[k]]$is_circle
    # The reference row decides which colour channels this element paints with;
    # `shape` is constant per element, so the channel set is too.
    channels <- point_paint(union$union_data[k, , drop = FALSE])

    cx <- cy <- r <- alpha <- rep(NA_real_, nframes)
    fill <- stroke <- rep(NA_character_, nframes)
    for (f in seq_len(nframes)) {
      if (!present[f]) {
        next
      }
      row <- frames[[f]][union$frame_index[[f]][k], , drop = FALSE]
      cx[f] <- affine$to_svg_x(row$x)
      cy[f] <- affine$to_svg_y(row$y)
      r[f] <- point_radius_px(
        row$size,
        row$stroke,
        info[[k]]$factor,
        affine$res
      )
      paint <- point_paint(row)
      fill[f] <- paint$fill
      stroke[f] <- paint$stroke
      alpha[f] <- if (is.na(row$alpha)) 1 else row$alpha
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
    # tweening `fill` on an open pch would paint over its `none`.
    opacity_track <- round_track(hold_absent(alpha, present), precision)
    if (!is.na(channels$fill)) {
      tracks$fill <- hold_absent(fill, present)
      tracks[["fill-opacity"]] <- opacity_track
    }
    if (!is.na(channels$stroke)) {
      tracks$stroke <- hold_absent(stroke, present)
      tracks[["stroke-opacity"]] <- opacity_track
    }
    tracks$opacity <- presence_opacity(present)

    tracks <- drop_constant_tracks(tracks, keep = "opacity"[!all(present)])
    list(id = ids[k], tracks = tracks)
  })
}

# --- point helpers ---------------------------------------------------------

# Ordered <use> point nodes for the (single) point layer, in document order.
point_nodes <- function(doc) {
  xml2::xml_find_all(doc, ".//use[starts-with(@id,'geom_point')]")
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

# The colours a point is painted with, matching how the device draws each pch:
# 0-14 are stroked in `colour` and unfilled, 15-18 are filled in `colour` with
# no outline, 19-20 are both, and 21-25 are stroked in `colour` and filled from
# the `fill` aesthetic. `NA` marks a channel the shape leaves at `none`.
point_paint <- function(row) {
  shape <- row$shape
  filled <- !is.na(shape) && shape >= 15L
  outlined <- is.na(shape) || shape < 15L || shape > 18L
  fill <- if (!filled) {
    NA
  } else if (shape >= 21L) {
    row$fill
  } else {
    row$colour
  }
  list(
    fill = hex_or_na(fill),
    stroke = if (outlined) hex_or_na(row$colour) else NA_character_
  )
}

hex_or_na <- function(x) {
  if (is.na(x)) NA_character_ else to_hex(x)
}

to_hex <- function(x) {
  m <- grDevices::col2rgb(x)
  grDevices::rgb(m[1, ], m[2, ], m[3, ], maxColorValue = 255)
}
