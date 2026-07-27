# Point adapter. Generics dispatch on the geom's S3 class vector, so later geoms
# add methods without touching the pipeline.

# --- generics --------------------------------------------------------------

# Annotate a layer's SVG nodes: assign animation ids and, for a pch built from a
# single shape, inline the <use> as that shape so its geometry can tween.
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
  frozen <- vapply(info, symbol_is_frozen, logical(1))
  if (any(frozen)) {
    shapes <- sort(unique(as.integer(union_data$shape[frozen])))
    cli::cli_warn(c(
      "Point shapes drawn from several parts keep a fixed size.",
      i = "Affected pch: {.val {shapes}}. They animate in position and colour only."
    ))
  }
  for (k in seq_along(nodes)) {
    if (frozen[k]) {
      set_element_id(nodes[[k]], layer_index, ids[k])
    } else {
      inline_symbol(nodes[[k]], info[[k]], layer_index, ids[k])
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
  nframes <- nrow(union$presence)
  lapply(seq_len(ncol(union$presence)), function(k) {
    present <- union$presence[, k]
    affine <- affines[[k]]
    sym <- info[[k]]
    # The reference row decides which colour channels this element paints with;
    # `shape` is constant per element, so the channel set and the symbol are too.
    channels <- point_paint(union$union_data[k, , drop = FALSE])

    px <- py <- scale <- stroke_w <- rep(NA_real_, nframes)
    fill_op <- stroke_op <- rep(NA_real_, nframes)
    fill <- stroke <- rep(NA_character_, nframes)
    for (f in seq_len(nframes)) {
      if (!present[f]) {
        next
      }
      row <- frames[[f]][union$frame_index[[f]][k], , drop = FALSE]
      centre <- affine_xy(affine, row$x, row$y)
      px[f] <- centre[, 1]
      py[f] <- centre[, 2]
      scale[f] <- point_symbol_px(row$size, row$stroke, affine$res)
      stroke_w[f] <- point_stroke_px(row$stroke, affine$res)
      paint <- point_paint(row)
      fill[f] <- paint$fill
      stroke[f] <- paint$stroke
      fill_op[f] <- paint$fill_opacity
      stroke_op[f] <- paint$stroke_opacity
    }

    # The symbol's own kind fixes the geometry tracks: `cx`/`cy`/`r` for a disc,
    # `x`/`y`/`width`/`height` for a square, `points` for a triangle or diamond,
    # and the `<use>`'s `x`/`y` alone for a pch that stayed frozen.
    tracks <- lapply(
      symbol_geometry(sym, px, py, scale, precision),
      function(v) round_track(hold_absent(v, present), precision)
    )
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
      # Only an inlined shape carries its outline width in user units; a frozen
      # `<use>` keeps the pre-divided one gridSVG wrote, which is not ours to
      # tween.
      if (!symbol_is_frozen(sym)) {
        tracks[["stroke-width"]] <- round_track(
          hold_absent(stroke_w, present),
          precision
        )
      }
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

# The symbol each element is drawn from, in union order. A pch with no symbol in
# the export keeps its <use>.
point_symbol_info <- function(union_data, symbols) {
  shapes <- as.integer(union_data$shape)
  lapply(shapes, function(sh) {
    symbols[[sprintf("gridSVG.pch%d", sh)]] %||% frozen_symbol()
  })
}

# The symbol scale in SVG user units, which is the <use>'s width: font-size =
# size * .pt + stroke * .stroke/2 (points), converted at res/72 (big points). A
# normalised symbol coordinate multiplies by this.
point_symbol_px <- function(size, stroke, res) {
  (size * ggplot2::.pt + stroke * ggplot2::.stroke / 2) * res / 72
}

# Outline width in SVG user units. grid measures lwd in 1/96 inch and one user
# unit is 1/res inch, so the `stroke` aesthetic converts at res/96.
point_stroke_px <- function(stroke, res) {
  stroke * ggplot2::.stroke / 2 * res / 96
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
