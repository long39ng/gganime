# Shadow element synthesis. A shadow draws rows from other frames behind the
# current frame. `shadow_mark()` draws the raw (untweened) rows of earlier
# frames, of later frames, or of both. `shadow_trail()` draws every nth frame's
# rows and keeps the `max_frames` most recent samples, so a trail element is
# shown for a stretch of frames and then hidden again.
#
# Each shadow element is modelled as an extra element with *constant* geometry and
# a visibility-only (opacity) track. That reuses the whole pipeline: a shadow
# layer is described with the same `(union, frames)` shape the correspondence
# module produces for live elements, then run through the same geom adapter.
# Because the shadow row is looked up identically on every present frame, its
# geometry tracks come out constant (dropped by `drop_constant_tracks`), leaving
# only the presence-driven `opacity` track. The shadow rows are prepended to the
# layer's union order, so they render first -- behind the live elements -- with
# z-order frozen there. Mark and trail differ only in the visibility rule.

#' Build the shadow-element unions for every layer
#'
#' @param spec A `gganime_scene_spec`.
#' @return A list over layers. Each entry is `NULL` (no shadow for that layer) or
#'   a list with `union` (the correspondence-shaped list: `union_data`,
#'   `presence`, `frame_index`, `panels`) and `frames` (a length-nframes list feeding the
#'   adapter's per-frame lookup; the same styled shadow rows on every frame, since
#'   shadow geometry does not move).
#' @noRd
build_shadow_unions <- function(spec) {
  shadows <- spec$shadows
  empty <- vector("list", length(spec$layers))
  if (is.null(shadows)) {
    return(empty)
  }
  rule <- shadow_presence_rule(shadows$type, shadows$params)
  if (is.null(rule)) {
    return(empty)
  }
  lapply(seq_along(spec$layers), function(i) {
    raw <- shadows$raw[[i]]
    if (is.null(raw) || nrow(raw) == 0L) {
      return(NULL)
    }
    grouped <- geom_is_grouped(spec$layers[[i]]$geom_class)
    shadow_layer_union(raw, rule, spec$nframes, grouped)
  })
}

# A shadow type's visibility rule: a function of the elements' origin frames and
# the frame count, returning the nframes x n_elements bitmap. NULL where the
# shadow shows nothing, as `shadow_mark(past = FALSE, future = FALSE)` does.
shadow_presence_rule <- function(type, params) {
  switch(
    type,
    mark = if (params$past || params$future) {
      function(elem_frame, nframes) {
        shadow_presence(elem_frame, nframes, params$past, params$future)
      }
    },
    trail = function(elem_frame, nframes) {
      trail_presence(elem_frame, nframes, params$max_frames)
    }
  )
}

# One layer's shadow rows -> a synthetic (union, frames) pair. Single-mode geoms
# (points/rects) make one element per row; grouped geoms (paths/polygons) make one
# element per row-group, keyed by (.frame, group) -- a single frame can hold
# several groups (e.g. several lines). Elements that `presence_rule` never shows
# (the last state's rows under `shadow_mark(past = TRUE)`, say) are dropped,
# matching gganimate. Returns `NULL` if the layer contributes no visible shadow.
shadow_layer_union <- function(raw, presence_rule, nframes, grouped) {
  # Row indices per element, into `raw`, in element order. PANEL joins the
  # grouped key because ggplot2 numbers `group` across the whole layer, so two
  # panels can share a group integer.
  members <- if (grouped) {
    key <- paste(raw$.frame, raw$PANEL, raw$group, sep = "\r")
    unname(split(seq_len(nrow(raw)), factor(key, levels = unique(key))))
  } else {
    as.list(seq_len(nrow(raw)))
  }
  elem_frame <- vapply(members, function(ix) raw$.frame[ix[1L]], numeric(1))

  presence <- presence_rule(elem_frame, nframes)
  visible <- apply(presence, 2L, any)
  if (!any(visible)) {
    return(NULL)
  }
  members <- members[visible]
  presence <- presence[, visible, drop = FALSE]

  # Reference rows: the visible elements' shadow rows, in element order. Grouped geoms
  # identify a polyline/polygon by its `group` integer, so it is reassigned to
  # the element index; single-mode geoms draw one node per row and ignore it.
  row_idx <- unlist(members)
  union_data <- raw[row_idx, , drop = FALSE]
  if (grouped) {
    union_data$group <- rep(seq_along(members), lengths(members))
  }
  rownames(union_data) <- NULL

  # Geometry is looked up from the same shadow rows on every frame, so it comes out
  # constant; only the presence-driven opacity track varies. Single-mode
  # adapters index `frame_index[[f]]` with `[k]` (an integer vector, element k ->
  # its row); grouped adapters use `[[k]]` (a list, element k -> its vertex rows).
  per_frame <- if (grouped) members else row_idx
  frame_index <- rep(list(per_frame), nframes)
  frames <- rep(list(raw), nframes)

  union <- list(
    union_data = union_data,
    presence = presence,
    frame_index = frame_index,
    panels = as.character(raw$PANEL[vapply(members, `[[`, integer(1), 1L)])
  )
  list(union = union, frames = frames)
}

# Prepend a layer's shadow rows to its live union data, so the reference gtable
# draws a grob per shadow mark (of the same node type as the live geom) first --
# behind the live elements and first in SVG child order. Grouped geoms identify
# a polyline/polygon by its `group` integer, so the live groups are shifted past
# the shadow ones to keep every element's node separate.
combine_shadow_union_data <- function(shadow, live_union_data, grouped) {
  if (is.null(shadow)) {
    return(live_union_data)
  }
  if (grouped) {
    live_union_data$group <- live_union_data$group + ncol(shadow$union$presence)
  }
  rbind_fill(shadow$union$union_data, live_union_data)
}

# rbind two data frames with differing columns, filling absent columns with NA.
# Shadow rows and live tweened rows share every aesthetic column (both are fully
# built layer data) and differ only in columns the geom does not read, such as the
# shadow rows' `.frame`.
rbind_fill <- function(a, b) {
  if (is.null(a) || nrow(a) == 0L) {
    return(b)
  }
  if (is.null(b) || nrow(b) == 0L) {
    return(a)
  }
  cols <- union(names(a), names(b))
  for (col in setdiff(cols, names(a))) {
    a[[col]] <- NA
  }
  for (col in setdiff(cols, names(b))) {
    b[[col]] <- NA
  }
  rbind(a[cols], b[cols])
}

# Visibility bitmap: nframes x n_elements. A mark whose origin is frame
# `elem_frame[k]` is drawn on frame `f` per gganimate's ShadowMark rule --
# `past && future` shows every other frame, `past` earlier frames, `future`
# later frames. Mirrors ShadowMark$prepare_frame_data (gganimate, MIT).
shadow_presence <- function(elem_frame, nframes, past, future) {
  outer(seq_len(nframes), elem_frame, function(f, origin) {
    if (past && future) {
      origin != f
    } else if (past) {
      origin < f
    } else {
      origin > f
    }
  })
}

# Visibility bitmap for a trail: nframes x n_elements. A trail element is drawn on
# every frame after the one it was sampled from, until `max_frames` newer samples
# have accumulated in front of it. The window counts frames, not elements, so every
# element sampled from one frame drops out together. Mirrors
# ShadowTrail$prepare_frame_data (gganimate, MIT).
trail_presence <- function(elem_frame, nframes, max_frames) {
  presence <- matrix(FALSE, nrow = nframes, ncol = length(elem_frame))
  for (f in seq_len(nframes)) {
    shown <- elem_frame < f
    origins <- unique(elem_frame[shown])
    if (max_frames < length(origins)) {
      oldest <- sort(origins, decreasing = TRUE)[max_frames]
      shown <- shown & elem_frame >= oldest
    }
    presence[f, ] <- shown
  }
  presence
}
