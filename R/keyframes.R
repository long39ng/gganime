# Per-attribute keyframe tracks. A track is a length-nframes vector of values.
# These helpers handle absence (hold at the nearest present frame), presence
# opacity, rounding, and dropping tracks that never change.

# Fill absent frames by holding the value from the nearest present frame.
# Ties resolve to the earlier frame, so a point holds its entry geometry before
# it appears and its exit geometry after it leaves.
hold_absent <- function(values, present) {
  if (all(present) || !any(present)) {
    return(values)
  }
  idx <- which(present)
  nearest <- vapply(
    seq_along(present),
    function(i) idx[which.min(abs(idx - i))],
    integer(1)
  )
  values[!present] <- values[nearest][!present]
  values
}

# Presence channel: 1 where the element is drawn, 0 where it is absent.
presence_opacity <- function(present) {
  as.numeric(present)
}

round_track <- function(values, precision) {
  if (is.numeric(values)) round(values, precision) else values
}

# Vertex-count normalisation for paths/polygons. A single element's vertex
# count varies across frames (transformr resamples only within a morph, and a
# reveal grows the line), but a `points` string can only tween between equal
# arities. Pad every present frame to the element's max arity by repeating its
# last vertex; the padding rides at the tail and unfolds one vertex per frame.
#
# `verts` is a length-nframes list; each entry is a 2-column (x, y) matrix for a
# present frame or `NULL` for an absent one. Absent frames stay `NULL`.
normalise_vertices <- function(verts) {
  arity <- max(vapply(
    verts,
    function(m) if (is.null(m)) 0L else nrow(m),
    integer(1)
  ))
  lapply(verts, function(m) {
    if (is.null(m) || nrow(m) == 0L) {
      return(NULL)
    }
    if (nrow(m) < arity) {
      m <- rbind(m, m[rep(nrow(m), arity - nrow(m)), , drop = FALSE])
    }
    m
  })
}

# Format a 2-column vertex matrix as an SVG `points` string, rounded.
vertices_to_points <- function(m, precision) {
  paste0(
    round(m[, 1], precision),
    ",",
    round(m[, 2], precision),
    collapse = " "
  )
}

# A track that never changes can be left on the reference element and dropped
# from the animation.
track_is_constant <- function(values) {
  length(values) <= 1L || all(values == values[[1]])
}

# Drop constant tracks from a named list of tracks. `keep` names are retained
# even when constant (e.g. the presence channel when an element is ever absent).
drop_constant_tracks <- function(tracks, keep = character(0)) {
  varying <- vapply(tracks, function(v) !track_is_constant(v), logical(1))
  tracks[varying | names(tracks) %in% keep]
}
