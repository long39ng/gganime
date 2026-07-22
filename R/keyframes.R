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
