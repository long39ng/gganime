# Repairing tweenr's element identity.
#
# `.id` is not a usable identity on the frame a tween lands on. `tween_state()`
# builds the arriving frame's ids in post-prune order and then assigns them to
# the unpruned rows, so the ids that continue land on the first k rows rather
# than on the k rows that actually continue (tweenr 2.0.3):
#
#   d1 <- data.frame(x = c(1, 2), g = c("a", "a"))
#   d2 <- data.frame(x = c(10, 20, 30), g = c("a", "b", "a"))
#   tween_state(keep_state(d1, 2), d2, "linear", 2, id = g)
#   # last frame: .id 1 -> x 10 (right), .id 2 -> x 20 (should be 30), NA -> x 30
#
# gganimate never reads `.id`, so its frame-by-frame output is unaffected;
# gganime uses it as the element identity, so a union slot teleports and
# cross-fades its colour across the one frame interval at each state boundary.
#
# The pairing tweenr meant to record is recoverable. It pairs the j-th row of a
# `group` in one state with the j-th row of the same `group` in the next. The
# group itself does not survive into the built frames -- gganimate renumbers it
# per (group, frame) in `finish_data()`, so the integers of two frames never
# overlap -- but the aesthetics that define it do. So pair on the j-th occurrence
# of each aesthetic tuple, built from the columns that are constant within every
# group, and only where that tuple is one-to-one with `group` in both frames.
# Where it is not (a `group` finer than the aesthetics), the boundary keeps
# tweenr's ids and the artefact stays.

#' Repair tweenr's `.id` across state boundaries
#'
#' @param frames A length-nframes list of per-frame data frames.
#' @param grouped Whether each element is a row-group (paths/polygons). Grouped
#'   layers are returned unchanged: their elements need vertex-level care, and
#'   the tuple is one-to-one with `group` only when every line differs in an
#'   aesthetic.
#' @return `frames` with `.id` rewritten on the frames that could be repaired.
#' @noRd
repair_frame_ids <- function(frames, grouped = FALSE) {
  if (grouped || !frames_are_tweened(frames)) {
    return(frames)
  }
  blocks <- id_blocks(frames)
  if (length(blocks) < 2L) {
    return(frames)
  }

  next_id <- max(unlist(lapply(frames, `[[`, ".id")), 0, na.rm = TRUE)
  for (b in seq_along(blocks)[-1]) {
    from <- last_raw_frame(frames[blocks[[b - 1L]]])
    to <- frames[[blocks[[b]][1L]]]
    if (is.null(from) || is.null(raw_frame(to))) {
      next
    }
    pairs <- tuple_pairs(from, to)
    if (is.null(pairs)) {
      next
    }
    ids <- from$.id[pairs]
    fresh <- which(is.na(ids))
    ids[fresh] <- next_id + seq_along(fresh)
    next_id <- next_id + length(fresh)
    # Every frame of the block carries the same `.id` vector, so the same
    # positional relabelling holds across all of them.
    for (f in blocks[[b]]) {
      frames[[f]]$.id <- ids
    }
  }
  frames
}

# The repair reads tweenr's own bookkeeping, so bail out unless every frame
# carries it (hand-built frames and untweened layers do not).
frames_are_tweened <- function(frames) {
  length(frames) > 1L &&
    all(vapply(
      frames,
      function(d) all(c(".id", ".phase", "group") %in% names(d)),
      logical(1)
    ))
}

# Maximal runs of frames that tweenr labelled alike. Within a run the `.id`
# vector is identical, so row position carries the identity; a new run starts on
# the frame a tween lands on, where the labelling shifts.
id_blocks <- function(frames) {
  ids <- lapply(frames, `[[`, ".id")
  starts <- 1L
  for (f in seq_along(frames)[-1]) {
    if (!identical(ids[[f]], ids[[f - 1L]])) {
      starts <- c(starts, f)
    }
  }
  Map(seq.int, starts, c(starts[-1] - 1L, length(frames)))
}

# A frame of unmodified data. tweenr's interpolated rows carry blended
# aesthetics, so a frame holding any of them cannot be paired on.
raw_frame <- function(df) {
  if (nrow(df) > 0L && all(df$.phase %in% c("static", "raw"))) df else NULL
}

# The raw frame the block's closing tween started from.
last_raw_frame <- function(block) {
  for (df in rev(block)) {
    raw <- raw_frame(df)
    if (!is.null(raw)) {
      return(raw)
    }
  }
  NULL
}

# The pairing tweenr recorded, as a `from` row per `to` row, or NULL when the
# aesthetic tuple cannot stand in for `group`.
tuple_pairs <- function(from, to) {
  cols <- group_constant_columns(from, to)
  if (length(cols) == 0L) {
    return(NULL)
  }
  from_tuple <- row_tuple(from, cols)
  to_tuple <- row_tuple(to, cols)
  if (
    !tuple_splits_groups(from_tuple, from$group) ||
      !tuple_splits_groups(to_tuple, to$group)
  ) {
    return(NULL)
  }
  match(
    paste(to_tuple, occurrence(to_tuple)),
    paste(from_tuple, occurrence(from_tuple))
  )
}

# Aesthetic columns constant within every group of both frames: only these can
# stand in for `group`. A continuous aesthetic (e.g. `size = hp`) varies within
# its group and drops out here, which is what keeps the tuple usable.
group_constant_columns <- function(from, to) {
  cols <- setdiff(
    intersect(names(from), names(to)),
    c("x", "y", "PANEL", "group", ".id", ".phase", ".frame")
  )
  cols[vapply(
    cols,
    function(col) is_group_constant(from, col) && is_group_constant(to, col),
    logical(1)
  )]
}

is_group_constant <- function(df, col) {
  all(vapply(
    split(df[[col]], df$group),
    function(v) length(unique(v)) <= 1L,
    logical(1)
  ))
}

row_tuple <- function(df, cols) {
  do.call(paste, c(lapply(df[cols], as.character), sep = "\r"))
}

# One tuple per group and one group per tuple. Only then does occurrence order
# within a tuple reproduce occurrence order within a group, which is what tweenr
# paired on.
tuple_splits_groups <- function(tuple, group) {
  n <- length(unique(group))
  length(unique(tuple)) == n && length(unique(paste(group, tuple))) == n
}

# 1, 2, 3, ... within each distinct value.
occurrence <- function(x) {
  stats::ave(seq_along(x), x, FUN = seq_along)
}
