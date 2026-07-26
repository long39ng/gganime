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
# of each aesthetic tuple, built from the discrete columns that are constant
# within every group, and only where that tuple is one-to-one with `group` in
# both frames.
# Where it is not (a `group` finer than the aesthetics), the boundary keeps
# tweenr's ids and the artefact stays.

#' Repair tweenr's `.id` across state boundaries
#'
#' @param frames A length-nframes list of per-frame data frames.
#' @param grouped Whether each element is a row-group (paths/polygons). Grouped
#'   layers only get the leading-frame repair below: pairing them across a state
#'   boundary needs vertex-level care, and their tuple is one-to-one with `group`
#'   only when every line differs in an aesthetic.
#' @param discrete Position columns that hold a discrete position, from
#'   `discrete_position_columns()`. These join the aesthetic tuple.
#' @return `frames` with `.id` rewritten on the frames that could be repaired.
#' @noRd
repair_frame_ids <- function(frames, grouped = FALSE, discrete = character()) {
  if (!frames_are_tweened(frames)) {
    return(frames)
  }
  repair_by_panel(frames, function(f) repair_panel_ids(f, grouped, discrete))
}

# gganimate tweens each panel separately (`Transition$expand_layer()` splits on
# PANEL), so the pairing to recover is per panel too: an aesthetic tuple can
# recur in another panel, and `.id` restarts there. Repair each panel's rows on
# their own and write `.id` back in place -- row order within a frame must not
# change, because `element_ids()` keys unidentified rows by their position and
# `union_data` row order becomes SVG document order.
repair_by_panel <- function(frames, repair) {
  panels <- unique(unlist(lapply(frames, function(d) as.character(d$PANEL))))
  if (length(panels) < 2L) {
    return(repair(frames))
  }
  for (panel in panels) {
    rows <- lapply(frames, function(d) which(as.character(d$PANEL) == panel))
    repaired <- repair(Map(function(d, i) d[i, , drop = FALSE], frames, rows))
    for (f in seq_along(frames)) {
      frames[[f]]$.id[rows[[f]]] <- repaired[[f]]$.id
    }
  }
  frames
}

# One panel's frames: relabel each block from the pairing its opening tween
# started from.
repair_panel_ids <- function(frames, grouped, discrete) {
  if (grouped) {
    return(repair_leading_group_ids(frames))
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
    pairs <- tuple_pairs(from, to, discrete)
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

# Leading held frames of a path or polygon layer are labelled per *vertex*.
# gganimate holds the first state with `keep_state()` before it ever calls
# `transform_path()`, and tweenr's `.get_last_frame()` mints `.id <- seq_len(nrow)`
# on data that has none yet -- one id per row, which for a grouped layer is one
# per vertex rather than one per element. transformr relabels per element from the
# first tween onward, and the two labellings then collide in the union: a
# 15-vertex line contributes keys 1..15 as single-vertex elements *and* keys 1..3
# as whole lines, and `polylineGrob` draws nothing for a one-vertex group, so the
# SVG holds fewer polylines than the union expects and `anime()` aborts.
#
# Those held frames are copies of the frame the first tween starts from, so take
# that frame's labelling.
repair_leading_group_ids <- function(frames) {
  labelled <- which(vapply(frames, is_element_labelled, logical(1)))
  if (length(labelled) == 0L || labelled[[1L]] == 1L) {
    return(frames)
  }
  ref <- frames[[labelled[[1L]]]]
  for (f in seq_len(labelled[[1L]] - 1L)) {
    if (holds_same_rows(frames[[f]], ref)) {
      frames[[f]]$.id <- ref$.id
    }
  }
  frames
}

# `group` delimits the elements of a grouped layer's frame, so a per-element
# `.id` is constant within each group.
is_element_labelled <- function(df) {
  nrow(df) > 0L &&
    all(vapply(
      split(df$.id, df$group),
      function(v) length(unique(v)) <= 1L,
      logical(1)
    ))
}

# The same rows in the same order, ignoring the columns tweenr and gganimate
# rewrite per frame.
holds_same_rows <- function(df, ref) {
  cols <- setdiff(names(ref), c(".id", ".phase", ".frame", "group"))
  nrow(df) == nrow(ref) &&
    all(cols %in% names(df)) &&
    isTRUE(all.equal(df[cols], ref[cols], check.attributes = FALSE))
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
    shifted <- !identical(ids[[f]], ids[[f - 1L]])
    if (shifted || tween_landed(frames[[f - 1L]], frames[[f]])) {
      starts <- c(starts, f)
    }
  }
  Map(seq.int, starts, c(starts[-1] - 1L, length(frames)))
}

# The labelling does not always shift at a boundary. tweenr reuses the `.id` an
# element vacated for one arriving at the same boundary, so a state that replaces
# one element with another keeps the same `.id` vector in every frame, and the two
# elements share a single union slot that interpolates between their positions
# over one frame interval. `.phase` marks the boundary independently: a tween
# lands on the first frame of unmodified data after an interpolated one.
tween_landed <- function(before, after) {
  !is.null(raw_frame(after)) && is.null(raw_frame(before))
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
tuple_pairs <- function(from, to, discrete = character()) {
  cols <- group_constant_columns(from, to, discrete)
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

# The columns that can stand in for `group`: the discrete ones, constant within
# every group of both frames. ggplot2 derives `group` from the interaction of a
# layer's discrete aesthetics, so a mapped colour or fill identifies an element.
# A continuous aesthetic (`size = x`) and every position hold values tweenr
# interpolates from one frame to the other, which no longer match. `discrete`
# adds back the positions whose scale is discrete, where the value is a
# category's mapped location and both frames hold the same one.
group_constant_columns <- function(from, to, discrete = character()) {
  cols <- setdiff(
    intersect(names(from), names(to)),
    c("PANEL", "group", ".id", ".phase", ".frame")
  )
  keeps <- function(col) {
    (col %in%
      discrete ||
      (is_discrete(from[[col]]) && is_discrete(to[[col]]))) &&
      is_group_constant(from, col) &&
      is_group_constant(to, col)
  }
  cols[vapply(cols, keeps, logical(1))]
}

# ggplot2's own test for a column it would group on.
is_discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
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
