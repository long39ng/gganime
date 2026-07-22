# Element correspondence across frames. The set of present `.id`s varies frame
# to frame, so each layer's elements are placed on a stable union keyed by
# (PANEL, .id), with a presence bitmap recording which frames each appears in.
#
# Points and rects are one row per element; paths and polygons are a row-group
# (one row per vertex sharing the key). `grouped = TRUE` switches `union_data`
# and `frame_index` to carry every row of a key rather than just the first.

# Encode the (PANEL, .id) key. "\r" cannot occur in a factor label or an .id.
element_key <- function(df) {
  paste(df$PANEL, df$.id, sep = "\r")
}

#' Build the element union for one layer
#'
#' @param frames A length-nframes list of per-frame data frames, each with
#'   `PANEL`, `.id`, and aesthetic columns.
#' @param grouped Whether each element is a row-group (paths/polygons) rather
#'   than a single row (points/rects).
#' @return A list with:
#'   * `keys` -- data frame (PANEL, .id, first_frame), one row per element,
#'     ordered by first appearance then PANEL then .id. This order is the SVG
#'     child order.
#'   * `union_data` -- the reference rows per element, in `keys` order. Single
#'     mode: the first-appearance row. Grouped mode: the max-arity present
#'     frame's row-group, with `group` reassigned to a unique integer per
#'     element so the reference draws one polyline/polygon per element.
#'   * `presence` -- logical matrix, nframes x n_elements.
#'   * `frame_index` -- per frame, element-ordered lookup into that frame's rows.
#'     Single mode: an integer vector (one row per element). Grouped mode: a
#'     list of integer vectors (the vertex rows of each element).
#' @noRd
union_elements <- function(frames, grouped = FALSE) {
  nframes <- length(frames)

  # Forward pass: keys in first-appearance order, with the frame each first
  # appears in. A key repeats across a frame's rows in grouped mode.
  seen <- character(0)
  first_frame <- integer(0)
  for (f in seq_len(nframes)) {
    df <- frames[[f]]
    if (is.null(df) || nrow(df) == 0L) {
      next
    }
    new <- unique(element_key(df))
    new <- new[!new %in% seen]
    if (length(new) == 0L) {
      next
    }
    seen <- c(seen, new)
    first_frame <- c(first_frame, rep.int(f, length(new)))
  }

  parts <- strsplit(seen, "\r", fixed = TRUE)
  panel <- vapply(parts, `[[`, character(1), 1L)
  id <- vapply(parts, function(p) as.numeric(p[[2]]), numeric(1))

  ord <- order(first_frame, panel, id)
  seen <- seen[ord]
  first_frame <- first_frame[ord]

  keys <- data.frame(
    PANEL = panel[ord],
    .id = id[ord],
    first_frame = first_frame,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  presence <- matrix(FALSE, nrow = nframes, ncol = length(seen))
  frame_index <- vector("list", nframes)
  for (f in seq_len(nframes)) {
    df <- frames[[f]]
    fkeys <- if (is.null(df) || nrow(df) == 0L) {
      character(0)
    } else {
      element_key(df)
    }
    if (grouped) {
      idx <- lapply(seen, function(k) which(fkeys == k))
      presence[f, ] <- lengths(idx) > 0L
    } else {
      idx <- match(seen, fkeys)
      presence[f, ] <- !is.na(idx)
    }
    names(idx) <- seen
    frame_index[[f]] <- idx
  }

  union_data <- if (grouped) {
    grouped_union_data(frames, seen, presence, frame_index)
  } else {
    single_union_data(frames, seen, first_frame, frame_index)
  }

  list(
    keys = keys,
    union_data = union_data,
    presence = presence,
    frame_index = frame_index,
    order = seen
  )
}

# First-appearance row per element, in `keys` order.
single_union_data <- function(frames, seen, first_frame, frame_index) {
  rows <- lapply(seq_along(seen), function(k) {
    f <- first_frame[k]
    frames[[f]][frame_index[[f]][k], , drop = FALSE]
  })
  ud <- do.call(rbind, rows)
  rownames(ud) <- NULL
  ud
}

# The max-arity present frame's row-group per element, `group` reassigned to a
# unique integer per element (frame `group` integers are not stable across
# frames). The max-arity frame guarantees >= 2 vertices so GeomPath draws it.
grouped_union_data <- function(frames, seen, presence, frame_index) {
  rows <- lapply(seq_along(seen), function(k) {
    present_f <- which(presence[, k])
    counts <- vapply(
      present_f,
      function(f) length(frame_index[[f]][[k]]),
      integer(1)
    )
    f <- present_f[which.max(counts)]
    grp <- frames[[f]][frame_index[[f]][[k]], , drop = FALSE]
    grp$group <- k
    grp
  })
  ud <- do.call(rbind, rows)
  rownames(ud) <- NULL
  ud
}
