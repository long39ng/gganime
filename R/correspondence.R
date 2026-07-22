# Element correspondence across frames. The set of present `.id`s varies frame
# to frame, so each layer's elements are placed on a stable union keyed by
# (PANEL, .id), with a presence bitmap recording which frames each appears in.

# Encode the (PANEL, .id) key. "\r" cannot occur in a factor label or an .id.
element_key <- function(df) {
  paste(df$PANEL, df$.id, sep = "\r")
}

#' Build the element union for one layer
#'
#' @param frames A length-nframes list of per-frame data frames, each with
#'   `PANEL`, `.id`, and aesthetic columns.
#' @return A list with:
#'   * `keys` — data frame (PANEL, .id, first_frame), one row per element,
#'     ordered by first appearance then PANEL then .id. This order is the SVG
#'     child order.
#'   * `union_data` — first-appearance row per element, in `keys` order.
#'   * `presence` — logical matrix, nframes x n_elements.
#'   * `frame_index` — per frame, a named vector mapping key to its row in that
#'     frame (for fast per-element lookup).
#' @noRd
union_elements <- function(frames) {
  nframes <- length(frames)

  meta <- new.env(parent = emptyenv())
  seen <- character(0)
  for (f in seq_len(nframes)) {
    df <- frames[[f]]
    if (is.null(df) || nrow(df) == 0L) {
      next
    }
    keys <- element_key(df)
    new <- keys[!keys %in% seen]
    if (length(new) == 0L) {
      next
    }
    for (k in new) {
      r <- match(k, keys)
      assign(
        k,
        list(first_frame = f, row = df[r, , drop = FALSE]),
        envir = meta
      )
    }
    seen <- c(seen, new)
  }

  first_frame <- vapply(seen, function(k) get(k, meta)$first_frame, integer(1))
  rows <- lapply(seen, function(k) get(k, meta)$row)
  panel <- vapply(rows, function(r) as.character(r$PANEL), character(1))
  id <- vapply(rows, function(r) as.numeric(r$.id), numeric(1))

  ord <- order(first_frame, panel, id)
  seen <- seen[ord]
  rows <- rows[ord]

  keys <- data.frame(
    PANEL = vapply(rows, function(r) as.character(r$PANEL), character(1)),
    .id = vapply(rows, function(r) as.numeric(r$.id), numeric(1)),
    first_frame = first_frame[ord],
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  union_data <- do.call(rbind, rows)
  rownames(union_data) <- NULL

  presence <- matrix(FALSE, nrow = nframes, ncol = length(seen))
  frame_index <- vector("list", nframes)
  for (f in seq_len(nframes)) {
    df <- frames[[f]]
    if (is.null(df) || nrow(df) == 0L) {
      frame_index[[f]] <- integer(0)
      next
    }
    fkeys <- element_key(df)
    idx <- match(seen, fkeys)
    presence[f, ] <- !is.na(idx)
    names(idx) <- seen
    frame_index[[f]] <- idx
  }

  list(
    keys = keys,
    union_data = union_data,
    presence = presence,
    frame_index = frame_index,
    order = seen
  )
}
