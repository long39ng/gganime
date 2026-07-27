# Per-frame title/subtitle/caption labels via animejs's anime_text().
#
# gganimate's Scene$set_labels() glue-interpolates the plot labels per frame
# ({frame_time}, {closest_state}, {frame_along}, frame/nframes/progress). This
# file precomputes each element's per-frame strings, keeps only the ones that
# vary, gives their SVG text nodes a data-animejs-id, and emits one anime_text()
# segment per varying line so the text swaps in sync with the scrubber.

# The label elements gganime animates. x/y/aesthetic labels are not per-frame.
label_element_names <- c("title", "subtitle", "caption")

# Per-frame strings for each present label element, with a static/dynamic flag.
# Reads gganimate's own Scene$set_labels(), so this is the one label-side touch
# of the gganimate boundary (mirrored by the geometry boundary in build.R).
precompute_labels <- function(built) {
  n <- built$scene$nframes
  out <- list()
  for (el in label_element_names) {
    values <- vapply(
      seq_len(n),
      function(i) {
        label_string(built$scene$set_labels(built, i)$plot$labels[[el]])
      },
      character(1)
    )
    if (all(is.na(values))) {
      next
    }
    out[[el]] <- list(values = values, dynamic = !label_is_static(values))
  }
  out
}

# Collapse a label (character, or an expression as returned by set_labels) to a
# single string. NA marks an absent label; a multi-line label keeps its newlines
# so label_lines() can split them back out per frame.
label_string <- function(label) {
  if (is.null(label) || length(label) == 0L) {
    return(NA_character_)
  }
  paste0(as.character(label), collapse = "\n")
}

label_is_static <- function(values) {
  all(values == values[[1]])
}

# The dynamic element names, in label_element_names order.
dynamic_label_names <- function(labels) {
  names(Filter(function(x) isTRUE(x$dynamic), labels))
}

# The lines of each frame's label, as a character matrix with one row per line
# and one column per frame. NULL when the frames do not agree on the number of
# lines: the tspans come from the reference render of frame 1, so a later frame
# has no node to put an extra line in.
label_lines <- function(values) {
  lines <- strsplit(values, "\n", fixed = TRUE)
  n <- lengths(lines)
  if (any(n != n[[1L]])) {
    return(NULL)
  }
  matrix(unlist(lines), nrow = n[[1L]])
}

# The data-animejs-id given to a label element, or to one line of a multi-line
# one.
label_id <- function(element, line = NULL) {
  if (is.null(line)) {
    paste0("label_", element)
  } else {
    paste0("label_", element, "_", line)
  }
}

# Locate a label element's SVG <text> node. gridSVG names the plot title,
# subtitle, and caption grobs plot.title / plot.subtitle / plot.caption (with a
# volatile counter suffix); the panel-external position keeps this unambiguous.
label_text_node <- function(doc, element) {
  xml2::xml_find_first(
    doc,
    sprintf(".//g[contains(@id, 'plot.%s.')]//text", element)
  )
}

# Give each dynamic label's text nodes a data-animejs-id and return the label
# animation elements (id + per-frame text values). One text swap writes one whole
# node, and grid puts each line of a label in its own tspan, so a multi-line
# label becomes one element per line. A line that never changes keeps the
# reference render's text and emits nothing.
annotate_labels <- function(doc, labels) {
  elements <- list()
  for (el in dynamic_label_names(labels)) {
    node <- label_text_node(doc, el)
    if (inherits(node, "xml_missing")) {
      next
    }
    tspans <- xml2::xml_find_all(node, "./tspan")
    lines <- label_lines(labels[[el]]$values)
    if (is.null(lines)) {
      cli::cli_warn(c(
        "{el} is frozen at its first-frame text.",
        i = "Its number of lines varies across frames."
      ))
      next
    }
    n_lines <- nrow(lines)
    if (n_lines != length(tspans)) {
      cli::cli_warn(c(
        "{el} is frozen at its first-frame text.",
        i = "It has {n_lines} line{?s} but the rendered label has {length(tspans)}."
      ))
      next
    }
    for (j in seq_len(n_lines)) {
      if (label_is_static(lines[j, ])) {
        next
      }
      id <- if (n_lines == 1L) label_id(el) else label_id(el, j)
      xml2::xml_set_attr(tspans[[j]], "data-animejs-id", id)
      elements[[length(elements) + 1L]] <- list(id = id, text = lines[j, ])
    }
  }
  elements
}
