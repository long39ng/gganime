# Per-frame title/subtitle/caption labels via animejs's anime_text().
#
# gganimate's Scene$set_labels() glue-interpolates the plot labels per frame
# ({frame_time}, {closest_state}, {frame_along}, frame/nframes/progress). This
# file precomputes each element's per-frame strings, keeps only the ones that
# vary, gives their SVG text elements a data-animejs-id, and emits one
# anime_text() segment per varying element so the text swaps in sync with the
# scrubber.

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
# so the SVG-side tspan count can detect it later.
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

# The data-animejs-id given to a label element.
label_id <- function(element) {
  paste0("label_", element)
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

# Give each dynamic label's text element a data-animejs-id and return the label
# animation elements (id + per-frame text values). A multi-line label cannot be
# driven by one text swap (v0.1 limit), so it is frozen at its first-frame text
# with a warning and emits no segment.
annotate_labels <- function(doc, labels) {
  elements <- list()
  for (el in dynamic_label_names(labels)) {
    node <- label_text_node(doc, el)
    if (inherits(node, "xml_missing")) {
      next
    }
    tspans <- xml2::xml_find_all(node, "./tspan")
    if (length(tspans) != 1L) {
      cli::cli_warn(c(
        "Multi-line {el} is frozen at its first-frame text.",
        i = "A single text swap cannot drive a multi-line label."
      ))
      next
    }
    id <- label_id(el)
    xml2::xml_set_attr(tspans[[1]], "data-animejs-id", id)
    elements[[length(elements) + 1L]] <- list(
      id = id,
      text = labels[[el]]$values
    )
  }
  elements
}
