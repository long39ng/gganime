# The widget layer: render the timeline + SVG through animejs, and print.

#' Render the timeline and SVG to a gganime widget
#'
#' @param timeline An `anime_timeline` with playback set.
#' @param svg The finalised SVG string.
#' @param width,height Widget dimensions in pixels, or `NULL`.
#' @param elementId Optional widget element id.
#' @return An `htmlwidget` of class `gganime`.
#' @noRd
gganime_widget <- function(timeline, svg, width, height, elementId) {
  w <- animejs::anime_render(
    timeline,
    svg = svg,
    width = width,
    height = height,
    elementId = elementId
  )
  # htmlwidgets uses class(w)[1] as the JS binding name, so keep animejs's name
  # first and insert "gganime" ahead of "htmlwidget" for print dispatch.
  class(w) <- append(class(w), "gganime", after = length(class(w)) - 1L)
  w
}

#' @export
print.gganime <- function(x, ...) {
  NextMethod()
}

#' @exportS3Method knitr::knit_print
knit_print.gganime <- function(x, ...) {
  knitr::knit_print(structure(x, class = setdiff(class(x), "gganime")), ...)
}
