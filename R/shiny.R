# Shiny bindings: thin wrappers over animejs's, since a gganime widget is an
# animejs widget (its JS binding name is "animejs").

#' Shiny bindings for gganime
#'
#' Output and render functions for using gganime widgets within Shiny
#' applications and interactive R Markdown documents.
#'
#' @param outputId Output variable to read from.
#' @param width,height Must be a valid CSS unit (like `"100%"`, `"400px"`,
#'   `"auto"`) or a number, which will be coerced to a string and have `"px"`
#'   appended.
#' @param expr An expression that generates a gganime widget, typically a call
#'   to [anime()].
#' @param env The environment in which to evaluate `expr`.
#' @param quoted Is `expr` a quoted expression (with `quote()`)? This is useful
#'   if you want to save an expression in a variable.
#'
#' @return `gganimeOutput()` returns a Shiny output function for a UI
#'   definition; `renderGganime()` returns a Shiny render function to assign to
#'   an output slot.
#'
#' @examples
#' if (interactive() && rlang::is_installed("shiny")) {
#'   library(shiny)
#'   library(ggplot2)
#'   library(gganimate)
#'
#'   ui <- fluidPage(gganimeOutput("plot"))
#'
#'   server <- function(input, output, session) {
#'     output$plot <- renderGganime({
#'       anime(
#'         ggplot(mtcars, aes(mpg, wt)) +
#'           geom_point() +
#'           transition_states(gear)
#'       )
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
#' @name gganime-shiny
NULL

#' @rdname gganime-shiny
#' @export
gganimeOutput <- function(outputId, width = "100%", height = "400px") {
  animejs::animejsOutput(outputId, width = width, height = height)
}

#' @rdname gganime-shiny
#' @export
renderGganime <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  }
  animejs::renderAnimejs(expr, env = env, quoted = TRUE)
}
