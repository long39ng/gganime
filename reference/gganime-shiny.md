# Shiny bindings for gganime

Output and render functions for using gganime widgets within Shiny
applications and interactive R Markdown documents.

## Usage

``` r
gganimeOutput(outputId, width = "100%", height = "400px")

renderGganime(expr, env = parent.frame(), quoted = FALSE)
```

## Arguments

- outputId:

  Output variable to read from.

- width, height:

  Must be a valid CSS unit (like `"100%"`, `"400px"`, `"auto"`) or a
  number, which will be coerced to a string and have `"px"` appended.

- expr:

  An expression that generates a gganime widget, typically a call to
  [`anime()`](https://long39ng.github.io/gganime/reference/anime.md).

- env:

  The environment in which to evaluate `expr`.

- quoted:

  Is `expr` a quoted expression (with
  [`quote()`](https://rdrr.io/r/base/substitute.html))? This is useful
  if you want to save an expression in a variable.

## Value

`gganimeOutput()` returns a Shiny output function for a UI definition;
`renderGganime()` returns a Shiny render function to assign to an output
slot.

## Examples

``` r
if (interactive() && rlang::is_installed("shiny")) {
  library(shiny)
  library(ggplot2)

  ui <- fluidPage(gganimeOutput("plot"))

  server <- function(input, output, session) {
    output$plot <- renderGganime({
      anime(
        ggplot(mtcars, aes(mpg, wt)) +
          geom_point() +
          transition_states(gear)
      )
    })
  }

  shinyApp(ui, server)
}
```
