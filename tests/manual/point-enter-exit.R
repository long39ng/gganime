# Render two geom_point animations with a genuine enter and exit as animated-SVG
# widgets, each with a gganimate gif of the same plot beside it. One point leaves
# and another arrives between the two states. The first plot fades and grows them
# in and out; the second keeps gganimate's defaults, where the two are simply
# drawn and undrawn at the state boundary and neither slides into the other's
# place.

devtools::load_all(".", quiet = TRUE)
suppressPackageStartupMessages({
  library(ggplot2)
})

df <- data.frame(
  id = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 6),
  state = rep(c("a", "b"), each = 5),
  x = c(1, 2, 3, 4, 5, 5, 4, 3, 2, 1),
  y = c(2, 4, 1, 5, 3, 5, 1, 4, 2, 3)
)

base <- ggplot(df, aes(x, y, group = id, colour = factor(id), size = x)) +
  geom_point() +
  labs(colour = "id", size = "x") +
  transition_states(state, transition_length = 2, state_length = 1)

render <- function(plot, name) {
  w <- anime(plot, nframes = 60, fps = 20, width = 640, height = 480)
  html <- normalizePath(
    file.path("tests", "manual", paste0(name, ".html")),
    mustWork = FALSE
  )
  htmlwidgets::saveWidget(w, html, selfcontained = TRUE)
  message("wrote ", html, " (", round(file.size(html) / 1024), " KB)")

  gif <- animate(
    plot,
    nframes = 60,
    fps = 20,
    width = 640,
    height = 480,
    renderer = gganimate::gifski_renderer()
  )
  path <- file.path("tests", "manual", paste0(name, ".gif"))
  gganimate::anim_save(path, gif)
  message("wrote ", normalizePath(path))
}

render(
  base + enter_fade() + enter_grow() + exit_fade() + exit_shrink(),
  "point-enter-exit"
)
render(base, "point-enter-exit-default")
