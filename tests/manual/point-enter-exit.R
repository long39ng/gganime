# Render a geom_point animation with a genuine enter and exit as an animated-SVG
# widget. One point leaves and another arrives between the two states, so the
# output exercises absence/hold, presence opacity, and enter/exit fade + grow.

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

p <- ggplot(df, aes(x, y, group = id, colour = factor(id), size = x)) +
  geom_point() +
  labs(colour = "id", size = "x") +
  transition_states(state, transition_length = 2, state_length = 1) +
  enter_fade() +
  enter_grow() +
  exit_fade() +
  exit_shrink()

w <- anime(p, nframes = 60, fps = 20, width = 640, height = 480)

out <- normalizePath(
  file.path("tests", "manual", "point-enter-exit.html"),
  mustWork = FALSE
)
htmlwidgets::saveWidget(w, out, selfcontained = TRUE)
message("wrote ", out, " (", round(file.size(out) / 1024), " KB)")
