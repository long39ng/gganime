# Render a geom_col bar chart under transition_states as an animated-SVG widget,
# and write a gganimate gif of the same plot for side-by-side comparison. Bars
# grow, shrink, and reposition as the values change between the two states.

devtools::load_all(".", quiet = TRUE)
suppressPackageStartupMessages({
  library(ggplot2)
})

df <- data.frame(
  cat = rep(c("A", "B", "C", "D"), 2),
  state = rep(c("before", "after"), each = 4),
  value = c(3, 5, 2, 6, 6, 1, 4, 2)
)

p <- ggplot(df, aes(cat, value, fill = cat)) +
  geom_col() +
  labs(x = NULL, y = "value", fill = NULL) +
  transition_states(state, transition_length = 2, state_length = 1)

w <- anime(p, nframes = 60, fps = 20, width = 640, height = 480)

out <- normalizePath(
  file.path("tests", "manual", "bar-transition-states.html"),
  mustWork = FALSE
)
htmlwidgets::saveWidget(w, out, selfcontained = TRUE)
message("wrote ", out, " (", round(file.size(out) / 1024), " KB)")

gif <- animate(
  p,
  nframes = 60,
  fps = 20,
  width = 640,
  height = 480,
  renderer = gifski_renderer()
)
gifout <- file.path("tests", "manual", "bar-transition-states.gif")
anim_save(gifout, gif)
message("wrote ", normalizePath(gifout))
