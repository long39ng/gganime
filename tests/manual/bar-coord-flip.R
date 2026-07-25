# Render a geom_col chart under coord_flip and transition_states as an
# animated-SVG widget, and write a gganimate gif of the same plot for
# side-by-side comparison. A flip swaps which data column drives which screen
# axis, so getting it wrong is visible immediately: bars would grow upward from
# the wrong baseline, or leave the panel entirely.

devtools::load_all(".", quiet = TRUE)
suppressPackageStartupMessages({
  library(ggplot2)
})

df <- data.frame(
  country = rep(c("Denmark", "Estonia", "Finland", "Germany"), 3),
  state = rep(c("2000", "2010", "2020"), each = 4),
  value = c(30, 55, 20, 70, 60, 25, 45, 35, 20, 70, 65, 50)
)

p <- ggplot(df, aes(country, value, fill = country)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "state {closest_state}", x = NULL, y = "value") +
  transition_states(state, transition_length = 2, state_length = 1)

w <- anime(p, nframes = 60, fps = 20, width = 700, height = 450)

out <- normalizePath(
  file.path("tests", "manual", "bar-coord-flip.html"),
  mustWork = FALSE
)
htmlwidgets::saveWidget(w, out, selfcontained = TRUE)
message("wrote ", out, " (", round(file.size(out) / 1024), " KB)")

gif <- animate(
  p,
  nframes = 60,
  fps = 20,
  width = 700,
  height = 450,
  renderer = gganimate::gifski_renderer()
)
gifout <- file.path("tests", "manual", "bar-coord-flip.gif")
gganimate::anim_save(gifout, gif)
message("wrote ", normalizePath(gifout))
