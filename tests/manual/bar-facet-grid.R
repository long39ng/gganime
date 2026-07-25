# Render a faceted geom_col chart under transition_states as an animated-SVG
# widget, and write a gganimate gif of the same plot for side-by-side comparison.
# facet_grid draws its panels column-major while PANEL numbers row-major, so this
# is the case where mis-attributing a node to a panel would be visible: a bar
# would grow in the wrong panel.

devtools::load_all(".", quiet = TRUE)
suppressPackageStartupMessages({
  library(ggplot2)
})

set.seed(1)
df <- expand.grid(
  cat = c("p", "q", "r"),
  row = c("r1", "r2"),
  col = c("c1", "c2", "c3"),
  state = 1:3,
  stringsAsFactors = FALSE
)
df$v <- runif(nrow(df), 1, 10)

p <- ggplot(df, aes(cat, v, fill = cat)) +
  geom_col() +
  facet_grid(row ~ col) +
  labs(title = "state {closest_state}", x = NULL, y = "value") +
  transition_states(state, transition_length = 2, state_length = 1)

w <- anime(p, nframes = 60, fps = 20, width = 800, height = 500)

out <- normalizePath(
  file.path("tests", "manual", "bar-facet-grid.html"),
  mustWork = FALSE
)
htmlwidgets::saveWidget(w, out, selfcontained = TRUE)
message("wrote ", out, " (", round(file.size(out) / 1024), " KB)")

gif <- animate(
  p,
  nframes = 60,
  fps = 20,
  width = 800,
  height = 500,
  renderer = gganimate::gifski_renderer()
)
gifout <- file.path("tests", "manual", "bar-facet-grid.gif")
gganimate::anim_save(gifout, gif)
message("wrote ", normalizePath(gifout))
