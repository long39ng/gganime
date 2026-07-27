# Render a geom_point scatter whose points are drawn with several pch and a
# mapped size as an animated-SVG widget, and write a gganimate gif of the same
# plot for side-by-side comparison. Squares, triangles and diamonds should grow
# and shrink with the circles, and each outline should thicken with its mapped
# stroke; the crossed square holds its size.

devtools::load_all(".", quiet = TRUE)
suppressPackageStartupMessages({
  library(ggplot2)
})

set.seed(1)
shapes <- c(19, 22, 24, 23, 7)
states <- letters[1:4]
df <- do.call(
  rbind,
  lapply(seq_along(states), function(s) {
    data.frame(
      id = seq_along(shapes),
      state = states[s],
      x = seq_along(shapes) + rnorm(length(shapes), sd = 0.4),
      y = rev(seq_along(shapes)) + rnorm(length(shapes), sd = 0.4),
      size = runif(length(shapes), 3, 9),
      stroke = runif(length(shapes), 0.5, 3)
    )
  })
)

p <- ggplot(
  df,
  aes(x, y, group = id, size = size, stroke = stroke, shape = factor(id))
) +
  geom_point(colour = "firebrick", fill = "steelblue") +
  scale_shape_manual(values = shapes) +
  scale_size_identity() +
  labs(title = "{closest_state}", shape = "id") +
  transition_states(state, transition_length = 2, state_length = 1)

w <- anime(p, nframes = 60, fps = 20, width = 640, height = 480)

out <- normalizePath(
  file.path("tests", "manual", "point-shapes.html"),
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
  renderer = gganimate::gifski_renderer()
)
gifout <- file.path("tests", "manual", "point-shapes.gif")
gganimate::anim_save(gifout, gif)
message("wrote ", normalizePath(gifout))
