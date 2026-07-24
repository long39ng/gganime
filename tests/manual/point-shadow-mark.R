# Render a geom_point scatter under transition_time with shadow_mark(past) as an
# animated-SVG widget, and write a gganimate gif of the same plot for
# side-by-side comparison. The live points move each frame while the raw data of
# earlier frames stays behind them as grey marks, accumulating as time advances.

devtools::load_all(".", quiet = TRUE)
suppressPackageStartupMessages({
  library(ggplot2)
})

set.seed(1)
n <- 8
times <- 1:6
df <- do.call(
  rbind,
  lapply(times, function(t) {
    data.frame(
      id = seq_len(n),
      time = t,
      x = t + rnorm(n, sd = 0.3),
      y = seq_len(n) + sin(t) + rnorm(n, sd = 0.2)
    )
  })
)

p <- ggplot(df, aes(x, y, colour = factor(id), group = id)) +
  geom_point(size = 4) +
  labs(x = "x", y = "y", colour = "id") +
  transition_time(time) +
  shadow_mark(colour = "grey70", size = 2)

w <- anime(p, nframes = 60, fps = 20, width = 640, height = 480)

out <- normalizePath(
  file.path("tests", "manual", "point-shadow-mark.html"),
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
gifout <- file.path("tests", "manual", "point-shadow-mark.gif")
anim_save(gifout, gif)
message("wrote ", normalizePath(gifout))
