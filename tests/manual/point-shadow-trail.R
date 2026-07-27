# Render a geom_point scatter under transition_time with shadow_trail() as an
# animated-SVG widget, and write a gganimate gif of the same plot for
# side-by-side comparison. The live points move each frame while every fifth
# frame stays behind them as grey breadcrumbs, and `max_frames` keeps only the
# ten most recent ones, so the oldest crumb disappears as a new one is added.

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
  shadow_trail(
    distance = 0.05,
    max_frames = 10,
    colour = "grey70",
    size = 2
  )

w <- anime(p, nframes = 60, fps = 20, width = 640, height = 480)

out <- normalizePath(
  file.path("tests", "manual", "point-shadow-trail.html"),
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
gifout <- file.path("tests", "manual", "point-shadow-trail.gif")
gganimate::anim_save(gifout, gif)
message("wrote ", normalizePath(gifout))
