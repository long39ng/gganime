# Render a gapminder-style bubble plot under transition_time with a per-frame
# title, subtitle, and static caption as an animated-SVG widget, and write a
# gganimate gif of the same plot for side-by-side comparison. The title year
# and subtitle counter swap in sync with the point animation; the caption holds.

devtools::load_all(".", quiet = TRUE)
suppressPackageStartupMessages({
  library(ggplot2)
})

set.seed(1)
years <- 2000:2009
df <- do.call(
  rbind,
  lapply(years, function(yr) {
    data.frame(
      year = yr,
      id = 1:6,
      x = (yr - 2000) + runif(6, 0, 2),
      y = sin((yr - 2000) / 2) * (1:6) + runif(6)
    )
  })
)

p <- ggplot(df, aes(x, y, group = id, colour = factor(id))) +
  geom_point(size = 5) +
  labs(
    title = "Year: {frame_time}",
    subtitle = "Frame {frame} of {nframes}",
    caption = "Source: simulated data",
    colour = "Series"
  ) +
  transition_time(year)

w <- anime(p, nframes = 60, fps = 20, width = 640, height = 480)

out <- normalizePath(
  file.path("tests", "manual", "label-transition-time.html"),
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
gifout <- file.path("tests", "manual", "label-transition-time.gif")
anim_save(gifout, gif)
message("wrote ", normalizePath(gifout))
