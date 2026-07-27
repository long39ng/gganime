# Render a bubble plot under transition_time with a two-line title and a
# three-line subtitle as an animated-SVG widget, and write a gganimate gif of the
# same plot for side-by-side comparison. The title's first line holds while its
# second counts the years; the subtitle's middle line is the only one of its
# three that changes.

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
    title = "Six series over ten years\nYear: {frame_time}",
    subtitle = "Simulated data\nFrame {frame} of {nframes}\nOne timeline",
    colour = "Series"
  ) +
  transition_time(year)

w <- anime(p, nframes = 60, fps = 20, width = 640, height = 480)

out <- normalizePath(
  file.path("tests", "manual", "label-multiline.html"),
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
gifout <- file.path("tests", "manual", "label-multiline.gif")
gganimate::anim_save(gifout, gif)
message("wrote ", normalizePath(gifout))
