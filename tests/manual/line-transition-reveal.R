# Render an airquality temperature line under transition_reveal as an
# animated-SVG widget, and write a gganimate gif of the same plot for
# side-by-side comparison. Each month's line draws in progressively along Day.

devtools::load_all(".", quiet = TRUE)
suppressPackageStartupMessages({
  library(ggplot2)
})

aq <- airquality[!is.na(airquality$Temp), ]

p <- ggplot(aq, aes(Day, Temp, colour = factor(Month))) +
  geom_line(linewidth = 1) +
  labs(x = "Day", y = "Temp (F)", colour = "Month") +
  transition_reveal(Day)

w <- anime(p, nframes = 60, fps = 20, width = 640, height = 480)

out <- normalizePath(
  file.path("tests", "manual", "line-transition-reveal.html"),
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
gifout <- file.path("tests", "manual", "line-transition-reveal.gif")
gganimate::anim_save(gifout, gif)
message("wrote ", normalizePath(gifout))
