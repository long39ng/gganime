# Render a three-layer plot -- an area, a line over it, and points on top -- as an
# animated-SVG widget, and write a gganimate gif of the same plot for
# side-by-side comparison. GeomRibbon draws an invisible outline polyline beside
# its polygon, so this is the case that used to be indistinguishable from the
# line layer.
#
# Note when comparing: gganime's realised frame count is nframes + 1 and gifs
# drop duplicate frames, so gif frame k is not widget frame k. Align on frame 1
# or on a labelled hold, not on an index.

devtools::load_all(".", quiet = TRUE)
suppressPackageStartupMessages({
  library(ggplot2)
})

aq <- airquality[!is.na(airquality$Temp), ]
aq <- aq[aq$Month %in% c(5, 6, 7), ]

p <- ggplot(aq, aes(Day, Temp)) +
  geom_area(fill = "#9ecae1") +
  geom_line(linewidth = 1, colour = "#08519c") +
  geom_point(size = 2.5, colour = "#a50f15") +
  labs(
    title = "Month {closest_state}",
    x = "Day",
    y = "Temp (F)"
  ) +
  transition_states(Month, transition_length = 2, state_length = 1)

w <- anime(p, nframes = 60, fps = 20, width = 640, height = 480)

out <- normalizePath(
  file.path("tests", "manual", "area-line-layers.html"),
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
gifout <- file.path("tests", "manual", "area-line-layers.gif")
gganimate::anim_save(gifout, gif)
message("wrote ", normalizePath(gifout))
