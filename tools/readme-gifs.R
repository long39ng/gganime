# Capture each README example widget to an animated GIF in man/figures/.

library(ggplot2)
library(gapminder)
devtools::load_all(".", quiet = TRUE)

# Render a plot to a gganime widget, then step its timeline in a headless
# browser and snapshot each frame into an animated GIF.
capture_gif <- function(
  plot,
  file,
  nframes = 60,
  gif_fps = 10,
  width = 640,
  height = 480
) {
  widget <- anime(plot, nframes = nframes, width = width, height = height)

  html <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(widget, html, selfcontained = TRUE)
  on.exit(unlink(html), add = TRUE)

  b <- chromote::ChromoteSession$new(width = width + 120, height = height + 200)
  on.exit(b$close(), add = TRUE)

  # Subscribe to the load event before navigating so it cannot fire in the gap
  # before the wait.
  b$Page$enable()
  loaded <- b$Page$loadEventFired(wait_ = FALSE)
  b$Page$navigate(paste0("file://", html), wait_ = FALSE)
  b$wait_for(loaded)
  Sys.sleep(1)

  seek <- function(fraction) {
    b$Runtime$evaluate(sprintf(
      "(function(){
         var s = document.querySelector('.animejs-controls input[type=range]');
         s.value = %f;
         s.dispatchEvent(new Event('input'));
       })()",
      fraction * 1000
    ))
  }

  # Pause autoplay (the first click stops a playing instance) and hide the
  # control bar. Seeking still works through the hidden range input.
  b$Runtime$evaluate(
    "document.querySelector('.animejs-controls button').click();
     document.querySelector('.animejs-controls').style.display = 'none';"
  )

  # Clip every snapshot to the SVG rectangle, measured once at a settled frame,
  # so all frames share the exact pixel dimensions gifski requires.
  seek(0.5)
  rect <- jsonlite::fromJSON(
    b$Runtime$evaluate(
      "JSON.stringify(document.querySelector('.html-widget svg').getBoundingClientRect())"
    )$result$value
  )
  cliprect <- round(c(rect$top, rect$left, rect$width, rect$height))

  frame_dir <- tempfile("frames")
  dir.create(frame_dir)
  on.exit(unlink(frame_dir, recursive = TRUE), add = TRUE)

  pngs <- file.path(frame_dir, sprintf("f%03d.png", seq_len(nframes)))
  for (i in seq_len(nframes)) {
    seek((i - 1) / (nframes - 1))
    b$screenshot(pngs[i], cliprect = cliprect, delay = 0.05)
  }

  gifski::gifski(
    pngs,
    gif_file = file,
    width = cliprect[3],
    height = cliprect[4],
    delay = 1 / gif_fps,
    progress = FALSE
  )
  invisible(file)
}

fig_dir <- "man/figures"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

p1 <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(
    title = "Year: {frame_time}",
    x = "GDP per capita",
    y = "life expectancy"
  ) +
  transition_time(year) +
  ease_aes("linear")

capture_gif(p1, file.path(fig_dir, "README-gapminder.gif"))

p2 <- ggplot(airquality, aes(Day, Temp)) +
  geom_point() +
  transition_time(Month) +
  shadow_mark(colour = "grey70") +
  labs(title = "Month: {frame_time}")

capture_gif(p2, file.path(fig_dir, "README-shadow.gif"))

message("Wrote ", file.path(fig_dir, "README-gapminder.gif"))
message("Wrote ", file.path(fig_dir, "README-shadow.gif"))
