# Render a ggplot with gganimate's transition_states as an animated-SVG widget.
#
# Five points move between two states. The plot is built through gganimate, the
# reference frame is exported to SVG with gridSVG, each point's per-frame position
# becomes an Anime.js keyframe track, and the whole thing is saved as a
# self-contained HTML widget with a play/scrub control bar.

suppressPackageStartupMessages({
  library(ggplot2)
  library(gganimate)
  library(grid)
  library(animejs)
})
stopifnot(packageVersion("animejs") >= "1.0.0")

fps <- 10
precision <- 2

# Build ---------------------------------------------------------------------
df <- data.frame(
  id = rep(1:5, times = 2),
  state = rep(c("a", "b"), each = 5),
  x = c(1, 2, 3, 4, 5, 5, 4, 3, 2, 1),
  y = c(2, 4, 1, 5, 3, 5, 1, 4, 2, 3)
)
p <- ggplot(df, aes(x, y, group = id)) +
  geom_point(size = 4) +
  transition_states(state, transition_length = 1, state_length = 1)
p$nframes <- 50L

b <- ggplot_build(p)
nf <- b$scene$nframes # realised frame count (gganimate adds one)
message("frames: ", nf)

# Reference gtable ----------------------------------------------------------
# All five points are present in every frame, so the first frame is a complete
# reference. Rewrap the frame so ggplot_gtable() dispatches (as gganimate does).
render_frame <- function(frame) {
  ccb <- get0("class_ggplot_built", asNamespace("ggplot2"))
  if (is.function(ccb)) {
    frame <- ccb(data = frame$data, layout = frame$layout, plot = frame$plot)
  } else {
    class(frame) <- "ggplot_built"
  }
  ggplot2::ggplot_gtable(frame)
}
gt <- render_frame(b$scene$get_frame(b, 1L))

# Export SVG ----------------------------------------------------------------
svgpath <- tempfile(fileext = ".svg")
grDevices::pdf(NULL)
grid.newpage()
grid.draw(gt)
grid.force()
exp <- gridSVG::grid.export(
  svgpath,
  exportCoords = "file",
  uniqueNames = TRUE,
  res = 96
)
grDevices::dev.off()

# Data-to-SVG mapping -------------------------------------------------------
# The panel's SVG rectangle plus the data ranges give a per-axis affine map.
# Both axes run positive; the panel viewport's own scales are [0, 1], not usable.
panel_key <- grep(
  "panel\\..*::GRID\\.VP\\.1\\.1$",
  names(exp$coords),
  value = TRUE
)
stopifnot(length(panel_key) == 1L)
rect <- exp$coords[[panel_key]]
xr <- b$layout$panel_params[[1]]$x.range
yr <- b$layout$panel_params[[1]]$y.range

to_svg_x <- function(dx) rect$x + (dx - xr[1]) / (xr[2] - xr[1]) * rect$width
to_svg_y <- function(dy) rect$y + (dy - yr[1]) / (yr[2] - yr[1]) * rect$height

# Locate the point elements, annotate them, tidy the SVG -------------------
# Points export as <use href="#gridSVG.pch19">, in data row order.
doc <- xml2::read_xml(svgpath)
xml2::xml_ns_strip(doc)

uses <- xml2::xml_find_all(doc, ".//use")
href <- vapply(
  uses,
  function(n) xml2::xml_attr(n, "href") %||% "",
  character(1)
)
pts <- uses[grepl("pch", href)]
stopifnot(length(pts) == 5L)
ids <- sprintf("pt-%d", seq_along(pts))

# Cross-check the affine against the exported positions before trusting it.
f1 <- b$data[[1]][[1L]]
f1 <- f1[order(f1$.id), ]
act_x <- as.numeric(vapply(pts, xml2::xml_attr, character(1), "x"))
act_y <- as.numeric(vapply(pts, xml2::xml_attr, character(1), "y"))
err <- max(abs(act_x - to_svg_x(f1$x)), abs(act_y - to_svg_y(f1$y)))
message("affine max error: ", signif(err, 3), " px")
stopifnot(err <= 0.05) # gridSVG rounds exported coordinates

# Anime.js targets each element by data-animejs-id.
for (i in seq_along(pts)) {
  xml2::xml_set_attr(pts[[i]], "data-animejs-id", ids[i])
}

# Drop gridSVG's coordinate <script>; it references an external file.
for (s in xml2::xml_find_all(doc, ".//script")) {
  xml2::xml_remove(s)
}

# Drop the fixed width/height on the root <svg> (keep viewBox) for resolution
# independence. xml2 cannot remove attributes, so patch the serialised tag.
root <- xml2::xml_root(doc)
stopifnot(nzchar(xml2::xml_attr(root, "viewBox")))
svg_string <- as.character(doc)
svg_string <- sub("(<svg\\b[^>]*?)\\s+width=\"[^\"]*\"", "\\1", svg_string)
svg_string <- sub("(<svg\\b[^>]*?)\\s+height=\"[^\"]*\"", "\\1", svg_string)

# Per-element keyframe tracks -----------------------------------------------
frames <- b$data[[1]] # length nf; each frame keyed by stable .id
xvals <- yvals <- setNames(vector("list", length(ids)), ids)
for (k in seq_along(ids)) {
  x_svg <- y_svg <- numeric(nf)
  for (fi in seq_len(nf)) {
    row <- frames[[fi]][frames[[fi]]$.id == k, ]
    x_svg[fi] <- to_svg_x(row$x)
    y_svg[fi] <- to_svg_y(row$y)
  }
  xvals[[k]] <- round(x_svg, precision)
  yvals[[k]] <- round(y_svg, precision)
}

# Timeline ------------------------------------------------------------------
tl <- anime_timeline(
  duration = nf / fps * 1000,
  ease = anime_easing("linear")
)
for (id in ids) {
  tl <- anime_add(
    tl,
    selector = anime_target_id(id),
    props = list(
      x = rlang::inject(anime_keyframes(!!!xvals[[id]])),
      y = rlang::inject(anime_keyframes(!!!yvals[[id]]))
    ),
    offset = 0 # absolute start so every element shares the one timeline
  )
}
tl <- anime_playback(tl, loop = TRUE, controls = TRUE)

w <- anime_render(tl, svg = svg_string, width = 640, height = 480)

# Save ----------------------------------------------------------------------
out <- normalizePath(
  file.path("tests", "manual", "point-transition-states.html"),
  mustWork = FALSE
)
htmlwidgets::saveWidget(w, out, selfcontained = TRUE)
message("wrote ", out, " (", round(file.size(out) / 1024), " KB)")
