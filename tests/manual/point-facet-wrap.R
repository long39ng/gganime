# Render a faceted geom_point scatter under transition_states as an animated-SVG
# widget, and write a gganimate gif of the same plot for side-by-side comparison.
# Every panel animates on the one timeline; the strips and the per-panel axes are
# drawn once and stay still. The x scale is free, so each panel maps its own data
# range onto its own rectangle.

devtools::load_all(".", quiet = TRUE)
suppressPackageStartupMessages({
  library(ggplot2)
})

set.seed(1)
groups <- c("a", "b", "c")
df <- do.call(
  rbind,
  lapply(seq_along(groups), function(g) {
    do.call(
      rbind,
      lapply(1:3, function(state) {
        data.frame(
          id = seq_len(6),
          facet = groups[g],
          state = state,
          # Panel c spans roughly ten times the x range of panel a.
          x = 10^(g - 1) * (seq_len(6) + rnorm(6, sd = 0.2)),
          y = seq_len(6) + sin(state) + rnorm(6, sd = 0.2)
        )
      })
    )
  })
)

p <- ggplot(df, aes(x, y, colour = factor(id), group = id)) +
  geom_point(size = 4) +
  facet_wrap(~facet, scales = "free_x") +
  labs(title = "state {closest_state}", colour = "id") +
  transition_states(state, transition_length = 2, state_length = 1)

w <- anime(p, nframes = 60, fps = 20, width = 800, height = 400)

out <- normalizePath(
  file.path("tests", "manual", "point-facet-wrap.html"),
  mustWork = FALSE
)
htmlwidgets::saveWidget(w, out, selfcontained = TRUE)
message("wrote ", out, " (", round(file.size(out) / 1024), " KB)")

gif <- animate(
  p,
  nframes = 60,
  fps = 20,
  width = 800,
  height = 400,
  renderer = gganimate::gifski_renderer()
)
gifout <- file.path("tests", "manual", "point-facet-wrap.gif")
gganimate::anim_save(gifout, gif)
message("wrote ", normalizePath(gifout))
