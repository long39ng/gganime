# gganime (development version)

* Plots with more than one layer now render. Each layer's grobs are exported in
  their own group, so two layers of the same kind (two `geom_point()` layers, two
  `geom_col()` layers) keep their own elements, and `geom_area()` or
  `geom_ribbon()` can be combined with `geom_line()` -- previously the area's
  invisible outline was mistaken for a second line and the plot stopped with a
  count mismatch.

* A layer that does not depend on the transition variable is drawn once and left
  static, so a fixed reference layer can sit beside an animated one.

# gganime 0.1.0

* First release. `anime()` renders a ggplot2 plot written with gganimate syntax
  as an animated SVG, returned as a self-contained htmlwidget driven by
  Anime.js, in place of gganimate's frame-by-frame GIF or video. The scene is
  drawn once and each element carries its own keyframes, so the animation stays
  sharp at any size and plays in the browser with a scrub bar.

* Transitions: `transition_states()`, `transition_time()`,
  `transition_reveal()`.

* Geoms: points, lines and paths, bars and columns, areas and ribbons, and
  polygons.

* Facets: `facet_wrap()` and `facet_grid()`, with fixed or free scales. Every
  panel animates on the one timeline, and strips and per-panel axes are drawn
  once.

* Coordinate systems: `coord_cartesian()`, `coord_fixed()` / `coord_equal()`,
  and `coord_flip()`.

* `enter_*()` and `exit_*()` transmuters, `ease_aes()`, `shadow_mark()`, and the
  `colour`, `fill`, `alpha`, and `size` aesthetics.

* Per-frame labels: title, subtitle, and caption glue strings (`{frame_time}`,
  `{closest_state}`, `{frame_along}`, `frame`, `nframes`, `progress`) swap in
  step with the scrub bar. Multi-line labels are frozen at their first-frame
  text with a warning.

* Loading gganime makes a bare gganimate plot print through `anime()` at the
  console and in knitr; set `options(gganime.autoprint = FALSE)` to keep
  gganimate's own output. `animate()` is re-exported from gganimate, so code
  that calls it keeps working.

* `gganimeOutput()` and `renderGganime()` embed a widget in Shiny applications
  and interactive R Markdown documents.

* A plot using anything gganime cannot render stops with one message naming the
  alternative. This covers non-linear coordinate systems (`coord_polar()`,
  `coord_radial()`, `coord_transform()`, `coord_sf()`), `shadow_wake()` and
  `shadow_trail()`, and `view_*()` other than `view_static()`.
