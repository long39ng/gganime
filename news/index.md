# Changelog

## gganime 0.1.0

- First release.
  [`anime()`](https://long39ng.github.io/gganime/reference/anime.md)
  renders a ggplot2 plot written with gganimate syntax as an animated
  SVG, returned as a self-contained htmlwidget driven by Anime.js, in
  place of gganimate’s frame-by-frame GIF or video. The scene is drawn
  once and each element carries its own keyframes, so the animation
  stays sharp at any size and plays in the browser with a scrub bar.

- Transitions:
  [`transition_states()`](https://gganimate.com/reference/transition_states.html),
  [`transition_time()`](https://gganimate.com/reference/transition_time.html),
  [`transition_reveal()`](https://gganimate.com/reference/transition_reveal.html).

- Geoms: points, lines and paths, bars and columns, areas and ribbons,
  and polygons.

- Facets:
  [`facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  and
  [`facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html),
  with fixed or free scales. Every panel animates on the one timeline,
  and strips and per-panel axes are drawn once.

- Coordinate systems:
  [`coord_cartesian()`](https://ggplot2.tidyverse.org/reference/coord_cartesian.html),
  [`coord_fixed()`](https://ggplot2.tidyverse.org/reference/coord_fixed.html)
  /
  [`coord_equal()`](https://ggplot2.tidyverse.org/reference/coord_fixed.html),
  and
  [`coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html).

- `enter_*()` and `exit_*()` transmuters,
  [`ease_aes()`](https://gganimate.com/reference/ease_aes.html),
  [`shadow_mark()`](https://gganimate.com/reference/shadow_mark.html),
  and the `colour`, `fill`, `alpha`, and `size` aesthetics.

- Per-frame labels: title, subtitle, and caption glue strings
  (`{frame_time}`, `{closest_state}`, `{frame_along}`, `frame`,
  `nframes`, `progress`) swap in step with the scrub bar. Multi-line
  labels are frozen at their first-frame text with a warning.

- Loading gganime makes a bare gganimate plot print through
  [`anime()`](https://long39ng.github.io/gganime/reference/anime.md) at
  the console and in knitr; set `options(gganime.autoprint = FALSE)` to
  keep gganimate’s own output.
  [`animate()`](https://gganimate.com/reference/animate.html) is
  re-exported from gganimate, so code that calls it keeps working.

- [`gganimeOutput()`](https://long39ng.github.io/gganime/reference/gganime-shiny.md)
  and
  [`renderGganime()`](https://long39ng.github.io/gganime/reference/gganime-shiny.md)
  embed a widget in Shiny applications and interactive R Markdown
  documents.

- A plot using anything gganime cannot render stops with one message
  naming the alternative. This covers non-linear coordinate systems
  ([`coord_polar()`](https://ggplot2.tidyverse.org/reference/coord_radial.html),
  [`coord_radial()`](https://ggplot2.tidyverse.org/reference/coord_radial.html),
  [`coord_transform()`](https://ggplot2.tidyverse.org/reference/coord_transform.html),
  [`coord_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)),
  `shadow_wake()` and `shadow_trail()`, and `view_*()` other than
  `view_static()`.
