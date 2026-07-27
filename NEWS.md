# gganime (development version)

* Plots with more than one layer now render. Each layer's grobs are exported in
  their own group, so two layers of the same kind (two `geom_point()` layers, two
  `geom_col()` layers) keep their own elements, and `geom_area()` or
  `geom_ribbon()` can be combined with `geom_line()` -- previously the area's
  invisible outline was mistaken for a second line and the plot stopped with a
  count mismatch.

* A layer that does not depend on the transition variable is drawn once and left
  static, so a fixed reference layer can sit beside an animated one.

* An element that leaves a state as another arrives no longer inherits its
  place. The two now animate as separate elements, so the arriving one appears
  where its own data puts it instead of sliding in from the other's position.
  This shape comes up whenever a state swaps one element for another and no
  `enter_*()`/`exit_*()` is set, which is gganimate's default.

* `shadow_trail()` is supported, so a breadcrumb trail of evenly spaced earlier
  frames can be left behind the animation. `distance` sets the spacing and
  `max_frames` caps how many marks are kept at once.

* A multi-line title, subtitle, or caption is animated a line at a time instead
  of being held at its first-frame text. A line whose text never changes is left
  as it was drawn. A label that gains or loses a line partway through is still
  held, since the text layout comes from the first frame.

* Point shapes other than circles now animate their size. Squares, triangles and
  diamonds (pch 0, 2, 5, 6, 15, 17, 18, 22, 23, 24, 25) grow and shrink with a
  mapped `size` the way the round shapes always did. The shapes drawn from
  several parts (pch 3, 4, and 7 to 14) still hold their size, and the warning
  that says so now names them and is issued once per layer.

* A mapped `stroke` animates a point's outline width, and an outline is drawn at
  the width R would draw it at. Outlines were previously about 1.6 times too
  thick at small sizes and too thin at large ones -- visible on the filled
  shapes, pch 21 to 25, where the outline and the fill are different colours.

* Bars keep their identity across a state boundary. A `geom_col()` or
  `geom_bar()` layer whose categories change between states previously redrew
  every bar as a new element, so bars that were present throughout blinked out
  and back at each boundary.

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
