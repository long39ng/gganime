# Changelog

## gganime (development version)

- A bare gganimate plot now prints as an animated-SVG widget through
  [`anime()`](https://long39ng.github.io/gganime/reference/anime.md) at
  the console and in knitr. Set `options(gganime.autoprint = FALSE)` to
  keep gganimate’s own output.

- [`animate()`](https://gganimate.com/reference/animate.html) is
  re-exported from gganimate, so code that calls it keeps working after
  loading gganime.

- [`anime()`](https://long39ng.github.io/gganime/reference/anime.md)
  supports
  [`shadow_mark()`](https://gganimate.com/reference/shadow_mark.html).
  The raw data of other frames is drawn as static background marks
  behind the current frame: `past` accumulates earlier frames, `future`
  recedes through later ones, `exclude_layer` drops layers, and
  aesthetic arguments (e.g. `colour = "grey"`) restyle the shadow.
  [`shadow_wake()`](https://gganimate.com/reference/shadow_wake.html)
  and
  [`shadow_trail()`](https://gganimate.com/reference/shadow_trail.html)
  remain unsupported.

- [`anime()`](https://long39ng.github.io/gganime/reference/anime.md)
  animates per-frame plot labels. Title, subtitle, and caption glue
  strings (`{frame_time}`, `{closest_state}`, `{frame_along}`, `frame`,
  `nframes`, `progress`, …) swap in sync with the scrubber via Anime.js
  text keyframes. Labels that do not vary across frames stay static, and
  multi-line labels are frozen at their first-frame text with a warning.

- [`gganimeOutput()`](https://long39ng.github.io/gganime/reference/gganime-shiny.md)
  and
  [`renderGganime()`](https://long39ng.github.io/gganime/reference/gganime-shiny.md)
  embed gganime widgets in Shiny applications and interactive R Markdown
  documents.
