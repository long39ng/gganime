# gganime (development version)

* A bare gganimate plot now prints as an animated-SVG widget through `anime()`
  at the console and in knitr. Set `options(gganime.autoprint = FALSE)` to keep
  gganimate's own output.

* `animate()` is re-exported from gganimate, so code that calls it keeps
  working after loading gganime.

* `anime()` supports `shadow_mark()`. The raw data of other frames is drawn as
  static background marks behind the current frame: `past` accumulates earlier
  frames, `future` recedes through later ones, `exclude_layer` drops layers, and
  aesthetic arguments (e.g. `colour = "grey"`) restyle the shadow. `shadow_wake()`
  and `shadow_trail()` remain unsupported.

* `anime()` animates per-frame plot labels. Title, subtitle, and caption glue
  strings (`{frame_time}`, `{closest_state}`, `{frame_along}`, `frame`,
  `nframes`, `progress`, ...) swap in sync with the scrubber via Anime.js text
  keyframes. Labels that do not vary across frames stay static, and multi-line
  labels are frozen at their first-frame text with a warning.

* `gganimeOutput()` and `renderGganime()` embed gganime widgets in Shiny
  applications and interactive R Markdown documents.
