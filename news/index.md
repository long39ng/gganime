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
  `shadow_wake()` and `shadow_trail()` remain unsupported.

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

- Points now animate the colours their shape is actually drawn with. A
  solid pch such as the default 19 paints both its disc and its outline
  in `colour`, and only the disc was animated, so a point kept a stale
  ring while its fill tweened on. Open shapes (pch 0-14) no longer gain
  a fill, borderless ones (15-18) no longer gain an outline, and `alpha`
  now applies to both channels.

- Frames whose rows carry no tweenr identity no longer collapse onto a
  single element. `transition_states(wrap = TRUE)` ends on such a frame,
  and all but one of its points went missing along with a “NAs
  introduced by coercion” warning.

- Points keep their identity across a state boundary. tweenr records the
  identity of the frame a transition lands on against the wrong rows, so
  an element could arrive as a different one: it darted to another
  position and blended between the two colours over the frame it took to
  get there.
  [`anime()`](https://long39ng.github.io/gganime/reference/anime.md) now
  reconstructs the pairing tweenr meant to record, from the order in
  which each set of aesthetics occurs. Where that cannot be verified
  against the layer’s groups, the boundary is left as before.

- Text in the exported SVG keeps its position when the host page sets
  `white-space: pre`, which pkgdown does for the output of an example.
  Firefox otherwise preserved gridSVG’s indentation and pushed tick
  labels, legend keys, and the legend title out of line.
