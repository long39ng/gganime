# gganime (development version)

* `anime()` animates per-frame plot labels. Title, subtitle, and caption glue
  strings (`{frame_time}`, `{closest_state}`, `{frame_along}`, `frame`,
  `nframes`, `progress`, ...) swap in sync with the scrubber via Anime.js text
  keyframes. Labels that do not vary across frames stay static, and multi-line
  labels are frozen at their first-frame text with a warning.
