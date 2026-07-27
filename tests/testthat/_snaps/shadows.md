# shadow shape snapshot

    Code
      cat(jsonlite::toJSON(list(frame = u$union$union_data$.frame, presence = u$union$
        presence), auto_unbox = TRUE, pretty = TRUE))
    Output
      {
        "frame": [1, 2, 3],
        "presence": [
          [false, true, true],
          [true, false, true],
          [true, true, false]
        ]
      }

# a trail whose distance rounds to no frames is rejected

    Code
      anime(shadow_trail_plot(distance = 0.001), nframes = 8)
    Condition
      Error in `anime()`:
      ! This plot uses features gganime cannot render yet:
      x `shadow_trail(distance = 0.001)` spans less than one of 8 frames.
      i Use a `distance` of at least 0.12, or animate more frames.

