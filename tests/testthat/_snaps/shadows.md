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

