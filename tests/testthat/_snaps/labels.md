# build_timeline emits an anime_text segment for a label element

    Code
      cat(jsonlite::toJSON(w$x$config$segments, auto_unbox = TRUE, pretty = TRUE,
      digits = NA))
    Output
      [
        {
          "selector": "[data-animejs-id='label_title']",
          "props": {
            "label": {
              "type": "text",
              "values": [
                "Year: 1967",
                "Year: 1968",
                "Year: 1969"
              ]
            }
          },
          "offset": 0
        }
      ]

# anime() freezes a multi-line label with a warning

    Code
      invisible(anime(time_plot(title = "Line one\nYear {frame_time}"), nframes = 4))
    Condition
      Warning:
      Multi-line title is frozen at its first-frame text.
      i A single text swap cannot drive a multi-line label.

