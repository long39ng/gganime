# annotate_labels freezes a label whose lines and tspans disagree

    Code
      elements <- annotate_labels(doc, labels)
    Condition
      Warning:
      title is frozen at its first-frame text.
      i It has 2 lines but the rendered label has 1.

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

# anime() freezes a label whose line count varies

    Code
      invisible(anime(time_plot(title = "Year {frame_time}{ifelse(frame > 2, '\nmore', '')}"),
      nframes = 4))
    Condition
      Warning:
      title is frozen at its first-frame text.
      i Its number of lines varies across frames.

