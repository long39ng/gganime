# build_timeline serialises to a stable config shape

    Code
      cat(jsonlite::toJSON(w$x$config, auto_unbox = TRUE, pretty = TRUE, digits = NA))
    Output
      {
        "kind": "timeline",
        "defaults": {
          "duration": 300,
          "ease": "linear",
          "delay": 0
        },
        "loop": true,
        "segments": [
          {
            "selector": "[data-animejs-id='L1e1']",
            "props": {
              "cx": [
                {
                  "to": 0
                },
                {
                  "to": 10
                },
                {
                  "to": 20
                }
              ],
              "cy": [
                {
                  "to": 0
                },
                {
                  "to": 5
                },
                {
                  "to": 0
                }
              ],
              "opacity": [
                {
                  "to": 1
                },
                {
                  "to": 1
                },
                {
                  "to": 0
                }
              ]
            },
            "offset": 0
          },
          {
            "selector": "[data-animejs-id='L1e2']",
            "props": {
              "r": [
                {
                  "to": 2
                },
                {
                  "to": 3
                },
                {
                  "to": 4
                }
              ]
            },
            "offset": 0
          }
        ],
        "events": [],
        "controls": true
      }

