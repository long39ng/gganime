# anime() rejects a plain ggplot

    Code
      anime(p)
    Condition
      Error in `anime()`:
      ! `plot` must be a <gganim> plot.
      i Add a transition, e.g. `+ transition_states(state)`, to a ggplot.

# a gganim without a supported geom is gated pre-build

    Code
      check_supported_prebuild(p)
    Condition
      Error:
      ! This plot uses features gganime cannot render yet:
      x Unsupported geom: "GeomBoxplot".
      i Supported: "GeomPoint", "GeomCol", "GeomBar", "GeomRect", "GeomLine", "GeomPath", "GeomArea", "GeomRibbon", and "GeomPolygon".

# geom_area with transition_time is gated with a geom_ribbon pointer

    Code
      check_supported_prebuild(p)
    Condition
      Error:
      ! This plot uses features gganime cannot render yet:
      x `geom_area()` with `transition_time()` errors inside gganimate.
      i Use `geom_ribbon()` instead.

# shadow_wake is gated with a shadow_mark pointer

    Code
      check_supported_prebuild(p)
    Condition
      Error:
      ! This plot uses features gganime cannot render yet:
      x Shadow <ShadowWake> is not supported yet.
      i Supported: `shadow_mark()` and `shadow_null()`.

# a gganim with no transition is gated

    Code
      check_supported_prebuild(p)
    Condition
      Error:
      ! This plot uses features gganime cannot render yet:
      x Transition <TransitionNull> is not supported yet.
      i Supported: "TransitionStates", "TransitionTime", and "TransitionReveal".

