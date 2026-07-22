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
      x Unsupported geom: "GeomLine".
      i Supported: "GeomPoint".

# a gganim with no transition is gated

    Code
      check_supported_prebuild(p)
    Condition
      Error:
      ! This plot uses features gganime cannot render yet:
      x Transition <TransitionNull> is not supported yet.
      i Supported: "TransitionStates", "TransitionTime", and "TransitionReveal".

