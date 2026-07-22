# Supported-feature checks: reject a plot that uses features gganime cannot
# render. One check runs before the build, on the plot object; another runs
# after, on the built object. Each collects every violation and raises a single
# cli_abort. Messages are formatted eagerly, so their values are in scope when
# collected.

supported_transitions <- c(
  "TransitionStates",
  "TransitionTime",
  "TransitionReveal"
)
supported_geoms <- c("GeomPoint", "GeomCol", "GeomBar", "GeomRect")

# Pre-build: transition, view, shadow, and every layer geom.
check_supported_prebuild <- function(plot, call = rlang::caller_env()) {
  problems <- character(0)

  tr <- plot$transition
  if (is.null(tr)) {
    problems <- c(
      problems,
      x = cli::format_inline(
        "No transition found; add e.g. {.fn transition_states} to animate the plot."
      )
    )
  } else if (!inherits(tr, supported_transitions)) {
    cls <- class(tr)[1]
    problems <- c(
      problems,
      x = cli::format_inline("Transition {.cls {cls}} is not supported yet."),
      i = cli::format_inline("Supported: {.val {supported_transitions}}.")
    )
  }

  view <- plot$view
  if (!is.null(view) && !inherits(view, "ViewStatic")) {
    cls <- class(view)[1]
    problems <- c(
      problems,
      x = cli::format_inline(
        "View {.cls {cls}} is not supported; only {.fn view_static}."
      )
    )
  }

  shadow <- plot$shadow
  if (!is.null(shadow) && !inherits(shadow, "ShadowNull")) {
    cls <- class(shadow)[1]
    problems <- c(
      problems,
      x = cli::format_inline(
        "Shadow {.cls {cls}} is not supported yet."
      )
    )
  }

  geoms <- vapply(plot$layers, function(l) class(l$geom)[1], character(1))
  bad <- unique(geoms[!geoms %in% supported_geoms])
  if (length(bad) > 0) {
    problems <- c(
      problems,
      x = cli::format_inline("Unsupported geom{?s}: {.val {bad}}."),
      i = cli::format_inline("Supported: {.val {supported_geoms}}.")
    )
  }

  abort_problems(problems, call)
}

# Post-build: coordinate system and panel count.
check_supported_postbuild <- function(built, call = rlang::caller_env()) {
  problems <- character(0)

  coord <- built$plot$coordinates
  if (!identical(class(coord)[1], "CoordCartesian")) {
    cls <- class(coord)[1]
    problems <- c(
      problems,
      x = cli::format_inline(
        "Coordinate system {.cls {cls}} is not supported; only {.fn coord_cartesian}."
      )
    )
  }

  if (length(built$layout$panel_params) != 1L) {
    problems <- c(
      problems,
      x = cli::format_inline(
        "Faceted plots are not supported yet."
      )
    )
  }

  abort_problems(problems, call)
}

abort_problems <- function(problems, call) {
  if (length(problems) == 0) {
    return(invisible())
  }
  cli::cli_abort(
    c("This plot uses features gganime cannot render yet:", problems),
    call = call
  )
}
