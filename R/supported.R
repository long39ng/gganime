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
supported_shadows <- c(
  "ShadowNull",
  "ShadowMark"
)
supported_geoms <- c(
  "GeomPoint",
  "GeomCol",
  "GeomBar",
  "GeomRect",
  "GeomLine",
  "GeomPath",
  "GeomArea",
  "GeomRibbon",
  "GeomPolygon"
)

# Row-group geoms: one element spans many vertex rows, so the correspondence
# union and per-frame lookup carry every row of a key rather than just the
# first. GeomLine draws through GeomPath; GeomArea through GeomRibbon.
grouped_geom_classes <- c("GeomPath", "GeomRibbon", "GeomPolygon")

geom_is_grouped <- function(geom_class) {
  any(geom_class %in% grouped_geom_classes)
}

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
  if (!is.null(shadow) && !inherits(shadow, supported_shadows)) {
    cls <- class(shadow)[1]
    problems <- c(
      problems,
      x = cli::format_inline(
        "Shadow {.cls {cls}} is not supported yet."
      ),
      i = cli::format_inline(
        "Supported: {.fn shadow_mark} and {.fn shadow_null}."
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

  # geom_area + transition_time errors inside gganimate's own easing (M0 spike
  # 3), independent of gganime; point at geom_ribbon, which builds fine.
  if ("GeomArea" %in% geoms && inherits(tr, "TransitionTime")) {
    problems <- c(
      problems,
      x = cli::format_inline(
        "{.fn geom_area} with {.fn transition_time} errors inside gganimate."
      ),
      i = cli::format_inline("Use {.fn geom_ribbon} instead.")
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
        "Faceted plots are not supported yet; only a single panel renders."
      ),
      i = cli::format_inline(
        "Drop {.fn facet_wrap}/{.fn facet_grid}, or animate each facet separately."
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
