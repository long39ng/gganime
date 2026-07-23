# Load hooks: the gganime.autoprint option and the S3 overrides that make a bare
# gganimate plot render through anime() at the console and in knitr.

# Holds the gganimate methods we shadow, for the autoprint = FALSE path.
gganime_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  if (!"gganime.autoprint" %in% names(options())) {
    options(gganime.autoprint = TRUE)
  }

  # gganime loads after gganimate, so registering here wins; keep theirs first.
  gganime_env$prev_print <- getS3method("print", "gganim", optional = TRUE)
  registerS3method("print", "gganim", print_gganim, envir = asNamespace("base"))

  if (requireNamespace("knitr", quietly = TRUE)) {
    gganime_env$prev_knit_print <- getS3method(
      "knit_print",
      "gganim",
      optional = TRUE
    )
    registerS3method(
      "knit_print",
      "gganim",
      knit_print_gganim,
      envir = asNamespace("knitr")
    )
  }
}

print_gganim <- function(x, ...) {
  if (isTRUE(getOption("gganime.autoprint", TRUE))) {
    print(anime(x))
    return(invisible(x))
  }
  prev <- gganime_env$prev_print
  if (is.null(prev)) NextMethod() else prev(x, ...)
}

knit_print_gganim <- function(x, ...) {
  if (isTRUE(getOption("gganime.autoprint", TRUE))) {
    return(knitr::knit_print(anime(x), ...))
  }
  prev <- gganime_env$prev_knit_print
  if (is.null(prev)) NextMethod() else prev(x, ...)
}
