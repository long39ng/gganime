check_number <- function(
  x,
  arg,
  min = -Inf,
  max = Inf,
  call = rlang::caller_env()
) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < min || x > max) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a single number{if (is.finite(min) || is.finite(max)) ' in range' else ''}.",
        x = "You supplied {.obj_type_friendly {x}}."
      ),
      call = call
    )
  }
  invisible(x)
}

check_count <- function(x, arg, min = 1L, call = rlang::caller_env()) {
  if (
    !is.numeric(x) ||
      length(x) != 1L ||
      is.na(x) ||
      x < min ||
      x != round(x)
  ) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a single integer >= {min}.",
        x = "You supplied {.obj_type_friendly {x}}."
      ),
      call = call
    )
  }
  invisible(as.integer(x))
}

check_bool <- function(x, arg, call = rlang::caller_env()) {
  if (!rlang::is_bool(x)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be {.val {TRUE}} or {.val {FALSE}}.",
        x = "You supplied {.obj_type_friendly {x}}."
      ),
      call = call
    )
  }
  invisible(x)
}
