map <- function(.x, .f, ...) {
  .f <- rlang::as_function(.f, env = rlang::global_env())
  lapply(.x, .f, ...)
}

walk <- function(.x, .f, ...) {
  map(.x, .f, ...)
  invisible(.x)
}

walk2 <- function(.x, .y, .f, ...) {
  map2(.x, .y, .f, ...)
  invisible(.x)
}

map2 <- function(.x, .y, .f, ...) {
  .f <- rlang::as_function(.f, env = rlang::global_env())
  out <- mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
  if (length(out) == length(.x)) {
    rlang::set_names(out, names(.x))
  } else {
    rlang::set_names(out, NULL)
  }
}

handle_arg_list = function(..., tests) {
  values = list(...)
  #names = names(values)
  names = eval(substitute(alist(...)))
  names = map(names, deparse)

  walk2(names, values, tests)
}

arg_is_scalar = function(...,  allow_null = FALSE, allow_na = FALSE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (length(value) > 1 | (!allow_null & length(value) == 0)) {
        cli::cli_abort("Argument {.val {name}} must be of length 1.")
      }

      if (!is.null(value)) {
        if (is.na(value) & !allow_na)
          cli::cli_abort("Argument {.val {name}} must not be a missing value ({.val {NA}}).")
      }
    }
  )
}

arg_is_lgl = function(..., allow_null = FALSE, allow_na = FALSE, allow_empty = TRUE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!(is.logical(value) | (is.null(value) & !allow_null)))
        cli::cli_abort("Argument {.val {name}} must be of logical type.")

      if (any(is.na(value)) & !allow_na)
        cli::cli_abort("Argument {.val {name}} must not contain any missing values ({.val {NA}}).")

      if (length(value) == 0 & !allow_empty)
        cli::cli_abort("Argument {.val {name}} must have length >= 1.")
    }
  )
}


arg_is_nonneg_int = function(..., allow_null = FALSE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!((is.numeric(value) && all(value >= 0) && all(value %% 1 == 0)) |
            (is.null(value) & !allow_null)))
        cli::cli_abort("All {.val {name}} must be whole positive number(s).")
    }
  )
}

arg_is_pos_int = function(..., allow_null = FALSE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!((is.numeric(value) && all(value > 0) && all(value %% 1 == 0)) |
            (is.null(value) & allow_null)))
        cli::cli_abort("All {.val {name}} must be whole positive number(s).")
    }
  )
}

arg_is_int = function(..., allow_null = FALSE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!((is.numeric(value) && all(value %% 1 == 0)) |
            (is.null(value) & !allow_null)))
        cli::cli_abort("All {.val {name}} must be whole positive number(s).")
    }
  )
}

arg_is_numeric = function(..., allow_null = FALSE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!(is.numeric(value) | (is.null(value) & !allow_null)))
        cli::cli_abort("All {.val {name}} must numeric.")
    }
  )
}


arg_is_lgl = function(..., allow_null = FALSE, allow_na = FALSE, allow_empty = TRUE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!(is.logical(value) | (is.null(value) & !allow_null)))
        cli::cli_abort("Argument {.val {name}} must be of logical type.")

      if (any(is.na(value)) & !allow_na)
        cli::cli_abort("Argument {.val {name}} must not contain any missing values ({.val {NA}}).")

      if (length(value) == 0 & !allow_empty)
        cli::cli_abort("Argument {.val {name}} must have length >= 1.")
    }
  )
}

arg_is_probabilities = function(..., allow_null = FALSE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!((is.numeric(value) && all(value >= 0) && all(value <= 1)) |
            (is.null(value) & allow_null)))
        cli::cli_abort("All {.val {name}} must be in [0,1].")
    }
  )
}

arg_is_chr = function(..., allow_null = FALSE, allow_na = FALSE, allow_empty = FALSE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!(is.character(value) | (is.null(value) & allow_null)))
        cli::cli_abort("Argument {.val {name}} must be of character type.")

      if (any(is.na(value)) & !allow_na)
        cli::cli_abort("Argument {.val {name}} must not contain any missing values ({.val {NA}}).")

      if (length(value) == 0 & !(allow_empty | allow_null))
        cli::cli_abort("Argument {.val {name}} must have length >= 1.")
    }
  )
}




