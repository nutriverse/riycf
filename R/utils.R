#'
#' Check if variables are missing
#' 
#' @keywords internal
#' 

check_if_missing <- function(x) {
  arg_name <- deparse(substitute(x))

  if (missing(x))
    cli::cli_abort(
      "{.strong {arg_name}} is missing."
    )
}

#'
#' Check if variables are NULL
#' 
#' @keywords internal 
#' 

check_if_null <- function(x) {
  arg_name <- deparse(substitute(x))

  if (is.null(x))
    cli::cli_abort(
      "{.strong {arg_name}} is NULL."
    )    
}

#'
#' Check if variables are integers
#' 
#' @keywords internal
#' 

check_if_integer <- function(x) {
  arg_name <- deparse(substitute(x))

  ## Check missing ----
  check_if_missing(x)

  if (!is.integer(x))
    cli::cli_abort(
      "{.strong {arg_name}} is not an integer."
    )
}

#'
#' Check if variables are numeric
#' 
#' @keywords internal
#' 

check_if_numeric <- function(x) {
  arg_name <- deparse(substitute(x))

  ## Check missing ----
  check_if_missing(x)

  if (!is.numeric(x))
    cli::cli_abort(
      "{.strong {arg_name}} is not numeric."
    )
}


#'
#' Check if number
#' 
#' @keywords internal
#' 

check_if_number <- function(x) {
  arg_name <- deparse(substitute(x))

  ## Check if missing ----
  ## (checked here rather than via check_if_missing() so that the message
  ## reports the caller's argument name instead of "x")
  if (missing(x))
    cli::cli_abort(
      "{.strong {arg_name}} is missing."
    )

  ## Check if NULL ----
  if (is.null(x))
    cli::cli_abort(
      "{.strong {arg_name}} is NULL."
    )

  ## Check if number ----
  ## (an all-NA vector is accepted, as NA is logical rather than numeric)
  if (!is.numeric(x) && !all(is.na(x)))
    cli::cli_abort(
      "{.strong {arg_name}} is not an integer or is not numeric."
    )
}

#'
#' Check if values are valid response codes
#'
#' Values of `x` that are not `NA` and not in `valid` (for example a
#' non-standard missing code such as `999`) are set to `NA`, with a warning
#' reporting how many values were changed.
#'
#' @param x A numeric vector of response codes.
#' @param valid A numeric vector of the documented valid codes. Default is
#'   `c(0, 1)` for No/Yes questions.
#'
#' @returns `x` with invalid values set to `NA`.
#'
#' @noRd
#'

check_if_valid_code <- function(x, valid = c(0, 1)) {
  arg_name <- deparse(substitute(x))

  ## Identify invalid codes ----
  invalid <- !is.na(x) & !(x %in% valid)

  ## Set invalid codes to NA and warn ----
  if (any(invalid)) {
    n_invalid <- sum(invalid)
    invalid_codes <- unique(x[invalid])

    cli::cli_warn(
      paste0(
        "{.strong {arg_name}}: {n_invalid} invalid value{?s} {?was/were} ",
        "set to NA ({invalid_codes}); valid codes are {valid}."
      )
    )

    x[invalid] <- NA
  }

  ## Return x ----
  x
}