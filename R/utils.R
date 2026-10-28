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
  check_if_missing(x)

  ## Check if number ----
  if (!is.numeric(x) && !is.integer(x) && !is.na(x))
    cli::cli_abort(
      "{.strong .var {arg_name}} is not an integer or is not numeric."
    )
}