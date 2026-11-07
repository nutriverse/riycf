################################################################################
#
#' @title Construction of Dummy Variable(s)
#'
#' @description generate dummy variables based on the input vector(s) binary
#'    parameters (true = 1 and false = 0). If the observation of one of the
#'    input parameter was true, that observation's newly generated dummy
#'    variable will return as true (1). And, if all input parameters were false,
#'    the return will be false (0).
#'
#' @param var_list list of food consumption variables
#'
#' @return the vector with "0" and "1" binary result.
#'
#' @details **Data preparation.** `get_dummy()` expects each variable in
#'   `var_list` to be coded 1 = Yes and 0 = No. Before using it, check the
#'   data for:
#'
#'   * **missing or "don't know" codes** such as `98`, `99` or `999`: values
#'     other than 0 and 1 are set to `NA` with a warning;
#'   * **variables that do not exist** in the dataset: `df$name` then returns
#'     `NULL`; such items are ignored with a warning, so check the variable
#'     names;
#'   * **skip-pattern follow-up questions** (for example "Was the milk
#'     sweetened?", only asked if the child had milk): a skipped follow-up is
#'     `NA` and makes the result `NA`; resolve it first, for example with
#'     `get_followup()`.
#'
#'   If any variable in `var_list` is `NA` for a child, the result for that
#'   child is `NA`.
#'
#' @examples
#'
#' # Individual Food Group Variable Construction
#'
#'  df <- iycfData
#'
#' # Individual Food Group Specific Function
#'  breastmilk <- get_dummy(var_list = list(df$child_bfyest))
#'
#'  grains <- get_dummy(var_list = list(df$child_rice, df$child_potatoes))
#'
#'  pulses <- get_dummy(var_list = list(df$child_beans))
#'
#'  dairy_list <- list(df$child_bms, df$child_milk,
#'                     df$child_mproduct, df$child_yogurt,
#'                     df$child_yogurt)
#'
#'  dairy <- get_dummy(var_list = dairy_list)
#'
#'  meat_list <- list(df$child_organ, df$child_insects,
#'                    df$child_beef, df$child_fish)
#'
#'  meat <- get_dummy(var_list = meat_list)
#'
#'  eggs <- get_dummy(var_list = list(df$child_eggs))
#'
#'  vita_fruveg_list <- list(df$child_pumpkin,
#'                           df$child_leafyveg,
#'                           df$child_mango)
#'
#'  vita_fruveg <- get_dummy(var_list = vita_fruveg_list)
#'
#'  oth_fruveg <- get_dummy(var_list = list(df$child_fruit, df$child_fruit))
#'
#'
#' @author Nicholus Tint Zaw
#' @export
#' @rdname get_dummy
#'
#'
#################################################################################

get_dummy <- function(var_list = NULL){
  ## Checkers ----
  if (!is.list(var_list) || length(var_list) == 0)
    cli::cli_abort(
      "{.strong var_list} must be a non-empty list of numeric vectors."
    )

  ## Drop items that do not exist (NULL) with a warning ----
  null_items <- which(vapply(var_list, is.null, logical(1)))

  if (length(null_items) == length(var_list))
    cli::cli_abort(
      "All items in {.strong var_list} are NULL; check the variable names."
    )

  if (length(null_items) > 0) {
    null_text <- paste(
      if (length(null_items) == 1) "Item" else "Items",
      paste(null_items, collapse = ", "),
      "of {.strong var_list}",
      if (length(null_items) == 1) "is NULL and was" else "are NULL and were",
      "ignored; check that the variable names exist."
    )
    cli::cli_warn(null_text)
    var_list <- var_list[-null_items]
  }

  ## Check that items are numeric and of equal length ----
  not_number <- which(
    !vapply(var_list, function(x) is.numeric(x) || all(is.na(x)), logical(1))
  )

  if (length(not_number) > 0)
    cli::cli_abort(
      paste(
        if (length(not_number) == 1) "Item" else "Items",
        paste(not_number, collapse = ", "),
        "of {.strong var_list}",
        if (length(not_number) == 1) "is" else "are",
        "not numeric."
      )
    )

  if (length(unique(lengths(var_list))) > 1)
    cli::cli_abort(
      "All items in {.strong var_list} must have the same length."
    )

  ## Set invalid codes to NA ----
  n_invalid <- sum(
    vapply(var_list, function(x) sum(!is.na(x) & !(x %in% c(0, 1))), numeric(1))
  )

  if (n_invalid > 0) {
    invalid_codes <- unique(
      unlist(lapply(var_list, function(x) x[!is.na(x) & !(x %in% c(0, 1))]))
    )
    cli::cli_warn(
      paste0(
        "{.strong var_list}: {n_invalid} invalid value{?s} {?was/were} set ",
        "to NA ({invalid_codes}); valid codes are 0 and 1."
      )
    )
    var_list <- lapply(
      var_list, function(x) { x[!is.na(x) & !(x %in% c(0, 1))] <- NA; x }
    )
  }

  ## Recode ----
  vect_df <- as.data.frame(do.call(cbind, var_list))

  total <- rowSums(vect_df, na.rm = FALSE)

  dummy_var <- ifelse(total > 0, 1,
                      ifelse(is.na(total), NA, 0))

  ## Return dummy_var ----
  dummy_var
}

