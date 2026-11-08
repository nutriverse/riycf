#'
#' Resolve a skip-pattern follow-up question using its main question
#'
#' @description Many IYCF questions have a follow-up question that is only
#'   asked when the main question is answered "yes". For example, *"Was the
#'   milk sweetened or flavoured?"* is only asked if the child had animal
#'   milk. When the main answer is "no", the follow-up is skipped and is
#'   usually stored as `NA`, which means *"did not have it"* rather than
#'   *"unknown"*.
#'
#'   `get_followup()` combines a main question and its follow-up into one
#'   variable that can be passed to [get_dummy()] and to the indicator
#'   functions (for example, the sweet beverage items used by [get_swb()]).
#'
#'   The follow-up can also be a count. For example, *"How many times did the
#'   child drink infant formula?"* is only asked if the child had infant
#'   formula; when it is skipped, the child had it 0 times. Use `valid` to give
#'   the accepted range of counts (for example `valid = 0:20`), and pass the
#'   result to [get_milk_frq()] or [get_nonbf_frq()].
#'
#' @param main An integer vector for the main ("gate") question. 1 = Yes
#'   (the follow-up is asked); 0 = No (the follow-up is skipped).
#' @param followup An integer vector for the follow-up question, of the same
#'   length as `main`. 1 = Yes; 0 = No; or a count when `valid` is a range of
#'   counts.
#' @param valid A numeric vector of the valid values of `followup`. Default is
#'   `c(0, 1)` for a Yes/No follow-up. Use, for example, `0:20` for the number
#'   of times something was eaten or drunk.
#'
#' @returns A numeric vector:
#'
#'   `main` | `followup` | Result
#'   :--- | :--- | :---
#'   0 | `NA` (correctly skipped) or 0 | 0
#'   1 | 1 (or a count) | 1 (or the count)
#'   1 | 0 | 0
#'   1 | `NA` (should have been asked) | `NA`, with a warning
#'   0 | above 0 (skip pattern broken) | `NA`, with a warning
#'   `NA` | any | `NA`
#'
#'   Values of `main` other than 0 and 1, and values of `followup` not in
#'   `valid` (for example the missing codes `98`, `99` or `999`), are set to
#'   `NA` with a warning before the above is applied.
#'
#' @details **Limitation:** `get_followup()` only handles the skip pattern in
#'   which a main question coded 1 means the follow-up is asked and 0 means it
#'   is skipped. It does not handle other kinds of skip pattern, such as:
#'
#'   * a categorical main question (for example, the number of hours after
#'     birth is asked only when the answer to the early initiation question is
#'     "hours"; [get_eibf()] handles this itself);
#'   * follow-ups that depend on more than one question, or on the child's
#'     age;
#'   * follow-ups that are asked when the main answer is "no".
#'
#'   Such follow-ups need to be recoded by the user before use.
#'
#' @examples
#' get_followup(main = c(1, 1, 0, 0, NA), followup = c(1, 0, NA, 0, NA))
#'
#' # Sweet beverage consumption (6-23 months)
#' df <- iycfData
#'
#' sweet <- list(
#'   get_followup(df$child_milk, df$child_milk_sweet),
#'   get_followup(df$child_mproduct, df$child_mproduct_sweet),
#'   df$child_chocolate, df$child_juice, df$child_soda,
#'   get_followup(df$child_tea, df$child_tea_sweet),
#'   get_followup(df$child_oth_drink, df$child_oth_drink_sweet)
#' )
#'
#' swb <- get_swb(get_dummy(var_list = sweet), df$calc_age_months)
#'
#' # Milk feeding frequency, using counts (number of times)
#' milk_frq <- get_milk_frq(
#'   q6bnum = get_followup(df$child_bms, df$child_bms_freq, valid = 0:20),
#'   q6cnum = get_followup(df$child_milk, df$child_milk_freq, valid = 0:20),
#'   q6dnum = get_followup(df$child_mproduct, df$child_mproduct_freq,
#'                         valid = 0:20),
#'   q7anum = get_followup(df$child_yogurt, df$child_yogurt_frq, valid = 0:20)
#' )
#'
#' mmff <- get_mmff(df$child_bfyest, df$calc_age_months, milk_frq)
#'
#' @author Nicholus Tint Zaw
#' @export
#'

get_followup <- function(main, followup, valid = c(0, 1)) {
  ## Checkers ----
  check_if_number(main)
  check_if_number(followup)
  check_if_number(valid)

  if (length(main) != length(followup))
    cli::cli_abort(
      "{.strong main} and {.strong followup} must have the same length."
    )

  ## Set invalid codes to NA ----
  main <- check_if_valid_code(main, valid = c(0, 1))
  followup <- check_if_valid_code(followup, valid = valid)

  ## Flag records that do not follow the skip pattern ----
  missing_followup <- !is.na(main) & main == 1 & is.na(followup)
  broken_skip <- !is.na(main) & main == 0 & !is.na(followup) & followup != 0

  if (any(missing_followup)) {
    n_missing <- sum(missing_followup)
    cli::cli_warn(
      paste0(
        "{n_missing} follow-up value{?s} {?is/are} missing although the ",
        "main question is 1 (yes); set to NA."
      )
    )
  }

  if (any(broken_skip)) {
    n_broken <- sum(broken_skip)
    cli::cli_warn(
      paste0(
        "{n_broken} record{?s} {?has/have} main question 0 (no) but a ",
        "follow-up value above 0, which breaks the skip pattern; set to NA."
      )
    )
  }

  ## Resolve ----
  followup_resolved <- ifelse(
    main == 0, ifelse(broken_skip, NA, 0L), followup
  )

  ## Return followup_resolved ----
  followup_resolved
}
