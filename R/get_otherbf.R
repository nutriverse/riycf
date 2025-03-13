#' 
#' Create other breastfeeding-related indicators
#'
#' @description Identification of individual 0-23 months old children
#'   breastfeeding status:
#' 
#'   * Ever breastfed;
#'   * Early initiation of breastfeeding;
#'   * Exclusive breastfeeding for the first two days after birth;
#'   * Mixed milk feeding under 6 months;
#'   * Continuous breastfeeding 12-23 months; and,
#'   * Bottle feeding 0-23 months.
#'
#' @param age An integer vector of child's age in months.
#' @param q4 An integer vector indicating whether the child breastfed in the
#'   previous day. 1 = Yes; 0 = No.
#' @param q2 An integer vector indicating whether the child was put to the
#'   breast *immediately*, within *hours*, or within *days*. 0 = immediately;
#'   1 = for hours; 2 = for days.
#' @param q2_hour An integer vector for the number of hours after birth the
#'   child was put to the breast.
#' @param q3 An integer vector indicating whether the child had anything else
#'   beside breastmilk within the first two days after birth. 1 = Yes; 0 = No.
#' @param q5 An integer vector indicating whether the child fed with a bottle
#'   in the previous day. 1 = Yes; 0 = No.
#' @param q6b An integer vector indicating whether the child had infant formula
#'   feeding in the previous day. 1 = Yes; 0 = No.
#' @param q6c An integer vector indicating whether the child had any dairy
#'   products such as milk from animals or fresh, tinned or powdered milk.
#'   1 = Yes; 0 = No.
#'
#' @returns An integer vector indicating whether the child meets the definition
#'   of the respective breastfeeding indicator: 1 = Yes; 0 = No.
#'
#' @examples
#' df <- iycfData
#'
#' # Ever Breastfed
#' evbf <- get_evbf(df$child_bf, df$calc_age_months)
#'
#' # Early Initiation of Breastfeeding
#' eibf <- get_eibf(df$calc_age_months, df$child_eibf, df$child_eibf_hrs)
#'
#' # Exclusive Breastfeeding for the first two days after birth
#' q3 <- rbinom(n = nrow(df), size = 1, prob = 0.5)
#'
#' ebf2d <- get_ebf2d(q3, df$calc_age_months)
#'
#' # Mixed Milk Feeding Under 6 months
#' mixmf <- get_mixmf(
#'   df$child_bfyest, df$calc_age_months, df$child_bms, df$child_milk
#' )
#'
#' # Continuous Breastfeeding 12-23 months
#' cbf <- get_cbf(df$child_bfyest, df$calc_age_months)
#'
#' # Bottle Feeding 0-23 months
#' q5 <- rbinom(n = nrow(df), size = 1, prob = 0.5)
#'
#' bof <- get_bof(q5, df$calc_age_months)
#'
#' @author Nicholus Tint Zaw
#' @export
#' @rdname get_otherbf
#'

get_evbf <- function(q4, age) {
  ## Checkers ----
  check_if_number(q4)
  check_if_number(age)

  ## Recode ----
  evbf <- ifelse(
    age < 24 & q4 == 1L, 1L,
    ifelse(
      age < 24 & q4 == 0L, 0L, NA
    )
  )

  ## Return evbf ----
  evbf
}

#' 
#' @export
#' @rdname get_otherbf
#'

# Early Initiation of Breastfeeding
get_eibf <- function(age, q2, q2_hour){
  ## Checkers ----
  check_if_number(age)
  check_if_number(q2)
  check_if_number(q2_hour)

  ## Recode ----
  eibf <- ifelse(
    age < 24 & (q2 == 0L | q2_hour == 0L), 1L,
    ifelse(
      age >= 24, NA, 0L
    )
  )

  ## Return eibf ----
  eibf
}

#' 
#' @export
#' @rdname get_otherbf
#'

get_ebf2d <- function(q3, age){
  ## Checkers ----
  check_if_number(q3)
  check_if_number(age)

  ## Recode ----
  ebf2d <- ifelse(
    age < 24 & q3 == 0L, 1L,
    ifelse(
      age >= 24, NA, 0L
    )
  )

  ## Return ebf2d ----
  ebf2d
}

#' 
#' @export
#' @rdname get_otherbf
#'

get_mixmf <- function(q4, age, q6b, q6c){

  if(!is.null(q4) & !is.null(age) & !is.null(q6b) & !is.null(q6c)){

    mixmf <- ifelse(age < 6 & q4 == 1 & (q6b == 1 | q6c == 1), 1,
                    ifelse(age >= 6, NA, 0))

    mixmf <- ifelse(is.na(q4) | is.na(age) | (is.na(q6b) & is.na(q6c)),
                    NA, mixmf)

    return(mixmf)
  }
}

#' 
#' @export
#' @rdname get_otherbf
#'

get_cbf <- function(q4, age){

  if(!is.null(q4) & !is.null(age)){

    cbf <- ifelse(age >= 12 & age < 24 & q4 == 1, 1,
                  ifelse(age < 12, NA,
                         ifelse(age >= 24, NA, 0)))

    cbf <- ifelse(is.na(q4) | is.na(age), NA, cbf)

    return(cbf)

  }
}

#' 
#' @export
#' @rdname get_otherbf
#'

get_bof <- function(q5, age){

  if(!is.null(q5) & !is.null(age)){

    bof <- ifelse(age < 24 & q5 == 1, 1,
                  ifelse(age >= 24, NA, 0))

    bof <- ifelse(is.na(q5) | is.na(age), NA, bof)

    return(bof)
  }
}
