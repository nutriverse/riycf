################################################################################
#
#' @title Construct other breastfeeding related indicators (from IYCF)
#'
#' @description Identification of individual 0-23 months old children
#'    breastfeeding status; Ever breastfed, Early initiation of breastfeeding,
#'    Exclusive breastfeeding for the first two days after birth, Mixed milk
#'    feeding under 6 months, Continuous breastfeeding 12-23 months, and Bottle
#'    feeding 0-23 months
#'
#' @param age This parameter holds the information about child age in the month
#'    format.
#'
#' @param q4 The binary variable which mentioned that the child was receiving
#'    breastfeeding in the previous day (yes = "1", no = "0").
#'
#' @param q2 the parameter indicate how long after the child was put to the
#'    barest immediately after birth (0 = immediately, 1 = for hours and 2 = for
#'    days)
#'
#' @param q2_hour the integer parameter record the hour(s) after birth the child
#'    was put to the breast
#'
#' @param q3 the binary variable indicating the child received anything else
#'    beside breastmilk with the first two days after birth
#'
#' @param q5 the binary variable presents the child received the bottle feeding
#'    in the previous day
#'
#' @param q6b the binary variable indicates that the child got infant formula
#'    feeding in the previous day
#'
#' @param q6c the binary variable presents the child got any of the following
#'    milk related food; Milk from animals, such as fresh, tinned or powdered
#'    milk.
#'
#' @return binary variables indicate child met the respective breastfeeding
#'    status or not (yes = 1 or no = 0)
#'
#'
#' @examples
#'
#'  df <- bfData
#'
#' # Ever Breastfed
#' evbf <- get_evbf(df$child_bfyest, df$calc_age_months)
#'
#' # Early Initiation of Breastfeeding
#' eibf <- get_eibf(df$calc_age_months, df$child_eibf, df$child_eibf_hrs)
#'
#' # Exclusive Breastfeeding for the first two days after birth
#' df$q3 <- rbinom(n = nrow(df), size = 1, prob = 0.5)
#'
#' ebf2d <- get_ebf2d(df$q3, df$calc_age_months)
#'
#' # Mixed Milk Feeding Under 6 months
#' mixmf <- get_mixmf(df$child_bfyest, df$calc_age_months,
#'                    df$child_bms, df$child_milk)
#'
#' # Continuous Breastfeeding 12-23 months
#' cbf <- get_cbf(df$child_bfyest, df$calc_age_months)
#'
#' # Bottle Feeding 0-23 months
#' df$q5 <- rbinom(n = nrow(df), size = 1, prob = 0.5)
#'
#' bof <- get_bof(df$q5, df$calc_age_months)
#'
#' @author Nicholus Tint Zaw
#' @export
#' @rdname get_otherbf
#'
#'
#################################################################################

# Ever Breastfed
get_evbf <- function(q4, age){

  if(!is.null(q4) & !is.null(age)){

    evbf <- ifelse(age < 24 & q4 == 1, 1, 0)

    return(evbf)
  }
}

################################################################################
#
#' @export
#' @rdname get_otherbf
#'
#
################################################################################

# Early Initiation of Breastfeeding
get_eibf <- function(age, q2, q2_hour){

  if(!is.null(age) & !is.null(q2) & !is.null(q2_hour)){

    eibf <- ifelse(age < 24 & (q2 == 0 | q2_hour == 0), 1, 0)

    return(eibf)
  }
}

################################################################################
#
#' @export
#' @rdname get_otherbf
#'
#
################################################################################

# Exclusive Breastfeeding for the first two days after birth
get_ebf2d <- function(q3, age){

  if(!is.null(q3) & !is.null(age)){

    ebf2d <- ifelse(age < 24 & q3 == 2, 1, 0)

    return(ebf2d)
  }
}

################################################################################
#
#' @export
#' @rdname get_otherbf
#'
#
################################################################################

# Mixed Milk Feeding Under 6 months
get_mixmf <- function(q4, age, q6b, q6c){

  if(!is.null(q4) & !is.null(age) & !is.null(q6b) & !is.null(q6c)){

    mixmf <- ifelse(age < 6 & q4 == 1 & (q6b == 1 | q6c == 1), 1, 0)

    return(mixmf)
  }
}

################################################################################
#
#' @export
#' @rdname get_otherbf
#'
#
################################################################################

# Continuous Breastfeeding 12-23 months
get_cbf <- function(q4, age){

  if(!is.null(q4) & !is.null(age)){

    cbf <- ifelse(age >= 12 & age < 24 & q4 == 1, 1, 0)

    return(cbf)

  }
}

################################################################################
#
#' @export
#' @rdname get_otherbf
#'
#
################################################################################

# Bottle Feeding 0-23 months
get_bof <- function(q5, age){

  if(!is.null(q5) & !is.null(age)){

    bof <- ifelse(age < 24 & q5 == 1, 1, 0)

    return(bof)

  }
}
