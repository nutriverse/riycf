################################################################################
#
#' @title Determining Minimum Acceptable Diet
#'
#' @description Identification of individual 6-23 months old children minimum
#'    acceptable diet based on their breastfeeding status
#'
#' @param age This parameter holds the information about child age in the month
#'    format.
#'
#' @param q4 The binary variable which mentions that the child was receiving
#'    breastfeeding in the previous day (yes = "1", no = "0").
#'
#' @param mdd The binary variable which holds whether the child received the
#'    minimum dietary diversity or not  (yes = "1", no = "0").
#'
#' @param mmf The binary variable which indicates that the child got minimum
#'    meal frequency ("1") or not ("0").
#'
#' @param mmff This parameter is about the non-breastfed children received
#'    minimum milk feeding frequency or not (yes = "1", no = "0").
#'
#'
#' @return binary variable indicate child get the minimum acceptable diet
#'    (mad =1 ) or not (mad = 0)
#'
#' @examples
#'   df <- iycfData
#'   df$mdd <- rbinom(n = nrow(df), size = 1, prob = 0.5)
#'   df$mmf <- rbinom(n = nrow(df), size = 1, prob = 0.5)
#'   df$mmff <- rbinom(n = nrow(df), size = 1, prob = 0.5)
#'
#'   mad <- get_mad(df$calc_age_months, df$child_bfyest, df$mdd, df$mmf, df$mmff)
#'
#'
#'
#' @author Nicholus Tint Zaw
#' @export
#' @rdname get_mdd
#'
#################################################################################


################################################################################

get_mad <- function(age, q4, mdd, mmf, mmff){
  ## Checkers ----
  check_if_number(age)
  check_if_number(q4)
  check_if_number(mdd)
  check_if_number(mmf)
  check_if_number(mmff)

  ## Set invalid codes and out-of-range ages to NA ----
  age <- check_if_valid_age(age)
  q4 <- check_if_valid_code(q4, valid = c(0, 1))
  mdd <- check_if_valid_code(mdd, valid = c(0, 1))
  mmf <- check_if_valid_code(mmf, valid = c(0, 1))
  mmff <- check_if_valid_code(mmff, valid = c(0, 1))

  ## Recode ----
  mad <- ifelse(age >= 6 & age < 24 &
                  (q4 == 1 | mmff == 1) &
                  mdd == 1 & mmf == 1, 1,
                ifelse(age < 6, NA,
                       ifelse(age >= 24, NA, 0)))

  mad <- ifelse(is.na(age) | is.na(mdd) | is.na(mmf) |
                  (is.na(q4) & is.na(mmff)), NA, mad)

  ## Return mad ----
  mad
}


