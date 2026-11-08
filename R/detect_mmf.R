################################################################################
#
#' @title Determining Minimum Meal Freq
#'
#' @description Identification of individual 6-23 months old children received
#'    the minimum meal frequency based on their breastfeeding status.
#'
#' @param age This parameter holds the information about child age in the month
#'    format.
#'
#' @param q4 The binary variable which mentioned that the child received
#'    breastfeeding in the previous day (yes = "1", no = "0").
#'
#' @param q8 the integer variable with continuous scale indicates the number of
#'    meals (solid, semi-solid or soft foods) the child ate yesterday.
#'
#' @param q6bnum This parameter indicates the number of infant formula feeding
#'   the children received in previous days.
#'
#' @param q6cnum This parameter holds information about the frequency of milk
#'    related food feeding the child received yesterday (milk from animals, such
#'    as fresh, tinned, or powdered milk)
#'
#' @param q6dnum This parameter presents the information about the frequency of
#'    yogurt drinks the child received in the previous day.
#'
#' @param mmfbf_6to8 to The binary variable indicates that the 6-8 months
#'    breastfed child received minimum meal frequency or not.
#'
#' @param mmfbf_9to23 The binary variable indicates that the 9-23 months
#'    breastfed child received minimum meal frequency or not.
#'
#' @param mmf_bf to The binary variable indicates that the overall breastfed
#'    child received minimum meal frequency or not.
#'
#' @param nonbf_frq the integer variables with continuous scale presents the
#'    frequency of non-Breastfed child meal including milk feeding.
#'
#' @param mmf_nonbf The binary variable indicates that the overall non-breastfed
#'    child received minimum meal frequency or not.
#'
#' @return binary variables indicate child get the minimum meal frequency
#'    (mmf =1 ) or not (mmf = 0)
#'
#'
#' @examples
#'
#' # Minimum Meal Frequency
#'
#'   df <- iycfData
#'
#'   # breastfeeding children
#'   mmfbf_6to8 <- get_mmf_bf_6to8(df$child_bfyest,
#'                                 df$child_food_freq,
#'                                 df$calc_age_months)
#'   mmfbf_9to23 <- get_mmf_bf_9to23(df$child_bfyest,
#'                                   df$child_food_freq,
#'                                   df$calc_age_months)
#'
#'   mmf_bf <- get_mmf_bf(mmfbf_6to8, mmfbf_9to23)
#'
#'   # non-breastfeeding children
#'   nonbf_frq <- get_nonbf_frq(df$child_bms_freq,
#'                              df$child_milk_freq,
#'                              df$child_mproduct_freq,
#'                              df$child_food_freq)
#'
#'   mmf_nonbf <- get_mmf_nonbf(df$child_bfyest,
#'                              df$child_food_freq,
#'                              nonbf_frq,
#'                              df$calc_age_months)
#'
#'
#'   # all children
#'   mmf <- get_mmf(mmf_bf, mmf_nonbf)
#'
#'
#'
#' @author Nicholus Tint Zaw
#' @export
#' @rdname get_mmf
#'
#'
#################################################################################




################################################################################
# Child - MMF 6-23 months
get_mmf <- function(mmf_bf, mmf_nonbf){
  ## Checkers ----
  ## (replaces the old NULL check, which tested the function get_mmf_bf
  ## instead of the argument mmf_bf)
  check_if_number(mmf_bf)
  check_if_number(mmf_nonbf)

  ## Set invalid codes to NA ----
  mmf_bf <- check_if_valid_code(mmf_bf, valid = c(0, 1))
  mmf_nonbf <- check_if_valid_code(mmf_nonbf, valid = c(0, 1))

  ## Recode ----
  mmf <- ifelse((!is.na(mmf_bf) & mmf_bf == 1) |
                  (!is.na(mmf_nonbf) & mmf_nonbf == 1), 1,
                ifelse(is.na(mmf_bf) & is.na(mmf_nonbf), NA, 0))

  ## Return mmf ----
  mmf
}


################################################################################
############################# Breastfeeding Child ##############################
################################################################################
#
#' @export
#' @rdname get_mmf
#'
################################################################################
# Breastfeeding Child - MMF 6-23 months
get_mmf_bf <- function(mmfbf_6to8, mmfbf_9to23){
  ## Checkers ----
  check_if_number(mmfbf_6to8)
  check_if_number(mmfbf_9to23)

  ## Set invalid codes to NA ----
  mmfbf_6to8 <- check_if_valid_code(mmfbf_6to8, valid = c(0, 1))
  mmfbf_9to23 <- check_if_valid_code(mmfbf_9to23, valid = c(0, 1))

  ## Recode ----
  mmf_bf <- ifelse((!is.na(mmfbf_6to8) & mmfbf_6to8 == 1) |
                     (!is.na(mmfbf_9to23) & mmfbf_9to23 == 1), 1,
                   ifelse(is.na(mmfbf_6to8) & is.na(mmfbf_9to23), NA, 0))

  ## Return mmf_bf ----
  mmf_bf
}

################################################################################
#
#' @export
#' @rdname get_mmf
#'
################################################################################
# Breastfeeding Child - MMF 6-8 months
get_mmf_bf_6to8 <- function(q4, q8, age){
  ## Checkers ----
  check_if_number(q4)
  check_if_number(q8)
  check_if_number(age)

  ## Set invalid codes, counts and ages to NA ----
  q4 <- check_if_valid_code(q4, valid = c(0, 1))
  q8 <- check_if_valid_code(q8, valid = 0:20)
  age <- check_if_valid_age(age)

  ## Recode ----
  mmfbf_6to8 <- ifelse(q4 == 1 & age >= 6 & age < 9 & q8 >= 2, 1,
                       ifelse(age < 6, NA,
                              ifelse(age >= 9, NA, 0)))

  mmfbf_6to8 <- ifelse(is.na(q4) | is.na(q8) | is.na(age) | q4 == 0,
                       NA, mmfbf_6to8)

  ## Return mmfbf_6to8 ----
  mmfbf_6to8
}

################################################################################
#
#' @export
#' @rdname get_mmf
#'
################################################################################
# Breastfeeding Child - MMF 9-23 months
get_mmf_bf_9to23 <- function(q4, q8, age){
  ## Checkers ----
  check_if_number(q4)
  check_if_number(q8)
  check_if_number(age)

  ## Set invalid codes, counts and ages to NA ----
  q4 <- check_if_valid_code(q4, valid = c(0, 1))
  q8 <- check_if_valid_code(q8, valid = 0:20)
  age <- check_if_valid_age(age)

  ## Recode ----
  mmfbf_9to23 <- ifelse(q4 == 1 & age >= 9 & age < 24 & q8 >= 3, 1,
                        ifelse(age < 9, NA,
                               ifelse(age >= 24, NA, 0)))

  mmfbf_9to23 <- ifelse(is.na(q4) | is.na(q8) | is.na(age) | q4 == 0,
                        NA, mmfbf_9to23)

  ## Return mmfbf_9to23 ----
  mmfbf_9to23
}


################################################################################
########################### Non-Breastfeeding Child ############################
################################################################################
#
#' @export
#' @rdname get_mmf
#'
################################################################################
# Non-Breastfeeding Child - MMF 6-23 months
get_mmf_nonbf <- function(q4, q8, nonbf_frq, age){
  ## Checkers ----
  check_if_number(q4)
  check_if_number(q8)
  check_if_number(nonbf_frq)
  check_if_number(age)

  ## Set invalid codes, counts and ages to NA ----
  ## (nonbf_frq is the sum of up to 4 counts of 0-20 each, so 0-80)
  q4 <- check_if_valid_code(q4, valid = c(0, 1))
  q8 <- check_if_valid_code(q8, valid = 0:20)
  nonbf_frq <- check_if_valid_code(nonbf_frq, valid = 0:80)
  age <- check_if_valid_age(age)

  ## Recode ----
  mmf_nonbf <- ifelse(q4 == 0 & age >= 6 & age < 24 & q8 >= 1 &
                        nonbf_frq >= 4, 1,
                      ifelse(age < 6, NA,
                             ifelse(age >= 24, NA, 0)))

  mmf_nonbf <- ifelse(is.na(q4) | is.na(q8) | is.na(nonbf_frq) |
                        is.na(age) | q4 == 1, NA, mmf_nonbf)

  ## Return mmf_nonbf ----
  mmf_nonbf
}


################################################################################
#
#' @export
#' @rdname get_mmf
#'
################################################################################
# Non-Breastfeeding Child Meal Frequency including milk feeding
get_nonbf_frq <- function(q6bnum, q6cnum, q6dnum, q8){
  ## Checkers ----
  check_if_number(q6bnum)
  check_if_number(q6cnum)
  check_if_number(q6dnum)
  check_if_number(q8)

  ## Set invalid counts to NA ----
  q6bnum <- check_if_valid_code(q6bnum, valid = 0:20)
  q6cnum <- check_if_valid_code(q6cnum, valid = 0:20)
  q6dnum <- check_if_valid_code(q6dnum, valid = 0:20)
  q8 <- check_if_valid_code(q8, valid = 0:20)

  ## Sum milk and meal feeds ----
  nonbf_frq <- as.data.frame(
    cbind(q6bnum, q6cnum, q6dnum, q8)
  )

  nonbf_frq <- rowSums(nonbf_frq, na.rm = FALSE)

  ## Return nonbf_frq ----
  nonbf_frq
}


