################################################################################
#
#' @title Construct other breastfeeding related indicators (from IYCF)
#'
#' @description Identification of individual 0-23 months old children
#'    breastfeeding status
#'
#' @param age This parameter holds the information about child age in the month
#'    format.
#'
#' @param solid_food The binary variable which mentioned that the child was
#'    received any type of solid foods yesterday (yes = 1 or no = 0).
#'
#' @param egg_meat The binary variable which mentioned that the child was
#'    received any egg and/or flesh food yesterday
#'
#' @param sweet The binary variable which mentioned that the child was
#'    received any sweet beverage foods yesterday
#'
#' @param unhealthy The binary variable which mentioned that the child was
#'    received the unhealthy food consumption: sweets and instant foods
#'
#' @param vege_fruit The binary variable which mentioned that the child was
#'    received any vegetables or fruits yesterday
#'
#' @return binary variables indicate child met the respective complementary
#'    feeding status or not (yes = 1 or no = 0)
#'
#'
#' @examples
#'
#'   df <- iycfData
#'
#' # Introduction of Solid, Semi-solid or soft foods (6-8 months)
#' # Solid food consumption previous day - yes/no
#'  solid <- list(df$child_rice, df$child_potatoes, df$child_pumpkin,
#'                df$child_beans, df$child_leafyveg, df$child_mango,
#'                df$child_fruit, df$child_organ, df$child_beef, df$child_fish,
#'                df$child_insects, df$child_eggs, df$child_yogurt,
#'                df$child_fat, df$child_plam, df$child_sweets,
#'                df$child_condiments)
#'
#'  df$solid_food <- get_dummy(var_list = solid)
#'
#'  isssf <- get_isssf(df$solid_food, df$calc_age_months)
#'
#'
#' # Egg and/or Flesh food consumption (6-23 months)
#' # Egg and/or Flesh food consumption previous day - yes/no
#'  egg_meat <- list(df$child_organ, df$child_beef, df$child_fish,
#'                   df$child_insects, df$child_eggs)
#'
#'  df$egg_meat <- get_dummy(var_list = egg_meat)
#'
#'  eff <- get_eff(df$egg_meat, df$calc_age_months)
#'
#' # Sweet beverage consumption (6-23 months)
#' # Sweet beverage consumption previous day - yes/no
#'  sweet <- list(df$child_milk_sweet, df$child_mproduct_sweet,
#'                df$child_chocolate, df$child_juice, df$child_soda,
#'                df$child_tea_sweet, df$child_oth_drink_swee)
#'
#'  df$sweet <- get_dummy(var_list = sweet)
#'
#'  swb <- get_swb(df$sweet, df$calc_age_months)
#'
#' # Unhealthy food consumption (6-23 months)
#' # Unhealthy food consumption previous day - yes/no
#'  unhealthy <- list(df$child_sweets, df$child_condiments)
#'
#'  df$unhealthy <- get_dummy(var_list = unhealthy)
#'
#'  ufc <- get_ufc(df$unhealthy, df$calc_age_months)
#'
#' # Zero vegetable or fruit consumption (6-23 months)
#' # Zero vegetable or fruit consumption previous day - yes/no
#'  vege_fruit <- list(df$child_pumpkin, df$child_leafyveg, df$child_mango,
#'                     df$child_fruit)
#'
#'  df$vege_fruit <- get_dummy(var_list = vege_fruit)
#'
#'  zvf <- get_zvf(df$vege_fruit, df$calc_age_months)
#'
#' @author Nicholus Tint Zaw
#' @export
#' @rdname get_othercf
#'
#'
#################################################################################

# Introduction of Solid, Semi-solid or soft foods (6-8 months)
get_isssf <- function(solid_food, age){
  ## Checkers ----
  check_if_number(solid_food)
  check_if_number(age)

  ## Set invalid codes and out-of-range ages to NA ----
  solid_food <- check_if_valid_code(solid_food, valid = c(0, 1))
  age <- check_if_valid_age(age)

  ## Recode ----
  isssf <- ifelse(age >= 6 & age < 9 & solid_food == 1, 1,
                  ifelse(age < 6, NA,
                         ifelse(age >= 9, NA, 0)))

  isssf <- ifelse(is.na(solid_food) | is.na(age), NA, isssf)

  ## Return isssf ----
  isssf
}

################################################################################
#
#' @export
#' @rdname get_othercf
#'
#
################################################################################

# Egg and/or Flesh food consumption (6-23 months)
get_eff <- function(egg_meat, age){
  ## Checkers ----
  check_if_number(egg_meat)
  check_if_number(age)

  ## Set invalid codes and out-of-range ages to NA ----
  egg_meat <- check_if_valid_code(egg_meat, valid = c(0, 1))
  age <- check_if_valid_age(age)

  ## Recode ----
  eff <- ifelse(age >= 6 & age < 24 & egg_meat == 1, 1,
                ifelse(age < 6, NA,
                       ifelse(age >= 24, NA, 0)))

  eff <- ifelse(is.na(egg_meat) | is.na(age), NA, eff)

  ## Return eff ----
  eff
}

################################################################################
#
#' @export
#' @rdname get_othercf
#'
#
################################################################################

# Sweet beverage consumption (6-23 months)
get_swb <- function(sweet, age){
  ## Checkers ----
  check_if_number(sweet)
  check_if_number(age)

  ## Set invalid codes and out-of-range ages to NA ----
  sweet <- check_if_valid_code(sweet, valid = c(0, 1))
  age <- check_if_valid_age(age)

  ## Recode ----
  swb <- ifelse(age >= 6 & age < 24 & sweet == 1, 1,
                ifelse(age < 6, NA,
                       ifelse(age >= 24, NA, 0)))

  swb <- ifelse(is.na(sweet) | is.na(age), NA, swb)

  ## Return swb ----
  swb
}

################################################################################
#
#' @export
#' @rdname get_othercf
#'
#
################################################################################

# Unhealthy food consumption (6-23 months)
get_ufc <- function(unhealthy, age){
  ## Checkers ----
  check_if_number(unhealthy)
  check_if_number(age)

  ## Set invalid codes and out-of-range ages to NA ----
  unhealthy <- check_if_valid_code(unhealthy, valid = c(0, 1))
  age <- check_if_valid_age(age)

  ## Recode ----
  ufc <- ifelse(age >= 6 & age < 24 & unhealthy == 1, 1,
                ifelse(age < 6, NA,
                       ifelse(age >= 24, NA, 0)))

  ufc <- ifelse(is.na(unhealthy) | is.na(age), NA, ufc)

  ## Return ufc ----
  ufc
}


################################################################################
#
#' @export
#' @rdname get_othercf
#'
#
################################################################################

# Zero vegetable or fruit consumption (6-23 months)
get_zvf <- function(vege_fruit, age){
  ## Checkers ----
  check_if_number(vege_fruit)
  check_if_number(age)

  ## Set invalid codes and out-of-range ages to NA ----
  vege_fruit <- check_if_valid_code(vege_fruit, valid = c(0, 1))
  age <- check_if_valid_age(age)

  ## Recode ----
  zvf <- ifelse(age >= 6 & age < 24 & vege_fruit == 0, 1,
                ifelse(age < 6, NA,
                       ifelse(age >= 24, NA, 0)))

  zvf <- ifelse(is.na(vege_fruit) | is.na(age), NA, zvf)

  ## Return zvf ----
  zvf
}


################################################################################
