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
#'  solid <- list("child_rice", "child_potatoes", "child_pumpkin",
#'                "child_beans", "child_leafyveg", "child_mango", "child_fruit",
#'                "child_organ", "child_beef", "child_fish", "child_insects",
#'                "child_eggs", "child_yogurt", "child_fat", "child_plam",
#'                "child_sweets", "child_condiments")
#'
#'  df$solid_food <- dummy_gen(df, solid)
#'
#'  isssf <- get_isssf(df$solid_food, df$calc_age_months)
#'
#'
#' # Egg and/or Flesh food consumption (6-23 months)
#' # Egg and/or Flesh food consumption previous day - yes/no
#'  egg_meat <- list("child_organ", "child_beef", "child_fish", "child_insects",
#'                   "child_eggs")
#'
#'  df$egg_meat <- dummy_gen(df, egg_meat)
#'
#'  eff <- get_eff(df$egg_meat, df$calc_age_months)
#'
#' # Sweet beverage consumption (6-23 months)
#' # Sweet beverage consumption previous day - yes/no
#'  sweet <- list("child_juice", "child_broth", "child_milk", "child_mproduct")
#'
#'  df$sweet <- dummy_gen(df, sweet)
#'
#'  swb <- get_swb(df$sweet, df$calc_age_months)
#'
#' # Unhealthy food consumption (6-23 months)
#' # Unhealthy food consumption previous day - yes/no
#'  unhealthy <- list("child_sweets", "child_condiments")
#'
#'  df$unhealthy <- dummy_gen(df, unhealthy)
#'
#'  ufc <- get_ufc(df$unhealthy, df$calc_age_months)
#'
#' # Zero vegetable or fruit consumption (6-23 months)
#' # Zero vegetable or fruit consumption previous day - yes/no
#'  vege_fruit <- list("child_pumpkin", "child_leafyveg", "child_mango",
#'                     "child_fruit")
#'
#'  df$vege_fruit <- dummy_gen(df, vege_fruit)
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

  if(!is.null(solid_food) & !is.null(age)){

    isssf <- ifelse(age >= 6 & age < 9 & solid_food == 1, 1, 0)

    return(isssf)
  }
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

  if(!is.null(egg_meat) & !is.null(age)){

    eff <- ifelse(age >= 6 & age < 24 & egg_meat == 1, 1, 0)

    return(eff)
  }
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

  if(!is.null(sweet) & !is.null(age)){

    swb <- ifelse(age >= 6 & age < 24 & sweet == 1, 1, 0)

    return(swb)
  }
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

  if(!is.null(unhealthy) & !is.null(age)){

    ufc <- ifelse(age >= 6 & age < 24 & unhealthy == 1, 1, 0)

    return(ufc)
  }
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

  if(!is.null(vege_fruit) & !is.null(age)){

    zvf <- ifelse(age >= 6 & age < 24 & vege_fruit == 0, 1, 0)

    return(zvf)
  }
}


################################################################################
# dummy var generator
dummy_gen <- function(df, var_list){

  if(!is.null(var_list)){

    dummy_var <- NA

    for (name in var_list){

      print(name)

      dummy_var <- ifelse(df[name] == 1, 1, dummy_var)
    }

    return(dummy_var)
  }
}
