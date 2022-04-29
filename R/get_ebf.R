################################################################################
#
#' @title Construct exclusive breastfeeding status for under 6 months old child
#'
#' @description Identification of individual 0-5 months old children exclusive
#'    breastfeeding status
#'
#' @param age This parameter holds the information about child age in the month
#'    format.
#'
#' @param q4 The binary variable which mentioned that the child was receiving
#'    breastfeeding in the previous day (yes = "1", no = "0").
#'
#' @param liquid_food The binary variable which mentioned that the child was
#'    receiving any type of liquid foods beside breastfeeding yesterday
#'    (yes = 1 or no = 0).
#'
#' @param solid_food The binary variable which mentioned that the child was
#'    receiving any type of solid foods yesterday (yes = 1 or no = 0).
#'
#' @return binary variables indicate child was exclusively breastfed or not
#'    during the previous day (ebf = 1 or 0)
#'
#'
#' @examples
#'
#'  df <- iycfData
#'
#' # Liquid consumption previous day - yes/no
#'  liquid <- list("child_vitdrop", "child_ors", "child_water", "child_juice",
#'                 "child_broth", "child_porridge", "child_bms", "child_milk",
#'                 "child_mproduct", "child_liquid")
#'
#'  df$liquid_food <- dummy_gen(df, liquid)
#'
#'
#' # Solid food consumption previous day - yes/no
#'  solid <- list("child_rice", "child_potatoes", "child_pumpkin",
#'                "child_beans", "child_leafyveg", "child_mango", "child_fruit",
#'                "child_organ", "child_beef", "child_fish", "child_insects",
#'                "child_eggs", "child_yogurt", "child_fat", "child_plam",
#'                "child_sweets", "child_condiments")
#'
#'  df$solid_food <- dummy_gen(df, solid)
#'
#'
#' # Child exclusively breastfed
#' ebf <- get_ebf(df$child_bfyest, df$calc_age_months, df$liquid_food,
#'                df$solid_food)
#'
#'
#' @author Nicholus Tint Zaw
#' @export
#' @rdname get_ebf
#'
#'
#################################################################################

# Exclusive Breastfeeding
get_ebf <- function(q4, age, liquid_food, solid_food){

  if(!is.null(q4) & !is.null(age) & !is.null(liquid_food) &
     !is.null(solid_food)){

    ebf <- ifelse(age < 6 & q4 == 1 & liquid_food == 0 & solid_food == 0, 1, 0)

    return(ebf)
  }
}

#################################################################################

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

