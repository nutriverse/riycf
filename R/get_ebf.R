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
#'  liquid <- list(df$child_vitdrop, df$child_ors, df$child_water,
#'                 df$child_juice, df$child_broth, df$child_porridge,
#'                 df$child_bms, df$child_milk, df$child_mproduct,
#'                 df$child_liquid)
#'
#'  df$liquid_food <- get_dummy(var_list = liquid)
#'
#'
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

    ebf <- ifelse(age < 6 & q4 == 1 & liquid_food == 0 & solid_food == 0, 1,
                  ifelse(age >= 6, NA, 0))

    ebf <- ifelse(is.na(q4) | is.na(age) | is.na(liquid_food) |
                    is.na(solid_food), NA, ebf)

    return(ebf)
  }
}

#################################################################################

