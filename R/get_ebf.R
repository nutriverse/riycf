#' 
#' @title Construct exclusive breastfeeding status for under 6 months old child
#'
#' @description Identification of individual 0-5 months old children exclusive
#'    breastfeeding status
#'
#' @param age This parameter holds the information about child age in the month
#'    format.
#' @param q4 The binary variable which mentioned that the child was receiving
#'    breastfeeding in the previous day (yes = "1", no = "0").
#' @param liquid_food The binary variable which mentioned that the child was
#'    receiving any type of liquid foods beside breastfeeding yesterday
#'    (yes = 1 or no = 0).
#' @param solid_food The binary variable which mentioned that the child was
#'    receiving any type of solid foods yesterday (yes = 1 or no = 0).
#'
#' @returns A vector of binary values indicating whether a child was exclusively
#'   breastfed or not during the previous day (1 = Yes or 0 = No).
#'
#' @examples
#'
#'  df <- iycfData
#'
#' # Liquid consumption previous day - yes/no (Questions 6A to 6J)
#'  liquid <- list(df$child_water, df$child_bms, df$child_milk,
#'                 df$child_mproduct, df$child_chocolate, df$child_juice,
#'                 df$child_soda, df$child_tea, df$child_broth,
#'                 df$child_oth_drink)
#'
#'  df$liquid_food <- get_dummy(var_list = liquid)
#'
#'
#' # Solid food consumption previous day - yes/no (Questions 7A to 7R)
#'  solid <- list(df$child_yogurt, df$child_rice, df$child_pumpkin,
#'                df$child_potatoes, df$child_leafyveg, df$child_oth_veg,
#'                df$child_mango, df$child_fruit, df$child_organ,
#'                df$child_processmeat, df$child_beef, df$child_eggs,
#'                df$child_fish, df$child_beans, df$child_cheese,
#'                df$child_sweets, df$child_snack, df$child_oth_food)
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
#'

# Exclusive Breastfeeding
get_ebf <- function(q4, age, liquid_food, solid_food){

  if(!is.null(q4) & !is.null(age) & !is.null(liquid_food) &
     !is.null(solid_food)){

    ebf <- ifelse(age < 6 & q4 == 1 & liquid_food == 0 & solid_food == 0, 1,
                  ifelse(age >= 6, NA, 0))

    ebf <- ifelse(is.na(q4) | is.na(age) | is.na(liquid_food) |
                    is.na(solid_food), NA, ebf)

    ebf
  }
}
