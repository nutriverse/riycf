################################################################################
#
#' @title Construct the food groups consumption score (from Minimum Dietary
#'    Diversity 6-23 months (MDD))
#'
#' @description Identification of individual 6-23 months old children dietary
#'    practices require the diversity of food group consumption. This command
#'    contains the series of function to calculate the individual food group
#'    (8 in total) and construct the total food group consumption score required
#'    to calculate Minimum Dietary Diversity (MDD)
#'
#' @param age This parameter holds the information about child age in the month
#'    format.
#'
#' @param q4 The binary variable which mentioned that the child was receiving
#'    breastfeeding in the previous day (yes = "1", no = "0").
#'
#' The following variables were use for solid food group construction and it
#' should be code as "1" for meeting the condition (consumed in last 24 hours),
#' "0" for didn't (not consumed in last 24 hours), and "9" for don't know.
#'
#' @param q7b Porridge, bread, rice, noodles, and pasta.
#'
#' @param q7d Plantains, white potatoes, white yams, manioc, cassava
#'
#' @param q7n Beans, peas, lentils, nuts , seeds
#'
#' @param q6b Infant formula
#'
#' @param q6c Milk from animals, such as fresh, tinned or powdered milk
#'
#' @param q6d Yogurt drinks
#'
#' @param q7a Yogurt, other than yogurt drinks
#'
#' @param q7o Hard or soft cheese
#'
#' @param q7i Liver, kidney, heart
#'
#' @param q7j Sausages, hot dogs, ham, bacon, salami, canned meat
#'
#' @param q7k Any other meat, such as beef, pork, lamb, goat, chicken, duck
#'
#' @param q7m Fresh fish, dried fish or shellfish
#'
#' @param q7l Eggs
#'
#' @param q7c Pumpkin, carrots, sweet red peppers, squash or sweet potatoes
#'    that are yellow or orange inside
#'
#' @param q7e Dark green leafy vegetables
#'
#' @param q7g Ripe mangoes, ripe papayas
#'
#' @param q7h Any other fruits
#'
#' @param q7f Any other vegetables
#'
#' @param breastmilk child received breastfeeding or not
#'
#' @param  grains child consumed grains food group or not
#'
#' @param pulses child consumed pulses food group or not
#'
#' @param dairy child consumed diary and milk product food group or not
#'
#' @param meat child consumed meat food group or not
#'
#' @param eggs child consumed eggs food group or not
#'
#' @param vita_fruveg child consumed vitamin A-rich fruits and vegetables or not
#'
#' @param oth_fruveg child consumed fruits and vegetables or not
#'
#' @param var_list list of food consumption variables
#'
#' @return Each command produces the respective vector with "0" and "1" binary
#'    result. For the `food_score` function, the return result was integer value
#'    at continuous scale (0-8 as the IYCF food groups have 8 food groups).
#' The return variable's detailed explanation is mentioned below.
#'
#'
#'
#' @examples
#'
#' # Individual Food Group Variable Construction
#'
#'  df <- iycfData
#'
#' # Individual Food Group Specific Function
#'  breastmilk <- fg_breastmilk(df$child_bfyest, df$calc_age_months)
#'
#'  grains <- fg_grains(df$child_rice, df$child_potatoes, df$calc_age_months)
#'
#'  pulses <- fg_pulses(df$child_beans, df$calc_age_months)
#'
#'  dairy <- fg_milk(df$child_bms, df$child_milk, df$child_mproduct,
#'                   df$child_yogurt, df$child_yogurt, df$calc_age_months)
#'
#'  meat <- fg_meat(df$child_organ, df$child_insects, df$child_beef,
#'                  df$child_fish, df$calc_age_months)
#'
#'  eggs <- fg_egg(df$child_eggs, df$calc_age_months)
#'
#'  vita_fruveg <- fg_vita_fruveg(df$child_pumpkin, df$child_leafyveg,
#'                                df$child_mango, df$calc_age_months)
#'
#'  oth_fruveg <- fg_oth_fruveg(df$child_fruit, df$child_fruit,
#'                              df$calc_age_months)
#'
#' # General Dummy variable generator function
#'  breastmilk <- dummy_gen(var_list = list(df$child_bfyest))
#'
#'  grains <- dummy_gen(var_list = list(df$child_rice, df$child_potatoes))
#'
#'  pulses <- dummy_gen(var_list = list(df$child_beans))
#'
#'  dairy_list <- list(df$child_bms, df$child_milk,
#'                     df$child_mproduct, df$child_yogurt,
#'                     df$child_yogurt)
#'
#'  dairy <- dummy_gen(var_list = dairy_list)
#'
#'  meat_list <- list(df$child_organ, df$child_insects,
#'                    df$child_beef, df$child_fish)
#'
#'  meat <- dummy_gen(var_list = meat_list)
#'
#'  eggs <- dummy_gen(var_list = list(df$child_eggs))
#'
#'  vita_fruveg_list <- list(df$child_pumpkin,
#'                           df$child_leafyveg,
#'                           df$child_mango)
#'
#'  vita_fruveg <- dummy_gen(var_list = vita_fruveg_list)
#'
#'  oth_fruveg <- dummy_gen(var_list = list(df$child_fruit, df$child_fruit))
#'
#'  # Calculate Food Consumption Score
#'  food_score <- fg_score(breastmilk, grains, pulses, dairy, meat,
#'                         eggs, vita_fruveg, oth_fruveg)
#'
#'
#' @author Nicholus Tint Zaw
#' @export
#' @rdname fg_score
#'
#'
#################################################################################

## Food Groups Score ##
fg_score <- function(breastmilk, grains, pulses, dairy, meat, eggs,
                     vita_fruveg, oth_fruveg){

  if(!is.null(breastmilk) & !is.null(grains) & !is.null(pulses) &
     !is.null(dairy) & !is.null(meat) & !is.null(eggs) &
     !is.null(vita_fruveg) & !is.null(oth_fruveg)){

       fg_group <- as.data.frame(
         cbind(breastmilk, grains, pulses, dairy, meat, eggs, vita_fruveg, oth_fruveg)
         )

       food_score <- rowSums(fg_group, na.rm = FALSE)

       return(food_score)
  }
}

################################################################################
################################################################################

## Food Groups Construction ##

################################################################################
#
#' @export
#' @rdname fg_score
#'
#
################################################################################

# Breast milk
fg_breastmilk <- function(q4, age){
  if(!is.null(q4)){
    breastmilk <- ifelse(age >= 6 & age < 24 & q4 == 1, 1, 0)

    return(breastmilk)
  }
}

################################################################################
#
#' @export
#' @rdname fg_score
#'
#
################################################################################

# Grains
fg_grains <- function(q7b, q7d, age){
  if(!is.null(q7b) & !is.null(q7d)){
    grains <- ifelse(age >= 6 & age < 24 &
                              (q7b == 1 | q7d == 1), 1, 0)

    return(grains)
  }
}

################################################################################
#
#' @export
#' @rdname fg_score
#'
#
################################################################################

# Pulses
fg_pulses <- function(q7n, age){
  if(!is.null(q7n)){
    pulses <- ifelse(age >= 6 & age < 24 & q7n == 1, 1, 0)

    return(pulses)
  }
}

################################################################################
#
#' @export
#' @rdname fg_score
#'
#
################################################################################

# Dairy Products
fg_milk <- function(q6b, q6c, q6d, q7a, q7o, age){
  if(!is.null(q6b) & !is.null(q6c) & !is.null(q6d) & !is.null(q7a) &
     !is.null(q7o)){
    dairy <- ifelse(age >= 6 & age < 24 &
                      (q6b == 1 | q6c == 1 | q6d == 1 |
                         q7a == 1 | q7o == 1), 1, 0)

    return(dairy)
  }
}

################################################################################
#
#' @export
#' @rdname fg_score
#'
#
################################################################################

# MEat
fg_meat <- function(q7i, q7j, q7k, q7m, age){
  if(!is.null(q7i) & !is.null(q7j) & !is.null(q7k) & !is.null(q7m)){
    meat <- ifelse(age >= 6 & age < 24 & (q7i == 1 | q7j == 1 |
                                            q7k == 1 | q7m == 1), 1, 0)

    return(meat)
  }
}

################################################################################
#
#' @export
#' @rdname fg_score
#'
#
################################################################################

# Egg
fg_egg <- function(q7l, age){
  if(!is.null(q7l)){
    eggs <- ifelse(age >= 6 & age < 24 & q7l == 1, 1, 0)

    return(eggs)
  }
}

################################################################################
#
#' @export
#' @rdname fg_score
#'
#
################################################################################

# Vit-A Fruits and vegetables
fg_vita_fruveg <- function(q7c, q7e, q7g, age){
  if(!is.null(q7c) & !is.null(q7e) & !is.null(q7g)){
    vita_fruveg <- ifelse(age >= 6 & age < 24 &
                            (q7c == 1 | q7e == 1 | q7g == 1), 1, 0)

    return(vita_fruveg)
  }
}

################################################################################
#
#' @export
#' @rdname fg_score
#'
#
################################################################################
# Other Fruits and vegetables
fg_oth_fruveg <- function(q7h, q7f, age){
  if(!is.null(q7h) & !is.null(q7f)){
    oth_fruveg <- ifelse(age >= 6 & age < 24 & (q7h == 1 | q7f == 1), 1, 0) # apply NA for age out of range

    return(oth_fruveg)
  }
}


################################################################################
#
#' @export
#' @rdname fg_score
#'
#
################################################################################
# dummy var generator
dummy_gen <- function(var_list = NULL){

  if(!is.null(var_list)){

    vect_df <- as.data.frame(do.call(cbind, var_list))

    total <- rowSums(vect_df, na.rm = FALSE)

    dummy_var <- ifelse(total > 0, 1,
                        ifelse(is.na(total), NA, 0))

    return(dummy_var)
  }
}

