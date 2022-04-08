################################################################################
#
#' @title Construct the food groups consumption score (from Minimum Dietary Diversity 6-23 months (MDD))
#'
#' @description Identification of individual 6-23 months old children dietary
#' practices require the diversity of food group consumption. This command
#' contains the series of function to calculate the individual food group (8 in
#' total) and construct the total food group consumption score required to
#' calculate Minimum Dietary Diversity (MDD)
#'
#'
#' @param age This parameter holds the information about child age in the month
#'    format, and it should be the continuous variable. It is either generated
#'    from the child's birth date or self-reported answers from the caregivers.
#'    If this was generated from the calculation based on the child's date of birth,
#'    the decimal class of the variable would be the decimal numeric format.
#'
#' @param q4 The binary variable which mentioned that the child was receiving
#'    breastfeeding yesterday (previous days), which were coded as "1", or
#'    not - as "0".
#'
#' The following variables were use for solid food group construction and it
#' should be code as "1" for meeting the condition (consumed in last 24 hours),
#' "0" for didn't (not consumed in last 24 hours), and "9" for don't know.
#' The value coding pattern applied the same for all of the following parameters
#' mentioned for this `fg_score` function.
#'
#' The following parameter input variables must contains the children information
#' received any food items yesterday;
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
#'
#' The following input parameters will be resulted after calculation of
#' individual food group using above parameters.
#'
#' @param breastmilk child received breastfeeding or not
#'
#' @param  grains child consumed grains food group or not
#'
#' @param pulses child consumed pulses food group or not
#'
#' @param diary child consumed diary and milk product food group or not
#'
#' @param meat child consumed meat food group or not
#'
#' @param eggs child consumed eggs food group or not
#'
#' @param vita_fruveg child consumed vitamin A-rich fruits and vegetables or not
#'
#' @param oth_fruveg child consumed fruits and vegetables or not
#'
#'
#' @return
#' Each command produces the respective vector with "0" and "1" binary code. For
#' the vector which meet the respective function, code as "1" and if didn't code
#' as "0".
#' The return variable's detailed explanation is mentioned below.
#'
#'    **Variables** | **Description**
#'    :--- | :---
#'    *breastmilk* | binary variables indicate child get the breastfeeding at yesterday (breastmilk =1 ) or not (breastmilk = 0)
#'    *grains* | binary variables indicate child get the food group related to grains, white/pale starchy roots, tubers and plantains at yesterday (grains =1 ) or not (grains = 0)
#'    *pulses* | binary variables indicate child get the food group related to beans, peas, lentils, nuts and seeds at yesterday (legumes =1 ) or not (legumes = 0)
#'    *diary* | binary variables indicate child get the food group related to dairy products (milk, infant formula, yogurt, cheese) at yesterday (diary =1 ) or not (diary = 0)
#'    *meat* | binary variables indicate child get the food group related to flesh foods (meat, fish, poultry, organ meats) at yesterday (meat =1 ) or not (meat = 0)
#'    *eggs* | binary variables indicate child get the any kind of eggs at yesterday (eggs =1 ) or not (eggs = 0)
#'    *vita_fruveg* | binary variables indicate child get the food group related to vitamin A-rich fruits and vegetables at yesterday (vita_fruveg =1 ) or not (vita_fruveg)
#'    *oth_fruveg* | binary variables indicate child get the food group related to other fruits and vegetables at yesterday (oth_fruveg =1 ) or not (oth_fruveg = 0)
#'    *food_score* | continuous variables indicate child consume number of food groups including solid, semi-solid or soft foods and breastfeeding
#'
#'
#'
#' @examples
#'
#' # Individual Food Group Variable Construction
#'
#'  df <- cfData
#'
#'  breastmilk <- fg_breastmilk(df$child_bfyest, df$calc_age_months)
#'
#'  grains <- fg_grains(df$child_rice, df$child_potatoes, df$calc_age_months)
#'
#'  pulses <- fg_pulses(df$child_beans, df$calc_age_months)
#'
#'  diary <- fg_milk(df$child_bms, df$child_milk, df$child_mproduct, df$child_yogurt, df$child_yogurt, df$calc_age_months)
#'
#'  meat <- fg_meat(df$child_organ, df$child_insects, df$child_beef, df$child_fish, df$calc_age_months)
#'
#'  eggs <- fg_egg(df$child_eggs, df$calc_age_months)
#'
#'  vita_fruveg <- fg_vita_fruveg(df$child_pumpkin, df$child_leafyveg, df$child_mango, df$calc_age_months)
#'
#'  oth_fruveg <- fg_oth_fruveg(df$child_fruit, df$child_fruit, df$calc_age_months)
#'
#'
#'  # Calculate Food Consumption Score
#'  food_score <- fg_score(breastmilk, grains, pulses, diary, meat,
#'                         eggs, vita_fruveg, oth_fruveg)
#'
#'  df$food_score <- food_score
#'
#' @author Nicholus Tint Zaw
#' @export
#' @rdname fg_score
#'
#'
#################################################################################

## Food Groups Score ##
fg_score <- function(breastmilk, grains, pulses, diary, meat, eggs,
                     vita_fruveg, oth_fruveg){

  if(!is.null(breastmilk) & !is.null(grains) & !is.null(pulses) &
     !is.null(diary) & !is.null(meat) & !is.null(eggs) &
     !is.null(vita_fruveg) & !is.null(oth_fruveg)){

       fg_group <- as.data.frame(
         cbind(breastmilk, grains, pulses, diary, meat, eggs, vita_fruveg, oth_fruveg)
         )

       food_score <- rowSums(fg_group, na.rm = TRUE)

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
    diary <- ifelse(age >= 6 & age < 24 &
                      (q6b == 1 | q6c == 1 | q6d == 1 |
                         q7a == 1 | q7o == 1), 1, 0)

    return(diary)
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
    oth_fruveg <- ifelse(age >= 6 & age < 24 & (q7h == 1 | q7f == 1), 1, 0)

    return(oth_fruveg)
  }
}
