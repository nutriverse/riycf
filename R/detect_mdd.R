################################################################################
#
#' @title Determining Minimum Dietary Diversity (MDD)
#'
#' @description Identification of individual 6-23 months old children dietary
#' practices meet the minimum requirement of dietary diversity (at least 5 food
#' group out of 8 food groups)
#'
#' @param age This parameter holds the information about child age in the month
#'    format, and it should be the continuous variable. It is either generated
#'    from the child's birth date or self-reported answers from the caregivers.
#'    If this was generated from the calculation based on the child's date of birth,
#'    the decimal class of the variable would be the decimal numeric format.
#'
#' @param food_score continuous variables indicate child consumed number of food
#'    groups including solid, semi-solid or soft foods and breastfeeding (total
#'    8 food groups)
#'
#'
#' @return binary variables indicate child get the minimum dietary diversity
#'    (mdd =1 ) or not (mdd = 0)
#'
#' @examples
#'
#' # Minimum Dietary Diversity
#'   mdd <- get_mdd(df$food_score, df$calc_age_months)
#'
#'
#'
#'
#' @author Nicholus Tint Zaw
#' @export
#' @rdname get_mdd
#'
#'
#################################################################################


get_mdd <- function(food_score, age){
  if(!is.null(age) & !is.null(food_score)){

    mdd <- ifelse(age >= 6 & age < 24 & food_score >= 5, 1 , 0)

    return(mdd)
  }
}

