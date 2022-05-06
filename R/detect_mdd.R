################################################################################
#
#' @title Determining Minimum Dietary Diversity (MDD)
#'
#' @description Identification of individual 6-23 months old children dietary
#'    practices meet the minimum requirement of dietary diversity (at least 5
#'    food group out of 8 food groups)
#'
#' @param age This parameter holds the information about child age in the month
#'    format.
#'
#' @param food_score the integer variable with continuous scale which indicates
#'    the number of food groups the child consumed in previous day
#'
#' @return binary variables indicate child get the minimum dietary diversity
#'    (mdd = 1 ) or not (mdd = 0)
#'
#'
#' @examples
#' # Minimum Dietary Diversity
#'   df <- iycfData
#'
#'   df$food_score <- round(runif(nrow(df), min = 0, max = 8), 0)
#'
#'   mdd <- get_mdd(df$food_score, df$calc_age_months)
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

    mdd <- ifelse(age >= 6 & age < 24 & food_score >= 5, 1,
                  ifelse(age < 6, NA,
                         ifelse(age >= 24, NA,
                                ifelse(is.na(food_score), NA, 0))))

    return(mdd)
  }
}

