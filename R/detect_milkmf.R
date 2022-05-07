################################################################################
#
#' @title Determining Minimum Milk Feeding Frequency
#'
#' @description Identification of individual 6-23 months old children minimum
#'    milk feeding frequency for non-breastfeed children 6-23 months (MMFF)
#'
#' @param age This parameter holds the information about child age in the month
#'    format.
#'
#' @param q4 The binary variable which mentioned that the child was receiving
#'    breastfeeding in the previous day (yes = "1", no = "0").
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
#' @param q7anum the integer variable with continuous scale represents the
#'    frequency of yogurt (solid or semi-solid) the child consumed yesterday.
#'
#' @param milk_frq the integer variable with continuous scale represents all the
#'    milk consumption frequency (beside breastmilk)
#'
#'
#' @return
#'
#' The output of the `get_mmff` and `get_milk_frq` variables includes the
#' following added variable.
#'
#'    **Variables** | **Description**
#'    :--- | :---
#'    *mf_frq* | continuous variables indicate non-breastfeed child consume number of milk and milk-product food feeding
#'    *mmff* | binary variables indicate child get the minimum milk feeding frequency (mmff =1 ) or not (mmff = 0)
#'
#'
#'
#' @examples
#'
#' # Calculate Non-breastfeed Children Milk Frequency
#'  df <- iycfData
#'
#'  df$milk_frq <- get_milk_frq(df$child_bms_freq, df$child_milk_freq,
#'                              df$child_mproduct_freq, df$child_yogurt)
#'
#' # MMFF
#' df$mmff <- get_mmff(df$child_bfyest, df$calc_age_months, df$milk_frq)
#'
#'
#' @author Nicholus Tint Zaw
#' @export
#' @rdname get_mmff
#'
#################################################################################




################################################################################
# Minimum Milk Feeding Frequency
get_mmff <- function(q4, age, milk_frq){

  if(!is.null(q4) & !is.null(milk_frq) & !is.null(age)){

    mmff <- ifelse(q4 == 0 & age >= 6 & age < 24 & milk_frq >= 2, 1,
                   ifelse(age < 6, NA,
                          ifelse(age >= 24, NA, 0)))

    mmff <- ifelse(is.na(q4) | is.na(age) | is.na(milk_frq) | q4 == 1,
                   NA, mmff)

    return(mmff)

  } else{
    stop("One or more of the input variables was missing!")
  }
}

################################################################################
#
#' @export
#' @rdname get_mmff
#'
################################################################################
# Milk Feeding Frequency Score
get_milk_frq <- function(q6bnum, q6cnum, q6dnum, q7anum){

  if(!is.null(q6bnum) & !is.null(q6cnum) & !is.null(q6dnum) &
     !is.null(q7anum)){

    milk_frq <- as.data.frame(
      cbind(q6bnum, q6cnum, q6dnum, q7anum)
    )

    milk_frq <- rowSums(milk_frq, na.rm = FALSE)

    return(milk_frq)

  } else{
    stop("One or more of the input variables was missing!")
  }

}

