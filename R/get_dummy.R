################################################################################
#
#' @title Construction of Dummy Variable(s)
#'
#' @description generate dummy variables based on the input vector(s) binary
#'    parameters (true = 1 and false = 0). If the observation of one of the
#'    input parameter was true, that observation's newly generated dummy
#'    variable will return as true (1). And, if all input parameters were false,
#'    the return will be false (0).
#'
#' @param var_list list of food consumption variables
#'
#' @return the vector with "0" and "1" binary result.
#'
#' @examples
#'
#' # Individual Food Group Variable Construction
#'
#'  df <- iycfData
#'
#' # Individual Food Group Specific Function
#'  breastmilk <- get_dummy(var_list = list(df$child_bfyest))
#'
#'  grains <- get_dummy(var_list = list(df$child_rice, df$child_potatoes))
#'
#'  pulses <- get_dummy(var_list = list(df$child_beans))
#'
#'  dairy_list <- list(df$child_bms, df$child_milk,
#'                     df$child_mproduct, df$child_yogurt,
#'                     df$child_yogurt)
#'
#'  dairy <- get_dummy(var_list = dairy_list)
#'
#'  meat_list <- list(df$child_organ, df$child_insects,
#'                    df$child_beef, df$child_fish)
#'
#'  meat <- get_dummy(var_list = meat_list)
#'
#'  eggs <- get_dummy(var_list = list(df$child_eggs))
#'
#'  vita_fruveg_list <- list(df$child_pumpkin,
#'                           df$child_leafyveg,
#'                           df$child_mango)
#'
#'  vita_fruveg <- get_dummy(var_list = vita_fruveg_list)
#'
#'  oth_fruveg <- get_dummy(var_list = list(df$child_fruit, df$child_fruit))
#'
#'
#' @author Nicholus Tint Zaw
#' @export
#' @rdname get_dummy
#'
#'
#################################################################################

get_dummy <- function(var_list = NULL){

  if(!is.null(var_list)){

    vect_df <- as.data.frame(do.call(cbind, var_list))

    total <- rowSums(vect_df, na.rm = FALSE)

    dummy_var <- ifelse(total > 0, 1,
                        ifelse(is.na(total), NA, 0))

    return(dummy_var)
  }
}

