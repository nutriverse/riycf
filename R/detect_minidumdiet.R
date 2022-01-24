################################################################################
#
#' @title Determining Minimum Dietary Practices (from Core IYCF indicatros)
#'
#' @description Identification of individual 6-23 months old children dietary
#' practices meet the minimum requirement in terms of diversity, frequency and
#' acceptable diet
#'
#' There are 4 series of commands to identify the status of 6-23 months children
#' are meet the condition of minimum diet quality in terms of diet diversity,
#' meal frequency, and acceptable diet. The application of commend should follow
#' the order of this package arrangement as the output variables of the first
#' commend will be applied back in a later one. All the argument parameters used
#' in this library were based on the question numbers of the example IYCF
#' questionnaires provided by the WHO guideline.
#'
#' The first commend `get_mdd` is about the identification of minimum dietary
#' diversity.
#'
#' @param df Survey dataset (as an R data.frame) with the following variables
#'    present as mentioned in the table. All variables are compulsory for
#'    function processing. If the dataset did not contain those variables,
#'    please perform data processing to have those variables in the dataset.
#'
#'    **Variables** | **Description**
#'    :--- | :---
#'    *calc_age_months* | child age in months (numeric decimal format)
#'    *child_bf* | child has ever breastfeed or not
#'    *child_eibf* |
#'    *child_eibf_hrs* |
#'    *child_eibf_days* |
#'    *chhild_addbf* |
#'    *chhild_addbf1* |
#'    *chhild_addbf2* |
#'    *chhild_addbf3* |
#'    *chhild_addbf4* |
#'    *chhild_addbf5* |
#'    *chhild_addbf888* |
#'    *child_bfyest* | child breastfeed yesterday
#'    *child_vitdrop* |
#'    *child_ors* | child get ORS yesterday
#'    *child_water* | child get plain water yesterday
#'    *child_juice* | child get Fruit juice or fruit-flavoured drinks including those made from syrups or powders yesterday
#'    *child_broth* |
#'    *child_porridge* |
#'    *child_bms* | child get any type of Infant formula yesterday
#'    *child_bms_freq* | frequency of Infant formula feeding yesterday
#'    *child_milk* | child get milk from animals, such as fresh, tinned or powdered milk yesterday
#'    *child_milk_freq* | frequency of those milk feeding
#'    *child_mproduct* | child get yogurt drinks yesterday
#'    *child_mproduct_freq* | frequency of yogurt drinks
#'    *child_liquid* |
#'    *child_rice* | child get any type of following food yesterday; Porridge, bread, rice, noodles, pasta
#'    *child_potatoes* | child get any type of following food yesterday; Plantains, white potatoes, white yams, manioc, cassava
#'    *child_pumpkin* | child get any type of following food yesterday; Pumpkin, carrots, sweet red peppers, squash or sweet potatoes that are yellow or orange inside
#'    *child_beans* | child get any type of following food yesterday; Beans, peas, lentils, nuts , seeds
#'    *child_leafyveg* | child get any type of following food yesterday; Dark green leafy vegetables
#'    *child_mango* | child get any type of following food yesterday; Ripe mangoes, ripe papayas
#'    *child_fruit* | child get any type of other fruits food yesterday
#'    *child_organ* | child get any type of following food yesterday; Liver, kidney, heart
#'    *child_beef* | child get any type of following food yesterday; Any other meat, such as beef, pork, lamb, goat, chicken, duck
#'    *child_fish* | child get any type of following food yesterday; Fresh fish, dried fish or shellfish
#'    *child_insects* |
#'    *child_eggs* | child get any type of eggs yesterday
#'    *child_yogurt* | child get any type of following food yesterday; Yogurt, other than yogurt drinks
#'    *child_fat* | child get any type of following food yesterday; Hard or soft cheese
#'    *child_plam* | child get any type of following food yesterday; Chips, crisps, puffs, French fries, fried dough, instant noodles
#'    *child_sweets* | child get any type of following food yesterday; Sweet foods such as chocolates, candies, pastries, cakes, biscuits, or frozen treats like ice cream and popsicles
#'    *child_condiments* |
#'    *child_food_freq* | number of times the child eat any solid, semi-solid or soft foods yesterday
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
#' @param q7b The variable contains the children information received any food
#'    items yesterday; Porridge, bread, rice, noodles, and pasta. If yes, the
#'    value was coded as "1" for meeting the condition, "0" for didn't, and
#'    "9" for don't know. The value coding pattern applied the same for all
#'    of the following parameters mentioned for this `get_mdd` function.
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
#' The second command `get_mmf` is used to classify 6-23 months children received
#' the minimum meal frequency based on their breastfeeding status.
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
#' @param q8 For this parameter, you should place the continuous variable,
#'    which contains the number of any solid, semi-solid, or soft foods the
#'    child received on the previous day (both day and night).
#'
#' @param q6bnum This parameter indicates the number of infant formula feeding
#'   the children received in previous days. The variable type should be a
#'   continuous variable.
#'
#' @param q6cnum This parameter holds information about the frequency of the
#'    children receiving the milk from animals, such as fresh, tinned, or
#'    powdered milk in the previous day.
#'
#' @param q6dnum For this parameter, you need to place the continuous variable,
#'    which presents the information about the frequency of the child receiving
#'    the yogurt drinks from the previous day.
#'
#' This `get_mmff` is designed to diagnose the non-breastfed children who meet
#' the minimum requirement of milk feeding frequency or not.
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
#' @param q6bnum This parameter indicates the number of infant formula feeding
#'   the children received in previous days. The variable type should be a
#'   continuous variable.
#'
#' @param q6cnum This parameter holds information about the frequency of the
#'    children receiving the milk from animals, such as fresh, tinned, or
#'    powdered milk in the previous day.
#'
#' @param q6dnum For this parameter, you need to place the continuous variable,
#'    which presents the information about the frequency of the child receiving
#'    the yogurt drinks from the previous day.
#'
#' @param q7anum This parameter also represents the information about yogurt
#'    consumption frequency. Still, it counts other than yogurt drinks, and
#'    the variable came from the solid food consumption module from sample
#'    IYCF questionnaires.
#'
#'
#' The final command `get_mad` is the composition of all three main dietary
#' quality indicators generated by the previous three commands. It identifies
#' the children meet the minimum acceptable diet condition or not.
#'
#' @param age This parameter holds the information about child age in the month
#'    format, and it should be the continuous variable. It is either generated
#'    from the child's birth date or self-reported answers from the caregivers.
#'    If this was generated from the calculation based on the child's date of birth,
#'    the decimal class of the variable would be the decimal numeric format.
#'
#' @param q4 The dummy variable which mentioned that the child was receiving
#'    breastfeeding yesterday (previous days), which were coded as "1", or
#'    not - as "0".
#'
#' @param mdd This parameter holds whether the child received the minimum
#'    dietary diversity or not. The variable type must be the binary format
#'    as "1" indicated that the child got minimum dietary diversity and "0"
#'    meant didn't.
#'
#' @param mmf This parameter also must be the binary variable which indicated
#'    that the child got minimum meal frequency (coded as "1") or not
#'    (coded as "0").
#'
#' @param mmff This parameter is about the non-breastfed children received
#'    minimum milk feeding frequency or not. The variable type must be in the
#'    binary format as "1" for the children who met the required condition
#'    and "0" for those who didn't.
#'
#'
#' @return
#' Each command produces the respective matrix consisting of the new variables
#' generated by each command. If the argument add was applied in the command,
#' the output would be a new data frame attaching new variables generated from
#' each command. Those data frames have the same structure as the input
#' data frame in the command's parameters regarding the number of observations,
#' columns, and column names, except for differences in newly added variables
#' based on the command.
#'
#' In `mdd_df` data frame resulting from `get_mdd` command, the following new
#' variables are added, and each variable's detailed explanation is mentioned
#' below.
#'
#'    **Variables** | **Description**
#'    :--- | :---
#'    *breastmilk* | binary variables indicate child get the breastfeeding at yesterday (breastmilk =1 ) or not (breastmilk = 0)
#'    *grains* | binary variables indicate child get the food group related to grains, white/pale starchy roots, tubers and plantains at yesterday (grains =1 ) or not (grains = 0)
#'    *legumes* | binary variables indicate child get the food group related to beans, peas, lentils, nuts and seeds at yesterday (legumes =1 ) or not (legumes = 0)
#'    *diary* | binary variables indicate child get the food group related to dairy products (milk, infant formula, yogurt, cheese) at yesterday (diary =1 ) or not (diary = 0)
#'    *meat* | binary variables indicate child get the food group related to flesh foods (meat, fish, poultry, organ meats) at yesterday (meat =1 ) or not (meat = 0)
#'    *eggs* | binary variables indicate child get the any kind of eggs at yesterday (eggs =1 ) or not (eggs = 0)
#'    *vita_fruveg* | binary variables indicate child get the food group related to vitamin A-rich fruits and vegetables at yesterday (vita_fruveg =1 ) or not (vita_fruveg)
#'    *oth_fruveg* | binary variables indicate child get the food group related to other fruits and vegetables at yesterday (oth_fruveg =1 ) or not (oth_fruveg = 0)
#'    *consumed_group* | continuous variables indicate child consume number of food groups including solid, semi-solid or soft foods and breastfeeding
#'    *mdd* | binary variables indicate child get the minimum dietary diversity (mdd =1 ) or not (mdd = 0)
#'
#' The newly added variables in the `mmf_df` data frame are mentioned below.
#' This data frame is the output of `get_mmf` command.
#'
#'    **Variables** | **Description**
#'    :--- | :---
#'    *mmf_bf* | binary variables indicate breastfeed child get the minimum meal frequency (mmf_bf =1 ) or not (mmf_bf = 0)
#'    *nbf_frq* | continuous variables indicate non-breastfeed child consume number of both solid, semi-solid or soft foods and milk feeds
#'    *mmf_nbf* | binary variables indicate non-breastfeed child get the minimum meal frequency (mmf_nbf =1 ) or not (mmf_nbf = 0)
#'    *mmf* | binary variables indicate child get the minimum meal frequency (mmf =1 ) or not (mmf = 0)
#'
#' The output of the `get_mmff` command is `mmff_df` data frame, which
#' includes the following added variable.
#'
#'    **Variables** | **Description**
#'    :--- | :---
#'    *mf_frq* | continuous variables indicate non-breastfeed child consume number of milk and milk-product food feeding
#'    *mmff* | binary variables indicate child get the minimum milk feeding frequency (mmff =1 ) or not (mmff = 0)
#'
#' The output of `get_mad` command, the `mad_df` data frame, consists of
#' the following variables.
#'
#'    **Variables** | **Description**
#'    :--- | :---
#'    *mad* | binary variable indicate child get the minimum acceptable diet (mad =1 ) or not (mad = 0)
#'
#'
#'
#' @examples
#'
#' # MDD
#'   df <- get_mdd(cfData, age = "calc_age_months", q4 = "child_bfyest",
#'                 q7b = "child_rice", q7d = "child_potatoes", q7n = "child_beans",
#'                 q6b = "child_bms", q6c = "child_milk", q6d = "child_mproduct",
#'                 q7a = "child_yogurt", q7o = "child_yogurt", q7i = "child_organ",
#'                 q7j = "child_insects", q7k = "child_beef", q7m = "child_fish",
#'                 q7l = "child_eggs", q7c = "child_pumpkin", q7e = "child_leafyveg",
#'                 q7g = "child_mango", q7h = "child_fruit", q7f = "child_fruit",
#'                 add = TRUE)
#'
#' # MMF
#'   df <- get_mmf(df, age = "calc_age_months", q4 = "child_bfyest",
#'                 q8 = "child_food_freq", q6bnum = "child_bms_freq",
#'                 q6cnum = "child_milk_freq", q6dnum = "child_mproduct_freq",
#'                 add = TRUE)
#'
#' # MMFF
#'   df <- get_mmff(df, age = "calc_age_months", q4 = "child_bfyest",
#'                  q6bnum = "child_bms_freq", q6cnum = "child_milk_freq",
#'                  q6dnum = "child_mproduct_freq", q7anum = "child_yogurt",
#'                  add = TRUE)
#'
#' # MAD
#'   df <- get_mad(df, age = "calc_age_months", q4 = "child_bfyest",
#'                 mdd = "mdd", mmf = "mmf", mmff = "mmff", add = TRUE)
#'
#'
#' @export
#'
#' @rdname detect_minimumdiet
#'
#'
#################################################################################




################################################################################

# Step 1: Generate Solid Food Consumption (last 24 hours recall)
get_mdd <- function(df, age = NULL, q4 = NULL, q7b = NULL, q7d = NULL,
                    q7n = NULL, q6b = NULL, q6c = NULL, q6d = NULL,
                    q7a = NULL, q7o = NULL, q7i = NULL, q7j = NULL,
                    q7k = NULL, q7m = NULL, q7l = NULL, q7c = NULL,
                    q7e = NULL, q7g = NULL, q7h = NULL, q7f = NULL,
                    add = TRUE){

  output <- data.frame(matrix(ncol = 0, nrow = nrow(df)))

  # food groups construction
  if(!is.null(q4)){
    output$breastmilk <- ifelse(df[age] >= 6 & df[age] < 24 &
                                  df[q4] == 1, 1, 0)
  }

  if(!is.null(q7b) & !is.null(q7d)){
    output$grains <- ifelse(df[age] >= 6 & df[age] < 24 &
                              (df[q7b] == 1 | df[q7d] == 1), 1, 0)
  }

  if(!is.null(q7n)){
    output$legumes <- ifelse(df[q7n] == 1, 1, 0)
  }

  if(!is.null(q6b) & !is.null(q6c) & !is.null(q6d) & !is.null(q7a) &
     !is.null(q7o)){
    output$diary <- ifelse(df[age] >= 6 & df[age] < 24 &
                             (df[q6b] == 1 | df[q6c] == 1 | df[q6d] == 1 |
                             df[q7a] == 1 | df[q7o] == 1), 1, 0)
  }

  if(!is.null(q7i) & !is.null(q7j) & !is.null(q7k) & !is.null(q7m)){
    output$meat <- ifelse(df[age] >= 6 & df[age] < 24 &
                            (df[q7i] == 1 | df[q7j] == 1 | df[q7k] == 1 |
                            df[q7m] == 1), 1, 0)
  }

  if(!is.null(q7l)){
    output$eggs <- ifelse(df[age] >= 6 & df[age] < 24 & df[q7l] == 1, 1, 0)
  }

  if(!is.null(q7c) & !is.null(q7e) & !is.null(q7g)){
    output$vita_fruveg <- ifelse(df[age] >= 6 & df[age] < 24 &
                                   (df[q7c] == 1 | df[q7e] == 1 |
                                      df[q7g] == 1), 1, 0)
  }

  if(!is.null(q7h) & !is.null(q7f)){
    output$oth_fruveg <- ifelse(df[age] >= 6 & df[age] < 24 &
                                  (df[q7h] == 1 | df[q7f] == 1), 1, 0)
  }

  output$consumed_group <- apply(output, 1, sum)

  # mdd
  output$mdd <- ifelse(output$consumed_group >= 5, 1 , 0)

  if(add) {
    df <- cbind(df, output)
    mdd_df <- df
  }

 return(mdd_df)
}

#' @export
#' @rdname detect_minimumdiet
#'


# Step 2: Generate Minimum Meal Frequency (last 24 hours recall)
get_mmf <- function(df, age = NULL, q4 = NULL, q8 = NULL, q6bnum = NULL,
                    q6cnum = NULL, q6dnum = NULL, add = TRUE){

  output <- data.frame(matrix(ncol = 0, nrow = nrow(df)))

  # bf mmf
  output$mmf_bf <- ifelse(df[q4] == 1 & df[age] >= 6 & df[age] < 9 &
                            df[q8] >= 2, 1,
                          ifelse(df[q4] == 1 & df[age] >=9 & df[age] < 24 &
                                   df[q8] >= 3, 1, 0))

  # non-bf mmf
  nbf_frq <- data.frame(df[q6bnum], df[q6cnum], df[q6dnum], df[q8])
  nbf_frq$tot <- apply(nbf_frq, 1, sum)
  output$nbf_frq <- nbf_frq$tot

  output$mmf_nbf <- ifelse(df[q4] == 0 & df[age] >= 6 & df[age] < 24 &
                             df[q8] >= 1 & nbf_frq$tot >= 4, 1, 0)

  # total mmf
  output$mmf <- ifelse(output$mmf_bf == 1 | output$mmf_nbf ==1, 1, 0)

  if(add){
    df <- cbind(df, output)
    mmf_df <- df
  }

  return(mmf_df)
}

#' @export
#' @rdname detect_minimumdiet
#'


# Step 3: Generate Minimum Milk Frequency - Non BF (last 24 hours recall)
get_mmff <- function(df, age = NULL, q4 = NULL, q6bnum = NULL, q6cnum = NULL,
                     q6dnum = NULL, q7anum = NULL, add = TRUE){

  output <- data.frame(matrix(ncol = 0, nrow = nrow(df)))

  # non-bf mmf
  mf_frq <- data.frame(df[q6bnum], df[q6cnum], df[q6dnum], df[q7anum])
  mf_frq$tot <- apply(mf_frq, 1, sum)
  output$mf_frq <- mf_frq$tot

  # mmff
  output$mmff <- ifelse(df[q4] == 0 & df[age] >= 6 & df[age] < 24 &
                             mf_frq$tot >= 2, 1, 0)

  if(add){
    df <- cbind(df, output)
    mmff_df <- df
  }

  return(mmff_df)
}

#' @export
#' @rdname detect_minimumdiet
#'


# Step 4: Generate Minimum Acceptable (last 24 hours recall)

get_mad <- function(df, age = NULL, q4 = NULL, mdd = NULL, mmf = NULL,
                    mmff = NULL, add = TRUE){

  output <- data.frame(matrix(ncol = 0, nrow = nrow(df)))

  # mad
  output$mad <- ifelse(df[age] >= 6 & df[age] < 24 & (df[q4] == 1 | df[mmff] == 1) &
                          df[mdd] == 1 & df[mmf] == 1, 1, 0)

  if(add){
    df <- cbind(df, output)
    mad_df <- df
  }

  return(mad_df)
}


