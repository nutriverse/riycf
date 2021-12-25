################################################################################
#
#' @title Determining Minimum Dietary Practices (from Core IYCF indicatros)
#'
#' @description Identification of individual 6-23 months old children dietary
#' practices meet the minimum requirement in terms of diversity, frequency and
#' acceptable diet
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
get_mdd <- function(df, q4 = NULL, q7b = NULL, q7d = NULL, q7n = NULL,
                    q6b = NULL, q6c = NULL, q6d = NULL, q7a = NULL,
                    q7o = NULL, q7i = NULL, q7j = NULL, q7k = NULL,
                    q7m = NULL, q7l = NULL, q7c = NULL, q7e = NULL,
                    q7g = NULL, q7h = NULL, q7f = NULL){

  output <- data.frame(matrix(ncol = 0, nrow = nrow(df)))

  # breastmilk <- vector(mode = "numeric", length = nrow(df))
  # grains <- vector(mode = "numeric", length = nrow(df))
  # legumes <- vector(mode = "numeric", length = nrow(df))
  # diary <- vector(mode = "numeric", length = nrow(df))
  # meat <- vector(mode = "numeric", length = nrow(df))
  # eggs <- vector(mode = "numeric", length = nrow(df))
  # vita_fruveg <- vector(mode = "numeric", length = nrow(df))
  # oth_fruveg <- vector(mode = "numeric", length = nrow(df))

  if(!is.null(q4)){
    output$breastmilk <- ifelse(df$q4 == 1, 1, 0)
  }

  if(!is.null(q7b) & !is.null(q7d)){
    output$grains <- ifelse(df$q7b == 1 | df$q7d == 1, 1, 0)
  }

  if(!is.null(q7n)){
    output$legumes <- ifelse(df$q7n == 1, 1, 0)
  }

  if(!is.null(q6b) & !is.null(q6c) & !is.null(q6d) & !is.null(q7a) &
     !is.null(q7o)){
    output$diary <- ifelse(df$q6b == 1 | df$q6c == 1 | df$q6d == 1 |
                             df$q7a == 1 | df$q7o == 1, 1, 0)
  }

  if(!is.null(q7i) & !is.null(q7j) & !is.null(q7k) & !is.null(q7m)){
    output$meat <- ifelse(df$q7i == 1 | df$q7j == 1 | df$q7k == 1 |
                            df$q7m == 1, 1, 0)
  }

  if(!is.null(q7l)){
    output$eggs <- ifelse(df$q7l == 1, 1, 0)
  }

  if(!is.null(q7c) & !is.null(q7e) & !is.null(q7g)){
    output$vita_fruveg <- ifelse(df$q7c == 1 | df$q7e == 1 | df$q7g == 1, 1, 0)
  }

  if(!is.null(q7h) & !is.null(q7f)){
    output$oth_fruveg <- ifelse(df$q7h == 1 | df$q7f == 1, 1, 0)
  }

  # output <- data.frame(breastmilk, grains, legumes, diary, meat,
  #               eggs, vita_fruveg, oth_fruveg)

  output$consumed_group <- apply(output, 1, sum)

 return(output)
}

#' @export
#' @rdname detect_minimumdiet
#'


# Step 2: Generate Minimum Meal Frequency (last 24 hours recall)

get_mmf <- function(df, age = NULL, q4 = NULL, q8 = NULL, q6bnum = NULL,
                    q6cnum = NULL, q6dnum = NULL){

  output <- data.frame(matrix(ncol = 0, nrow = nrow(df)))

  # bf mmf
  output$mmf_bf <- ifelse(df$q4 == 1 & df$age >= 6 & df$age < 9 &
                            df$q8 >= 2, 1,
                          ifelse(df$q4 == 1 & df$age >=9 & df$age < 24 &
                                   df$q8 >= 3, 1, 0))

  # non-bf mmf
  nbf_frq <- data.frame(df$q6bnum, df$q6cnum, df$q6dnum, df$q8)
  nbf_frq$tot <- apply(nbf_frq, 1, sum)

  output$mmf_nbf <- ifelse(df$q4 == 0 & df$age >= 6 & df$age < 24 &
                             df$q8 >= 1 & nbf_frq$tot >= 4)

  # total mmf
  output$mmf <- ifelse(output$mmf_bf == 1 | output$mmf_nbf ==1, 1, 0)

  return(output)
}


#' @export
#' @rdname detect_minimumdiet
#'


# Step 3: Generate Minimum Milk Frequency - Non BF (last 24 hours recall)

get_mmff <- function(df, age = NULL, q4 = NULL, q6bnum = NULL, q6cnum = NULL,
                     q6dnum = NULL, q7anum = NULL){

  output <- data.frame(matrix(ncol = 0, nrow = nrow(df)))

  # non-bf mmf
  mf_frq <- data.frame(df$q6bnum, df$q6cnum, df$q6dnum, df$q7anum)
  mf_frq$tot <- apply(mf_frq, 1, sum)

  output$mmff <- ifelse(df$q4 == 0 & df$age >= 6 & df$age < 24 &
                             mf_frq$tot >= 2)

  return(output)
}


#' @export
#' @rdname detect_minimumdiet
#'


# Step 4: Generate Minimum Acceptable (last 24 hours recall)

get_mad <- function(df, age = NULL, q4 = NULL, mdd = NULL, mmf = NULL,
                    mmff = NULL){

  output <- data.frame(matrix(ncol = 0, nrow = nrow(df)))

  output$mmff <- ifelse(df$age >= 6 & df$age < 24 & (df$q4 == 1 | mmff == 1) &
                          mdd == 1 & mmf == 1)

  return(output)
}


