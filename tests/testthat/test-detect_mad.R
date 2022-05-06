

library(riycf)


################################################################################
# Return result values check
################################################################################

# get_mad
check_1 <- get_mad(6, 1, 1, 1, NA)
check_2 <- get_mad(3, 1, 1, 1, NA)
check_3 <- get_mad(24, 1, 1, 1, NA)
check_4 <- get_mad(6, 0, 1, 1, 1)
check_5 <- get_mad(3, 0, 1, 1, 1)
check_6 <- get_mad(24, 0, 1, 1, 1)
check_7 <- get_mad(6, NA, 1, 1, 1)
check_8 <- get_mad(3, NA, 1, 1, 1)
check_9 <- get_mad(24, NA, 1, 1, 1)
check_10 <- get_mad(20, 0, 1, 1, 1)
check_11 <- get_mad(3, 0, 1, 1, 1)
check_12 <- get_mad(24, 0, 1, 1, 1)
check_13 <- get_mad(NA, 1, 1, 1, NA)
check_14 <- get_mad(6, NA, 1, 1, NA)
check_15 <- get_mad(6, 1, NA, 1, NA)
check_16 <- get_mad(6, 1, 1, NA, NA)
check_17 <- get_mad(6, 1, 0, 1, NA)
check_18 <- get_mad(6, 1, 1, 0, NA)
check_19 <- get_mad(6, 0, 1, 1, 0)

check_list <- c(check_1, check_2, check_3, check_4, check_5, check_6, check_7,
                check_8, check_9, check_10, check_11, check_12, check_13,
                check_14, check_15, check_16, check_17, check_18, check_19)

exp_result <- c(1, NA, NA, 1, NA, NA, 1, NA, NA, 1, NA, NA, NA, NA, NA, NA, 0,
                0, 0)


test_that("`get_mad` function define the output values correctly", {

  for(i in 1:length(check_list)){

    expect_equal(check_list[i], exp_result[i])
  }

})
