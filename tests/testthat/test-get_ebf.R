

library(riycf)


################################################################################
# Return result values check
################################################################################

# Exclusive Breastfeeding
# get_ebf
check_1 <- get_ebf(1, 5, 0, 0)
check_2 <- get_ebf(1, 5, 0, 1)
check_3 <- get_ebf(1, 5, 1, 0)
check_4 <- get_ebf(1, 6, 0, 0)
check_5 <- get_ebf(1, 6, 0, 1)
check_6 <- get_ebf(1, 6, 1, 0)
check_7 <- get_ebf(0, 6, 0, 0)
check_8 <- get_ebf(NA, NA, NA, NA)
check_9 <- get_ebf(NA, 5, 0, 0)
check_10 <- get_ebf(1, NA, 0, 0)
check_11 <- get_ebf(1, 5, NA, 0)
check_12 <- get_ebf(1, 5, 0, NA)
check_13 <- get_ebf(NA, 5, 1, 0)
check_14 <- get_ebf(1, NA, 1, 0)
check_15 <- get_ebf(1, 5, NA, 1)
check_16 <- get_ebf(1, 5, 1, NA)

check_17 <- get_ebf(0, 5, 0, 0)
check_18 <- get_ebf(1, 0, 0, 0)

check_list <- c(check_1, check_2, check_3, check_4, check_5, check_6, check_7,
                check_8, check_9, check_10, check_11, check_12, check_13,
                check_14, check_15, check_16, check_17, check_18)

exp_result <- c(1, 0, 0, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                0, 1)

test_that("`get_ebf` function define the output values correctly", {

  for(i in 1:length(check_list)){

    expect_equal(check_list[i], exp_result[i])
  }

})
