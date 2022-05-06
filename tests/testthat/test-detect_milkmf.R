

library(riycf)


################################################################################
# Return result values check
################################################################################

# get_milk_frq
check_1 <- get_milk_frq(0, 0, 0, 0)
check_2 <- get_milk_frq(1, 1, 1, 1)
check_3 <- get_milk_frq(1, 0, 0, 1)
check_4 <- get_milk_frq(0, 1, 1, 0)
check_5 <- get_milk_frq(0, 1, 0, 1)
check_6 <- get_milk_frq(1, 0, 1, 0)
check_7 <- get_milk_frq(NA, NA, NA, NA)
check_8 <- get_milk_frq(NA, 0, 1, 0)
check_9 <- get_milk_frq(0, NA, 1, 0)
check_10 <- get_milk_frq(0, 0, NA, 0)
check_11 <- get_milk_frq(0, 0, 1, NA)

check_list <- c(check_1, check_2, check_3, check_4, check_5, check_6, check_7,
                check_8, check_9, check_10, check_11)

exp_result <- c(0, 4, 2, 2, 2, 2, NA, NA, NA, NA, NA)


test_that("`get_milk_frq` function define the output values correctly", {

  for(i in 1:length(check_list)){

    expect_equal(check_list[i], exp_result[i])
  }

})


# get_mmff
check_1 <- get_mmff(0, 2, 1)
check_2 <- get_mmff(1, 2, 1)
check_3 <- get_mmff(1, 6, NA)
check_4 <- get_mmff(1, NA, 2)
check_5 <- get_mmff(NA, 6, 2)
check_6 <- get_mmff(0, 6, 2)
check_7 <- get_mmff(1, 9, 2)
check_8 <- get_mmff(0, 20, 1)

check_list <- c(check_1, check_2, check_3, check_4, check_5, check_6, check_7,
                check_8)

exp_result <- c(NA, NA, NA, NA, NA, 1, NA, 0)

test_that("`get_mmff` function define the output values correctly", {

  for(i in 1:length(check_list)){

    expect_equal(check_list[i], exp_result[i])
  }

})
