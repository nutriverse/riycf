

library(riycf)

################################################################################
# Return result values check
################################################################################
# Breastfeeding Child - MMF 6-8 months
# get_mmf_bf_6to8
check_1 <- get_mmf_bf_6to8(0, 1, 2)
check_2 <- get_mmf_bf_6to8(1, 1, 2)
check_3 <- get_mmf_bf_6to8(1, NA, 6)
check_4 <- get_mmf_bf_6to8(1, 2, NA)
check_5 <- get_mmf_bf_6to8(NA, 2, 6)
check_6 <- get_mmf_bf_6to8(1, 2, 6)
check_7 <- get_mmf_bf_6to8(1, 2, 9)
check_8 <- get_mmf_bf_6to8(1, 1, 8)

check_list <- c(check_1, check_2, check_3, check_4, check_5, check_6, check_7,
                check_8)

exp_result <- c(NA, NA, NA, NA, NA, 1, NA, 0)

test_that("`get_mmf_bf_6to8` function define the output values correctly", {

  for(i in 1:length(check_list)){

    expect_equal(check_list[i], exp_result[i])
  }

})

# Breastfeeding Child - MMF 9-23 months
# get_mmf_bf_9to23
check_1 <- get_mmf_bf_9to23(0, 1, 9)
check_2 <- get_mmf_bf_9to23(1, 1, 8)
check_3 <- get_mmf_bf_9to23(1, NA, 10)
check_4 <- get_mmf_bf_9to23(1, 3, NA)
check_5 <- get_mmf_bf_9to23(NA, 2, 23)
check_6 <- get_mmf_bf_9to23(1, 3, 9)
check_7 <- get_mmf_bf_9to23(1, 4, 8)
check_8 <- get_mmf_bf_9to23(1, 2, 10)

check_list <- c(check_1, check_2, check_3, check_4, check_5, check_6, check_7,
                check_8)

exp_result <- c(NA, NA, NA, NA, NA, 1, NA, 0)


test_that("`get_mmf_bf_9to23` function define the output values correctly", {

  for(i in 1:length(check_list)){

    expect_equal(check_list[i], exp_result[i])
  }

})

# Breastfeeding Child - MMF 6-23 months
# get_mmf_bf
check_1 <- get_mmf_bf(NA, 1)
check_2 <- get_mmf_bf(1, NA)
check_3 <- get_mmf_bf(NA, NA)
check_4 <- get_mmf_bf(0, 0)
check_5 <- get_mmf_bf(1, 0)
check_6 <- get_mmf_bf(0, NA)
check_7 <- get_mmf_bf(NA, 0)

check_list <- c(check_1, check_2, check_3, check_4, check_5, check_6, check_7)

exp_result <- c(1, 1, NA, 0, 1, 0, 0)


test_that("`get_mmf_bf` function define the output values correctly", {

  for(i in 1:length(check_list)){

    expect_equal(check_list[i], exp_result[i])
  }

})

# Non-Breastfeeding Child Meal Frequency including milk feeding
# get_nonbf_frq
check_1 <- get_nonbf_frq(0, 0, 0, 0)
check_2 <- get_nonbf_frq(1, 1, 1, 1)
check_3 <- get_nonbf_frq(1, 0, 0, 1)
check_4 <- get_nonbf_frq(0, 1, 1, 0)
check_5 <- get_nonbf_frq(0, 1, 0, 1)
check_6 <- get_nonbf_frq(1, 0, 1, 0)
check_7 <- get_nonbf_frq(NA, NA, NA, NA)
check_8 <- get_nonbf_frq(NA, 0, 1, 0)
check_9 <- get_nonbf_frq(0, NA, 1, 0)
check_10 <- get_nonbf_frq(0, 0, NA, 0)
check_11 <- get_nonbf_frq(0, 0, 1, NA)

check_list <- c(check_1, check_2, check_3, check_4, check_5, check_6, check_7,
                check_8, check_9, check_10, check_11)

exp_result <- c(0, 4, 2, 2, 2, 2, NA, NA, NA, NA, NA)


test_that("`get_nonbf_frq` function define the output values correctly", {

  for(i in 1:length(check_list)){

    expect_equal(check_list[i], exp_result[i])
  }

})


# Non-Breastfeeding Child - MMF 6-23 months
# get_mmf_nonbf
check_1 <- get_mmf_nonbf(NA, NA, NA, NA)
check_2 <- get_mmf_nonbf(NA, 1, 4, 6)
check_3 <- get_mmf_nonbf(0, NA, 4, 6)
check_4 <- get_mmf_nonbf(0, 1, NA, 6)
check_5 <- get_mmf_nonbf(0, 1, 4, NA)
check_6 <- get_mmf_nonbf(1, 1, 4, 6)
check_7 <- get_mmf_nonbf(0, 1, 4, 5)
check_8 <- get_mmf_nonbf(0, 1, 4, 6)
check_9 <- get_mmf_nonbf(0, 1, 3, 6)
check_10 <- get_mmf_nonbf(0, 1, 4, 20)
check_11 <- get_mmf_nonbf(0, 1, 4, 24)


check_list <- c(check_1, check_2, check_3, check_4, check_5, check_6, check_7,
                check_8, check_9, check_10, check_11)

exp_result <- c(NA, NA, NA, NA, NA, NA, NA, 1, 0, 1, NA)


test_that("`get_mmf_nonbf` function define the output values correctly", {

  for(i in 1:length(check_list)){

    expect_equal(check_list[i], exp_result[i])
  }

})

# Child - MMF 6-23 months
# get_mmf
check_1 <- get_mmf(NA, 1)
check_2 <- get_mmf(1, NA)
check_3 <- get_mmf(NA, NA)
check_4 <- get_mmf(0, 0)
check_5 <- get_mmf(1, 0)
check_6 <- get_mmf(0, NA)
check_7 <- get_mmf(NA, 0)

check_list <- c(check_1, check_2, check_3, check_4, check_5, check_6, check_7)

exp_result <- c(1, 1, NA, 0, 1, 0, 0)


test_that("`get_mmf` function define the output values correctly", {

  for(i in 1:length(check_list)){

    expect_equal(check_list[i], exp_result[i])
  }

})




