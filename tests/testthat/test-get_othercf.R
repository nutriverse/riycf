

library(riycf)

################################################################################
# Return result values check
################################################################################

# Introduction of Solid, Semi-solid or soft foods (6-8 months)
# get_isssf
check_1 <- get_isssf(1, 6)
check_2 <- get_isssf(1, 8)
check_3 <- get_isssf(1, 24)
check_4 <- get_isssf(1, 5)
check_5 <- get_isssf(0, 6)
check_6 <- get_isssf(0, 8)
check_7 <- get_isssf(0, 24)
check_8 <- get_isssf(0, 5)
check_9 <- get_isssf(NA, 6)
check_10 <- get_isssf(1, NA)
check_11 <- get_isssf(NA, 24)
check_12 <- get_isssf(0, NA)

check_list <- c(check_1, check_2, check_3, check_4, check_5, check_6, check_7,
                check_8, check_9, check_10, check_11, check_12)

exp_result <- c(1, 1, NA, NA, 0, 0, NA, NA, NA, NA, NA, NA)

test_that("`get_isssf` function define the output values correctly", {

  for(i in 1:length(check_list)){

    expect_equal(check_list[i], exp_result[i])
  }

})


# Egg and/or Flesh food consumption (6-23 months)
# get_eff
check_1 <- get_eff(1, 6)
check_2 <- get_eff(1, 8)
check_3 <- get_eff(1, 24)
check_4 <- get_eff(1, 5)
check_5 <- get_eff(0, 6)
check_6 <- get_eff(0, 8)
check_7 <- get_eff(0, 24)
check_8 <- get_eff(0, 5)
check_9 <- get_eff(NA, 6)
check_10 <- get_eff(1, NA)
check_11 <- get_eff(NA, 24)
check_12 <- get_eff(0, NA)

check_list <- c(check_1, check_2, check_3, check_4, check_5, check_6, check_7,
                check_8, check_9, check_10, check_11, check_12)

exp_result <- c(1, 1, NA, NA, 0, 0, NA, NA, NA, NA, NA, NA)

test_that("`get_eff` function define the output values correctly", {

  for(i in 1:length(check_list)){

    expect_equal(check_list[i], exp_result[i])
  }

})


# Sweet beverage consumption (6-23 months)
# get_swb
check_1 <- get_swb(1, 6)
check_2 <- get_swb(1, 8)
check_3 <- get_swb(1, 24)
check_4 <- get_swb(1, 5)
check_5 <- get_swb(0, 6)
check_6 <- get_swb(0, 8)
check_7 <- get_swb(0, 24)
check_8 <- get_swb(0, 5)
check_9 <- get_swb(NA, 6)
check_10 <- get_swb(1, NA)
check_11 <- get_swb(NA, 24)
check_12 <- get_swb(0, NA)

check_list <- c(check_1, check_2, check_3, check_4, check_5, check_6, check_7,
                check_8, check_9, check_10, check_11, check_12)

exp_result <- c(1, 1, NA, NA, 0, 0, NA, NA, NA, NA, NA, NA)

test_that("`get_swb` function define the output values correctly", {

  for(i in 1:length(check_list)){

    expect_equal(check_list[i], exp_result[i])
  }

})


# Unhealthy food consumption (6-23 months)
# get_ufc
check_1 <- get_ufc(1, 6)
check_2 <- get_ufc(1, 8)
check_3 <- get_ufc(1, 24)
check_4 <- get_ufc(1, 5)
check_5 <- get_ufc(0, 6)
check_6 <- get_ufc(0, 8)
check_7 <- get_ufc(0, 24)
check_8 <- get_ufc(0, 5)
check_9 <- get_ufc(NA, 6)
check_10 <- get_ufc(1, NA)
check_11 <- get_ufc(NA, 24)
check_12 <- get_ufc(0, NA)

check_list <- c(check_1, check_2, check_3, check_4, check_5, check_6, check_7,
                check_8, check_9, check_10, check_11, check_12)

exp_result <- c(1, 1, NA, NA, 0, 0, NA, NA, NA, NA, NA, NA)

test_that("`get_ufc` function define the output values correctly", {

  for(i in 1:length(check_list)){

    expect_equal(check_list[i], exp_result[i])
  }

})


# Zero vegetable or fruit consumption (6-23 months)
# get_zvf
check_1 <- get_zvf(1, 6)
check_2 <- get_zvf(1, 8)
check_3 <- get_zvf(1, 24)
check_4 <- get_zvf(1, 5)
check_5 <- get_zvf(0, 6)
check_6 <- get_zvf(0, 8)
check_7 <- get_zvf(0, 24)
check_8 <- get_zvf(0, 5)
check_9 <- get_zvf(NA, 6)
check_10 <- get_zvf(1, NA)
check_11 <- get_zvf(NA, 24)
check_12 <- get_zvf(0, NA)

check_list <- c(check_1, check_2, check_3, check_4, check_5, check_6, check_7,
                check_8, check_9, check_10, check_11, check_12)

exp_result <- c(0, 0, NA, NA, 1, 1, NA, NA, NA, NA, NA, NA)

test_that("`get_zvf` function define the output values correctly", {

  for(i in 1:length(check_list)){

    expect_equal(check_list[i], exp_result[i])
  }

})


