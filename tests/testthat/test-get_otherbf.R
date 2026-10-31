

library(riycf)

################################################################################
# Return result values check
################################################################################

# Ever Breastfed
# get_evbf
check_1 <- get_evbf(1, 5)
check_2 <- get_evbf(1, 23)
check_3 <- get_evbf(1, 24)
check_4 <- get_evbf(0, 5)
check_5 <- get_evbf(0, 23)
check_6 <- get_evbf(0, 24)
check_7 <- get_evbf(NA, 5)
check_8 <- get_evbf(1, NA)
check_9 <- get_evbf(NA, 24)
check_10 <- get_evbf(0, NA)

check_list <- c(check_1, check_2, check_3, check_4, check_5, check_6, check_7,
                check_8, check_9, check_10)

exp_result <- c(1, 1, NA, 0, 0, NA, NA, NA, NA, NA)

test_that("get_evbf define the output values correctly", {

  for(i in 1:length(check_list)){

    expect_equal(check_list[i], exp_result[i])
  }

})

# Early Initiation of Breastfeeding
# get_eibf
check_1 <- get_eibf(5, 0, NA)
check_2 <- get_eibf(5, NA, NA)
check_3 <- get_eibf(24, 0, NA)
check_4 <- get_eibf(24, NA, NA)
check_5 <- get_eibf(5, 1, 0)
check_6 <- get_eibf(5, 1, 1)
check_7 <- get_eibf(24, 1, 0)
check_8 <- get_eibf(24, 1, 1)
check_9 <- get_eibf(NA, 1, 0)
check_10 <- get_eibf(NA, 0, NA)
check_11 <- get_eibf(NA, NA, NA)

check_list <- c(check_1, check_2, check_3, check_4, check_5, check_6, check_7,
                check_8, check_9, check_10, check_11)

exp_result <- c(1, NA, NA, NA, 1, 0, NA, NA, NA, NA, NA)


test_that("get_eibf define the output values correctly", {

  for(i in 1:length(check_list)){

    expect_equal(check_list[i], exp_result[i])
  }

})

test_that("get_eibf returns 0 when first put to the breast after days (#51)", {
  expect_equal(get_eibf(5, 2, NA), 0)
  expect_equal(get_eibf(5, 1, 3), 0)
  expect_true(is.na(get_eibf(5, 1, NA)))
})

test_that("get_eibf sets invalid codes and ages to NA with a warning (#51)", {
  expect_warning(get_eibf(5, 3, NA), "q2: 1 invalid value was set to NA \\(3\\)")
  expect_true(is.na(suppressWarnings(get_eibf(5, 3, NA))))
  expect_warning(get_eibf(5, 999, NA), "q2: 1 invalid value was set to NA")

  expect_warning(get_eibf(-4, 0, NA), "age: 1 value was outside the valid range")
  expect_true(is.na(suppressWarnings(get_eibf(-4, 0, NA))))
})

test_that("get_eibf gives the expected counts on iycfData (#51)", {
  eibf <- suppressWarnings(
    get_eibf(
      iycfData$calc_age_months, iycfData$child_eibf, iycfData$child_eibf_hrs
    )
  )
  expect_equal(sum(eibf == 1, na.rm = TRUE), 101)
  expect_equal(sum(eibf == 0, na.rm = TRUE), 41)
})

# Exclusive Breastfeeding for the first two days after birth
# get_ebf2d
check_1 <- get_ebf2d(1, 5)
check_2 <- get_ebf2d(1, 23)
check_3 <- get_ebf2d(1, 24)
check_4 <- get_ebf2d(0, 5)
check_5 <- get_ebf2d(0, 23)
check_6 <- get_ebf2d(0, 24)
check_7 <- get_ebf2d(NA, 5)
check_8 <- get_ebf2d(1, NA)
check_9 <- get_ebf2d(NA, 24)
check_10 <- get_ebf2d(0, NA)

check_list <- c(check_1, check_2, check_3, check_4, check_5, check_6, check_7,
                check_8, check_9, check_10)

exp_result <- c(0, 0, NA, 1, 1, NA, NA, NA, NA, NA)

test_that("`get_ebf2d` function define the output values correctly", {

  for(i in 1:length(check_list)){

    expect_equal(check_list[i], exp_result[i])
  }

})

# Mixed Milk Feeding Under 6 months
# get_mixmf
check_1 <- get_mixmf(1, 5, 1, 1)
check_2 <- get_mixmf(1, 5, 1, 0)
check_3 <- get_mixmf(1, 5, 0, 1)
check_4 <- get_mixmf(0, 5, 1, 1)
check_5 <- get_mixmf(0, 5, 0, 1)
check_6 <- get_mixmf(0, 5, 1, 0)
check_7 <- get_mixmf(1, 6, 1, 1)
check_8 <- get_mixmf(1, 6, 1, 0)
check_9 <- get_mixmf(1, 6, 0, 1)
check_10 <- get_mixmf(0, 24, 1, 1)
check_11 <- get_mixmf(0, 24, 0, 1)
check_12 <- get_mixmf(0, 24, 1, 0)
check_13 <- get_mixmf(NA, NA, NA, NA)
check_14 <- get_mixmf(NA, 5, 1, 1)
check_15 <- get_mixmf(1, NA, 1, 1)
check_16 <- get_mixmf(1, 5, NA, 1)
check_17 <- get_mixmf(1, 5, 1, NA)
check_18 <- get_mixmf(0, 5, NA, 1)
check_19 <- get_mixmf(0, 5, 1, NA)

check_list <- c(check_1, check_2, check_3, check_4, check_5, check_6, check_7,
                check_8, check_9, check_10, check_11, check_12, check_13,
                check_14, check_15, check_16, check_17, check_18, check_19)

exp_result <- c(1, 1, 1, 0, 0, 0, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1, 1, 0, 0)

test_that("`get_mixmf` function define the output values correctly", {

  for(i in 1:length(check_list)){

    expect_equal(check_list[i], exp_result[i])
  }

})



# Continuous Breastfeeding 12-23 months
# get_cbf
check_1 <- get_cbf(1, 12)
check_2 <- get_cbf(1, 23)
check_3 <- get_cbf(1, 24)
check_4 <- get_cbf(1, 11)
check_5 <- get_cbf(0, 12)
check_6 <- get_cbf(0, 23)
check_7 <- get_cbf(0, 24)
check_8 <- get_cbf(0, 11)
check_9 <- get_cbf(NA, 12)
check_10 <- get_cbf(1, NA)
check_11 <- get_cbf(NA, 24)
check_12 <- get_cbf(0, NA)

check_list <- c(check_1, check_2, check_3, check_4, check_5, check_6, check_7,
                check_8, check_9, check_10, check_11, check_12)

exp_result <- c(1, 1, NA, NA, 0, 0, NA, NA, NA, NA, NA, NA)

test_that("`get_cbf` function define the output values correctly", {

  for(i in 1:length(check_list)){

    expect_equal(check_list[i], exp_result[i])
  }

})

# Bottle Feeding 0-23 months
# get_bof
check_1 <- get_bof(1, 5)
check_2 <- get_bof(1, 23)
check_3 <- get_bof(1, 24)
check_4 <- get_bof(0, 5)
check_5 <- get_bof(0, 23)
check_6 <- get_bof(0, 24)
check_7 <- get_bof(NA, 5)
check_8 <- get_bof(1, NA)
check_9 <- get_bof(NA, 24)
check_10 <- get_bof(0, NA)

check_list <- c(check_1, check_2, check_3, check_4, check_5, check_6, check_7,
                check_8, check_9, check_10)

exp_result <- c(1, 1, NA, 0, 0, NA, NA, NA, NA, NA)

test_that("`get_bof` function define the output values correctly", {

  for(i in 1:length(check_list)){

    expect_equal(check_list[i], exp_result[i])
  }

})

