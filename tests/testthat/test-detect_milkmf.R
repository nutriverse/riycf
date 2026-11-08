


################################################################################
# Return result values check
################################################################################
# Milk Feeding Frequency Score
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

# Minimum Milk Feeding Frequency
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


test_that("errors are produced as expected", {
  expect_error(get_mmff(0, 2, NULL))
  expect_error(get_milk_frq(0, 0, 0, NULL))
})

test_that("get_milk_frq and get_mmff name the missing argument (#67)", {
  expect_error(get_mmff(0, 2, NULL), "milk_frq is NULL")
  expect_error(get_milk_frq(0, 0, 0, NULL), "q7anum is NULL")
})

test_that("get_milk_frq sets counts outside 0-20 to NA with a warning (#67)", {
  expect_warning(
    get_milk_frq(1, 999, 0, 0), "q6cnum: 1 invalid value was set to NA \\(999\\)"
  )
  expect_true(is.na(suppressWarnings(get_milk_frq(1, 999, 0, 0))))
  expect_equal(get_milk_frq(20, 0, 0, 0), 20)
})

test_that("get_mmff checks codes, ages and milk_frq (#67)", {
  expect_warning(get_mmff(999, 12, 2), "q4: 1 invalid value was set to NA")
  expect_warning(get_mmff(0, -4, 2), "age: 1 value was outside the valid range")
  expect_warning(get_mmff(0, 12, 81), "milk_frq: 1 invalid value was set to NA")
  expect_true(is.na(suppressWarnings(get_mmff(0, 12, 81))))
  expect_equal(c(get_mmff(0, 12, 2), get_mmff(0, 12, 1)), c(1, 0))
})