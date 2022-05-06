library(riycf)


# Return result values check
result_1 <- get_mdd(7, 1)
result_2 <- get_mdd(c(NA, 4, 5), c(6, 10, 20))
result_3 <- get_mdd(c(7, 4, 3), c(7, 23, NA))
result_4 <- get_mdd(7, NA)

check_1 <- NA
check_2 <- c(NA, 0, 1)
check_3 <- c(1, 0, NA)
check_4 <- NA




test_that("`get_mdd` function define the output values correctly", {

  expect_equal(result_1, check_1)
  expect_equal(result_2, check_2)
  expect_equal(result_3, check_3)
  expect_equal(result_4, check_4)
})


# return data type check
test_that("`get_mdd` output has correct variable type", {
  expect_true(typeof(result_1) == "logical")
  expect_true(typeof(result_2) == "double")
  expect_true(typeof(result_3) == "double")
})


