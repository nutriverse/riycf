library(riycf)


# individual vector
food_1 <- c(0, NA, 1, 1, 1, 0, 0, 1, 1, 0)
food_2 <- c(1, 0, 0, 1, 1, 0, 0, 1, 1, 0)


# Return result values check
fg_one <- get_dummy(var_list = list(food_1))
fg_two <- get_dummy(var_list = list(food_2))
fg_combined <- get_dummy(var_list = list(food_1, food_2))


test_that("`get_dummy` function define the output values correctly", {

  expect_equal(fg_one, food_1)
  expect_equal(fg_two, food_2)
  expect_equal(fg_combined, c(1, NA, 1, 1, 1, 0, 0, 1, 1, 0))
})


# return data type check
test_that("`get_dummy` output has correct variable type", {
  expect_true(typeof(fg_one) == "double")
  expect_true(typeof(fg_two) == "double")
  expect_true(typeof(fg_combined) == "double")
})

