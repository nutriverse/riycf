library(riycf)

################################################################################
# Dummy Variable(s)
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

test_that("get_dummy gives clear errors for invalid var_list (#67)", {
  expect_error(get_dummy(NULL), "must be a non-empty list")
  expect_error(get_dummy(list()), "must be a non-empty list")
  expect_error(get_dummy(list(NULL, NULL)), "All items in var_list are NULL")
  expect_error(get_dummy(list(c("1", "0"))), "Item 1 of var_list is not numeric")
  expect_error(get_dummy(list(c(0, 1), c(0, 1, 1))), "must have the same length")
})

test_that("get_dummy ignores NULL items with a warning (#67)", {
  expect_warning(
    get_dummy(list(c(0, 1), NULL)), "Item 2 of var_list is NULL and was ignored"
  )
  expect_equal(suppressWarnings(get_dummy(list(c(0, 1), NULL))), c(0, 1))
})

test_that("get_dummy sets invalid codes to NA with a warning (#67)", {
  expect_warning(
    get_dummy(list(c(0, 999, 1), c(0, 0, 0))),
    "var_list: 1 invalid value was set to NA \\(999\\)"
  )
  expect_equal(
    suppressWarnings(get_dummy(list(c(0, 999, 1), c(0, 0, 0)))), c(0, NA, 1)
  )
})

