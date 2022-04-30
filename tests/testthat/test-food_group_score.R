library(riycf)


# individual vector
food_1 <- c(0, NA, 1, 1, 1, 0, 0, 1, 1, 0)
food_2 <- c(1, 0, 0, 1, 1, 0, 0, 1, 1, 0)


breastmilk <- c(0, 0, 1, 1, 1, 0, 0, 1, 1, 0)
grains <- c(1, NA, 0, 1, 1, 0, 0, 1, 1, 0)
pulses <- c(1, 0, 1, 0, 1, 0, 0, 0, 1, 0)
dairy <- c(0, 1, 1, 0, 1, 0, 1, 0, 1, 0)
meat <- c(1, 0, 1, 0, 1, 0, NA, 0, 1, 1)
eggs <- c(0, 1, 1, 0, 1, 0, 0, 0, 1, 1)
vita_fruveg <- c(1, 0, 1, 0, 1, 1, 0, 0, 1, 0)
oth_fruveg <- c(1, 0, 1, 1, 0, 0, 1, 1, 1, 0)


# Return result values check

fg_one <- dummy_gen(var_list = list(food_1))
fg_two <- dummy_gen(var_list = list(food_2))
fg_combined <- dummy_gen(var_list = list(food_1, food_2))

food_score <- fg_score(breastmilk, grains, pulses, dairy, meat,
                       eggs, vita_fruveg, oth_fruveg)


test_that("Individual `fg_score` function define the output values correctly", {

  expect_equal(fg_one, food_1)
  expect_equal(fg_two, food_2)
  expect_equal(fg_combined, c(1, NA, 1, 1, 1, 0, 0, 1, 1, 0))
})


# return data type check
test_that("Individual `fg_score` output has correct variable type", {
  expect_true(typeof(fg_one) == "double")
  expect_true(typeof(fg_two) == "double")
  expect_true(typeof(fg_combined) == "double")
})


