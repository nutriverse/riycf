

library(riycf)


################################################################################
## Food Groups Score ##
# individual vector
breastmilk <- c(0, 0, 1, 1, 1, 0, 0, 1, 1, NA)
grains <- c(1, NA, 0, 1, 1, 0, 0, 1, 1, 0)
pulses <- c(1, 0, 1, 0, 1, 0, 0, 0, 1, 0)
dairy <- c(0, 1, 1, 0, 1, 0, 1, 0, 1, 0)
meat <- c(1, 0, 1, 0, 1, 0, NA, 0, 1, 1)
eggs <- c(0, 1, 1, 0, 1, 0, 0, 0, 1, 1)
vita_fruveg <- c(1, 0, 1, 0, 1, 1, 0, 0, 1, 0)
oth_fruveg <- c(1, 0, 1, 1, NA, 0, 1, 1, 1, 0)

# Return result values check
score_1 <- get_foodscore(breastmilk, breastmilk, breastmilk, breastmilk,
                         breastmilk, breastmilk, breastmilk, breastmilk)

score_2 <- get_foodscore(breastmilk, grains, pulses, dairy, meat, eggs,
                         vita_fruveg, oth_fruveg)

score_3 <- get_foodscore(breastmilk, grains, pulses, dairy, meat, eggs,
                         meat, oth_fruveg)

check_1 <- apply(cbind(breastmilk, breastmilk, breastmilk, breastmilk,
                       breastmilk, breastmilk, breastmilk, breastmilk), 1,
                 sum,na.rm = FALSE)

check_2 <- apply(cbind(breastmilk, grains, pulses, dairy, meat, eggs,
                       vita_fruveg, oth_fruveg), 1,
                 sum,na.rm = FALSE)

check_3 <- apply(cbind(breastmilk, grains, pulses, dairy, meat, eggs,
                       meat, oth_fruveg), 1,
                 sum,na.rm = FALSE)


test_that("`fg_score` function define the output values correctly", {

  expect_equal(score_1, check_1)
  expect_equal(score_2, check_2)
  expect_equal(score_3, check_3)
})


# return data type check
test_that("`fg_score` output has correct variable type", {
  expect_true(typeof(score_1) == "double")
  expect_true(typeof(score_2) == "double")
  expect_true(typeof(score_3) == "double")
})


