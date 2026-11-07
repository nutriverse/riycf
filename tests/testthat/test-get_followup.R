################################################################################
# Tests for get_followup()
################################################################################

test_that("get_followup resolves the skip pattern correctly (#72)", {
  main     <- c(1, 1, 0, 0, NA, NA)
  followup <- c(1, 0, NA, 0, NA, 1)
  expect_no_warning(get_followup(main, followup))
  expect_equal(get_followup(main, followup), c(1, 0, 0, 0, NA, NA))
})

test_that("get_followup warns when a follow-up is missing after main = 1 (#72)", {
  expect_warning(
    get_followup(c(1, 1), c(NA, 1)),
    "1 follow-up value is missing although the main question is 1"
  )
  expect_equal(suppressWarnings(get_followup(c(1, 1), c(NA, 1))), c(NA, 1))
})

test_that("get_followup warns when the skip pattern is broken (#72)", {
  expect_warning(
    get_followup(c(0, 0, 0), c(1, 1, NA)),
    "2 records have main question 0 \\(no\\) but a follow-up value above 0"
  )
  expect_equal(
    suppressWarnings(get_followup(c(0, 0, 0), c(1, 1, NA))), c(NA, NA, 0)
  )
})

test_that("get_followup checks input types, lengths and codes (#72)", {
  expect_error(get_followup(NULL, 1), "main is NULL")
  expect_error(get_followup(1, "1"), "followup is not an integer or is not numeric")
  expect_error(get_followup(c(1, 0), 1), "must have the same length")

  expect_warning(get_followup(999, 1), "main: 1 invalid value was set to NA")
  expect_true(is.na(suppressWarnings(get_followup(999, 1))))

  expect_warning(get_followup(0, 9), "followup: 1 invalid value was set to NA")
  expect_equal(suppressWarnings(get_followup(0, 9)), 0)
})

test_that("get_followup gives a non-missing SwB on iycfData (#72)", {
  df <- iycfData

  sweet <- list(
    get_followup(df$child_milk, df$child_milk_sweet),
    get_followup(df$child_mproduct, df$child_mproduct_sweet),
    df$child_chocolate, df$child_juice, df$child_soda,
    get_followup(df$child_tea, df$child_tea_sweet),
    get_followup(df$child_oth_drink, df$child_oth_drink_sweet)
  )

  swb <- suppressWarnings(
    get_swb(get_dummy(var_list = sweet), df$calc_age_months)
  )
  expect_equal(sum(swb == 1, na.rm = TRUE), 77)
  expect_equal(sum(swb == 0, na.rm = TRUE), 36)
})

test_that("get_followup resolves skipped counts with valid (#74)", {
  expect_equal(
    get_followup(c(1, 1, 0, 0), c(3, 0, NA, 0), valid = 0:20), c(3, 0, 0, 0)
  )

  expect_warning(
    get_followup(0, 98, valid = 0:20),
    "followup: 1 invalid value was set to NA \\(98\\)"
  )

  expect_warning(get_followup(0, 2, valid = 0:20), "a follow-up value above 0")
  expect_true(is.na(suppressWarnings(get_followup(0, 2, valid = 0:20))))

  # the default valid = c(0, 1) does not accept counts
  expect_warning(get_followup(0, 2), "followup: 1 invalid value was set to NA")
})

test_that("MMFF and non-breastfed MMF are non-missing on iycfData (#74)", {
  df <- iycfData
  count <- function(main, followup) {
    get_followup(main, followup, valid = 0:20)
  }

  milk_frq <- suppressWarnings(get_milk_frq(
    count(df$child_bms, df$child_bms_freq),
    count(df$child_milk, df$child_milk_freq),
    count(df$child_mproduct, df$child_mproduct_freq),
    count(df$child_yogurt, df$child_yogurt_frq)
  ))
  mmff <- get_mmff(df$child_bfyest, df$calc_age_months, milk_frq)
  expect_equal(sum(mmff == 1, na.rm = TRUE), 8)
  expect_equal(sum(mmff == 0, na.rm = TRUE), 31)

  nonbf_frq <- suppressWarnings(get_nonbf_frq(
    count(df$child_bms, df$child_bms_freq),
    count(df$child_milk, df$child_milk_freq),
    count(df$child_mproduct, df$child_mproduct_freq),
    df$child_food_freq
  ))
  mmf_nonbf <- get_mmf_nonbf(
    df$child_bfyest, df$child_food_freq, nonbf_frq, df$calc_age_months
  )
  expect_equal(sum(mmf_nonbf == 1, na.rm = TRUE), 15)
  expect_equal(sum(mmf_nonbf == 0, na.rm = TRUE), 25)
})
