################################################################################
# Tests for internal checker functions
################################################################################

# check_if_number
test_that("check_if_number accepts numbers, integers and NA", {
  expect_no_error(check_if_number(5))
  expect_no_error(check_if_number(c(0L, 1L, NA)))
  expect_no_error(check_if_number(c(0.5, 23, 59)))
  expect_no_error(check_if_number(NA))
  expect_no_error(check_if_number(c(NA, NA)))
})

test_that("check_if_number gives a clear error for NULL input", {
  q4 <- NULL
  expect_error(check_if_number(q4), "q4 is NULL")
  expect_error(get_evbf(NULL, 5), "q4 is NULL")
  expect_error(get_evbf(1, NULL), "age is NULL")
})

test_that("check_if_number gives a clear error for non-numeric input", {
  q4 <- c("1", "0")
  expect_error(check_if_number(q4), "q4 is not an integer or is not numeric")
  expect_error(
    get_evbf(c("1", "0"), c(5, 6)), "q4 is not an integer or is not numeric"
  )
  expect_error(get_evbf(TRUE, 5), "q4 is not an integer or is not numeric")
})

test_that("check_if_number reports the caller's argument name when missing", {
  expect_error(get_evbf(age = 5), "q4 is missing")
  expect_error(get_eibf(age = 5, q2 = 0), "q2_hour is missing")
})

# check_if_valid_code
test_that("check_if_valid_code keeps valid codes and NA without warning", {
  q4 <- c(0, 1, NA, 1)
  expect_no_warning(check_if_valid_code(q4))
  expect_equal(check_if_valid_code(q4), c(0, 1, NA, 1))

  q2 <- c(0L, 1L, 2L, NA)
  expect_no_warning(check_if_valid_code(q2, valid = 0:2))
  expect_identical(check_if_valid_code(q2, valid = 0:2), q2)
})

test_that("check_if_valid_code sets invalid codes to NA with a warning", {
  q4 <- c(0, 1, 999, NA)
  expect_warning(
    check_if_valid_code(q4), "q4: 1 invalid value was set to NA \\(999\\)"
  )
  expect_equal(suppressWarnings(check_if_valid_code(q4)), c(0, 1, NA, NA))

  q2 <- c(0, 3, 2, 999, 999)
  expect_warning(
    check_if_valid_code(q2, valid = 0:2),
    "q2: 3 invalid values were set to NA \\(3 and 999\\)"
  )
  expect_equal(
    suppressWarnings(check_if_valid_code(q2, valid = 0:2)),
    c(0, NA, 2, NA, NA)
  )
})

test_that("check_if_valid_code keeps the integer type", {
  q4 <- c(1L, 999L)
  expect_type(suppressWarnings(check_if_valid_code(q4)), "integer")
})

# check_if_valid_age
test_that("check_if_valid_age keeps ages from 0 to 59 months and NA", {
  age <- c(0, 5, 23.5, 59, NA)
  expect_no_warning(check_if_valid_age(age))
  expect_equal(check_if_valid_age(age), c(0, 5, 23.5, 59, NA))
})

test_that("check_if_valid_age sets out-of-range ages to NA with a warning", {
  age <- c(-4, 5, NA)
  expect_warning(
    check_if_valid_age(age),
    "age: 1 value was outside the valid range and set to NA \\(-4\\)"
  )
  expect_equal(suppressWarnings(check_if_valid_age(age)), c(NA, 5, NA))

  age <- c(60, 12, 300)
  expect_warning(
    check_if_valid_age(age),
    "age: 2 values were outside the valid range and set to NA \\(60 and 300\\)"
  )
  expect_equal(suppressWarnings(check_if_valid_age(age)), c(NA, 12, NA))
})

test_that("check_if_valid_age respects custom limits and integer type", {
  age <- c(10L, 30L)
  expect_warning(check_if_valid_age(age, max_age = 23), "valid range is 0 to 23")
  expect_identical(
    suppressWarnings(check_if_valid_age(age, max_age = 23)), c(10L, NA)
  )
})
