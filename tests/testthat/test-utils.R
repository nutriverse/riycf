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
