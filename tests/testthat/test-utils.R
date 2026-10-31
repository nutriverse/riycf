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
