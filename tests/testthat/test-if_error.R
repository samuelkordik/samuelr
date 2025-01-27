test_that("Returns function value if no error", {
  log_or_na <- if_error(log)
  expect_type(log_or_na(2), "double")
})

test_that("Returns NA if error, verbose", {
  log_or_na <- if_error(log)
  a <- log_or_na("A")
  expect_equal(a, NA)
  expect_message(log_or_na("A"), "non-numeric argument")
})

test_that("Returns NA if error, stays silent", {
  log_or_na <- if_error(log, .verbose = FALSE)
  a <- log_or_na("A")
  expect_no_message(log_or_na("A"))
  expect_equal(a, NA)
})

test_that("Returns custom value if error, verbose", {
  log_or_na <- if_error(log, .on_error = 2)
  expect_equal(log_or_na("A"), 2)
  expect_message(log_or_na("A"), "non-numeric argument")
})

test_that("Returns custom value if error, silent", {
  log_or_na <- if_error(log, .on_error = 2, .verbose = FALSE)
  expect_equal(log_or_na("A"), 2)
  expect_no_message(log_or_na("A"))
})
