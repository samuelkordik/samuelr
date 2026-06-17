d <- as.Date("2025-01-01")
d2 <- as.Date("2025-07-15")

test_that("fmt_date abbr format produces correct string", {
  expect_equal(fmt_date(d, "abbr"), "Jan 1, 2025")
})

test_that("fmt_date short format produces correct string", {
  expect_equal(fmt_date(d, "short"), "1/1/2025")
})

test_that("fmt_date abbr format on non-January date", {
  expect_equal(fmt_date(d2, "abbr"), "Jul 15, 2025")
})

test_that("fmt_date short format on non-January date", {
  expect_equal(fmt_date(d2, "short"), "7/15/2025")
})

test_that("fmt_date range with abbr collapses with ' to '", {
  result <- fmt_date(c(d, d2), "abbr", range = TRUE)
  expect_equal(result, "Jan 1, 2025 to Jul 15, 2025")
})

test_that("fmt_date range with short collapses with ' - '", {
  result <- fmt_date(c(d, d2), "short", range = TRUE)
  expect_equal(result, "1/1/2025 - 7/15/2025")
})

test_that("fmt_date range with custom range_collapse uses it", {
  result <- fmt_date(c(d, d2), "abbr", range = TRUE, range_collapse = " – ")
  expect_equal(result, "Jan 1, 2025 – Jul 15, 2025")
})

test_that("fmt_date is vectorized without range", {
  result <- fmt_date(c(d, d2), "abbr")
  expect_equal(result, c("Jan 1, 2025", "Jul 15, 2025"))
})

test_that("fmt_date range with unrecognized format warns and defaults to ' to '", {
  expect_message(
    fmt_date(c(d, d2), "other", range = TRUE),
    "defaulting"
  )
})
