test_that("fmt_comma formats numbers with thousands separator", {
  expect_equal(fmt_comma(1000), "1,000")
  expect_equal(fmt_comma(1000000), "1,000,000")
  expect_equal(fmt_comma(0), "0")
  expect_equal(fmt_comma(-1234), "−1,234")
})

test_that("fmt_comma respects decimals argument", {
  expect_equal(fmt_comma(1234.567, decimals = 2), "1,234.57")
  expect_equal(fmt_comma(1234.567, decimals = 0), "1,235")
})

test_that("fmt_comma handles NA", {
  expect_equal(fmt_comma(NA_real_), "NA")
})

test_that("fmt_comma is vectorized", {
  result <- fmt_comma(c(1000, 2000, NA_real_))
  expect_length(result, 3)
  expect_equal(result[1], "1,000")
  expect_equal(result[3], "NA")
})

test_that("fmt_compact abbreviates large numbers", {
  expect_equal(fmt_compact(1000), "1K")
  expect_equal(fmt_compact(1000000), "1M")
  expect_equal(fmt_compact(80), "80")
})

test_that("fmt_compact respects drop_trailing_zeros", {
  expect_equal(fmt_compact(1500, drop_trailing_zeros = FALSE), "1.50K")
  expect_equal(fmt_compact(1500, drop_trailing_zeros = TRUE), "1.5K")
})

test_that("fmt_compact handles NA", {
  expect_equal(fmt_compact(NA_real_), "NA")
})

test_that("fmt_pct formats proportions as percentages", {
  expect_equal(fmt_pct(0.5), "50%")
  expect_equal(fmt_pct(0), "0%")
  expect_equal(fmt_pct(1), "100%")
})

test_that("fmt_pct respects decimals argument", {
  expect_equal(fmt_pct(0.1234, decimals = 1), "12.3%")
})

test_that("fmt_pct handles NA", {
  expect_equal(fmt_pct(NA_real_), "NA")
})

test_that("fmt_pct is vectorized", {
  result <- fmt_pct(c(0.1, 0.5, NA_real_))
  expect_length(result, 3)
  expect_equal(result[2], "50%")
})
