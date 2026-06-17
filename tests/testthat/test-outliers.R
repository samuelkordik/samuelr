test_that("tukey_fences returns a list with 4 named elements", {
  fences <- tukey_fences(1:20)
  expect_type(fences, "list")
  expect_named(fences, c("tukey_lower", "tukey_upper", "tukey_extreme_lower", "tukey_extreme_upper"))
})

test_that("tukey_fences calculates correct fence values", {
  x <- 1:20
  q1 <- quantile(x, 0.25, names = FALSE)
  q3 <- quantile(x, 0.75, names = FALSE)
  iqr <- q3 - q1

  fences <- tukey_fences(x)
  expect_equal(fences$tukey_lower, q1 - 1.5 * iqr)
  expect_equal(fences$tukey_upper, q3 + 1.5 * iqr)
  expect_equal(fences$tukey_extreme_lower, q1 - 3 * iqr)
  expect_equal(fences$tukey_extreme_upper, q3 + 3 * iqr)
})

test_that("tukey_fences errors on non-numeric input", {
  expect_error(tukey_fences(c("a", "b")))
  expect_error(tukey_fences(c(TRUE, FALSE)))
})

test_that("label_tukey_outliers returns a factor", {
  result <- label_tukey_outliers(1:20)
  expect_s3_class(result, "factor")
})

test_that("label_tukey_outliers has correct 5 levels in order", {
  result <- label_tukey_outliers(1:20)
  expect_equal(
    levels(result),
    c("Extreme Low Outlier", "Low Outlier", "Not Outlier", "High Outlier", "Extreme High Outlier")
  )
})

test_that("label_tukey_outliers labels obvious high outlier correctly", {
  x <- c(rep(10, 20), 10000)
  result <- label_tukey_outliers(x)
  expect_equal(as.character(result[21]), "Extreme High Outlier")
})

test_that("label_tukey_outliers labels symmetric distribution all as Not Outlier", {
  x <- 40:60
  result <- label_tukey_outliers(x)
  expect_true(all(result == "Not Outlier"))
})

test_that("label_tukey_outliers output length matches input length", {
  x <- c(1:18, 100, -100)
  result <- label_tukey_outliers(x)
  expect_length(result, length(x))
})
