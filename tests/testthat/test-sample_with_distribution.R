test_that("output type is double", {
  withr::with_seed(1234, {
    x <- stats::rnorm(1000)
    a <- suppressMessages(sample_with_distribution(x, size = 10, seed = 1234))
    expect_type(a, "double")
  })
})

test_that("output length matches size argument", {
  withr::with_seed(42, {
    x <- stats::rnorm(500, mean = 10, sd = 2)
    a <- suppressMessages(sample_with_distribution(x, size = 50, seed = 42))
    expect_length(a, 50)
  })
})

test_that("result is reproducible with the same seed", {
  x <- stats::rnorm(500)
  a1 <- suppressMessages(sample_with_distribution(x, size = 20, seed = 99))
  a2 <- suppressMessages(sample_with_distribution(x, size = 20, seed = 99))
  expect_equal(a1, a2)
})

test_that("errors on non-numeric x", {
  expect_error(
    suppressMessages(sample_with_distribution(c("a", "b"), size = 10)),
    "numeric"
  )
})

test_that("errors on size <= 0", {
  x <- stats::rnorm(100)
  expect_error(
    suppressMessages(sample_with_distribution(x, size = 0)),
    "greater than zero"
  )
})

test_that("works with exponential-ish data", {
  withr::with_seed(7, {
    x <- stats::rexp(500, rate = 0.1)
    a <- suppressMessages(sample_with_distribution(x, size = 30, seed = 7))
    expect_type(a, "double")
    expect_length(a, 30)
  })
})
