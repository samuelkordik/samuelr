test_that("No errors", {
  withr::with_seed(1234, {
    x <- stats::rnorm(1000)
    a <- sample_with_distribution(x, size = 10, seed = 1234)

    expect_type(a, "double")

})


})
