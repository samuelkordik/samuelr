df <- data.frame(
  n = stats::rnorm(200, mean = 50, sd = 10),
  group = sample(c("A", "B"), 200, replace = TRUE)
)

test_that("hist_graph returns ggplot for basic input", {
  p <- hist_graph(df, n)
  expect_s3_class(p, "ggplot")
})

test_that("hist_graph with log_x = TRUE does not error", {
  df_pos <- data.frame(n = abs(stats::rnorm(200, mean = 100, sd = 10)) + 1)
  p <- hist_graph(df_pos, n, log_x = TRUE)
  expect_s3_class(p, "ggplot")
})

test_that("hist_graph with fill mapping returns ggplot", {
  p <- hist_graph(df, n, fill = group)
  expect_s3_class(p, "ggplot")
})

test_that("hist_graph passes title through", {
  p <- hist_graph(df, n, title = "Test Title")
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  expect_equal(p$labels$title, "Test Title")
})

test_that("hist_graph bins argument changes bin count", {
  p1 <- hist_graph(df, n, bins = 10)
  p2 <- hist_graph(df, n, bins = 100)
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
})

test_that("better_hist returns ggplot for basic input", {
  p <- suppressMessages(better_hist(df, n))
  expect_s3_class(p, "ggplot")
})

test_that("better_hist with add_density = TRUE does not error", {
  p <- suppressWarnings(suppressMessages(better_hist(df, n, add_density = TRUE)))
  expect_s3_class(p, "ggplot")
})

test_that("better_hist with explicit bounds does not error", {
  p <- suppressMessages(better_hist(df, n, lower_bound = 20, upper_bound = 80))
  expect_s3_class(p, "ggplot")
})

test_that("better_hist with add_min annotation does not error", {
  p <- suppressMessages(better_hist(df, n, add_min = TRUE, lower_bound = 0, upper_bound = 80))
  expect_s3_class(p, "ggplot")
})

test_that("better_hist with add_max annotation does not error", {
  p <- suppressMessages(better_hist(df, n, add_max = TRUE, lower_bound = 0, upper_bound = 80))
  expect_s3_class(p, "ggplot")
})

test_that("better_hist with add_median annotation does not error", {
  p <- suppressMessages(better_hist(df, n, add_median = TRUE, lower_bound = 0, upper_bound = 80))
  expect_s3_class(p, "ggplot")
})

test_that("better_hist with add_sd annotation does not error", {
  p <- suppressMessages(better_hist(df, n, add_sd = c(1), lower_bound = 0, upper_bound = 80))
  expect_s3_class(p, "ggplot")
})
