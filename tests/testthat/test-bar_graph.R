df_id <- data.frame(label = c("A", "B", "C"), value = c(10, 30, 20))
df_count <- mtcars

test_that("bar_graph errors when neither x nor y is supplied", {
  expect_error(bar_graph(df_count), "Either x or y must be supplied")
})

test_that("bar_graph returns ggplot with only y (count mode)", {
  p <- bar_graph(df_count, y = as.character(cyl))
  expect_s3_class(p, "ggplot")
})

test_that("bar_graph returns ggplot with only x (count mode)", {
  p <- bar_graph(df_count, x = as.character(cyl))
  expect_s3_class(p, "ggplot")
})

test_that("bar_graph returns ggplot with both x and y (identity mode)", {
  p <- bar_graph(df_id, x = value, y = label)
  expect_s3_class(p, "ggplot")
})

test_that("bar_graph returns ggplot with fill mapped", {
  df <- data.frame(label = c("A", "B", "C"), value = c(10, 30, 20), group = c("x", "y", "x"))
  p <- bar_graph(df, x = value, y = label, fill = group)
  expect_s3_class(p, "ggplot")
})

test_that("bar_graph with label column does not error", {
  p <- bar_graph(df_id, x = value, y = label, label = value)
  expect_s3_class(p, "ggplot")
})

test_that("bar_graph with log_x = TRUE does not error", {
  df <- data.frame(label = c("A", "B", "C"), value = c(10, 300, 2000))
  p <- bar_graph(df, x = value, y = label, label = value, log_x = TRUE)
  expect_s3_class(p, "ggplot")
})

test_that("bar_graph with y + fill but no x (count mode with fill mapping)", {
  p <- bar_graph(df_count, y = as.character(cyl), fill = as.character(am))
  expect_s3_class(p, "ggplot")
})

test_that("bar_graph with x + fill but no y (count mode with fill mapping on x)", {
  p <- bar_graph(df_count, x = as.character(cyl), fill = as.character(am))
  expect_s3_class(p, "ggplot")
})

test_that("bar_graph with align_title = 'panel' uses panel layout", {
  p <- bar_graph(df_id, x = value, y = label, align_title = "panel", title = "T")
  expect_s3_class(p, "ggplot")
})

test_that("bar_graph with invalid align_title warns and defaults to 'plot'", {
  expect_message(
    bar_graph(df_id, x = value, y = label, align_title = "left"),
    "improperly set"
  )
})
