test_that("use_defined_columns renames a column matching the regex", {
  df <- data.frame(PatientAge = 25:30, ResponseTime_sec = 1:6)
  result <- suppressMessages(
    use_defined_columns(
      df,
      defined_colnames = c("age"),
      defined_colname_patterns = c("patientage"),
      keep = TRUE
    )
  )
  expect_true("age" %in% names(result))
})

test_that("keep = TRUE preserves non-defined columns", {
  df <- data.frame(PatientAge = 25:30, ResponseTime_sec = 1:6)
  result <- suppressMessages(
    use_defined_columns(
      df,
      defined_colnames = c("age"),
      defined_colname_patterns = c("patientage"),
      keep = TRUE
    )
  )
  expect_true("ResponseTime_sec" %in% names(result))
})

test_that("keep = FALSE drops non-defined columns", {
  df <- data.frame(PatientAge = 25:30, ResponseTime_sec = 1:6)
  result <- suppressMessages(
    use_defined_columns(
      df,
      defined_colnames = c("age"),
      defined_colname_patterns = c("patientage"),
      keep = FALSE
    )
  )
  expect_false("ResponseTime_sec" %in% names(result))
  expect_true("age" %in% names(result))
})

test_that("length mismatch between colnames and patterns errors", {
  df <- data.frame(a = 1:3)
  expect_error(
    use_defined_columns(df,
      defined_colnames = c("x", "y"),
      defined_colname_patterns = c("a")
    )
  )
})

test_that("invalid column_required value errors", {
  df <- data.frame(PatientAge = 25:30)
  expect_error(
    use_defined_columns(
      df,
      defined_colnames = c("age"),
      defined_colname_patterns = c("patientage"),
      column_required = "mandatory"
    )
  )
})

test_that("regex pattern is case-insensitive (lowercased before matching)", {
  df <- data.frame(eSituation.01 = 1:3)
  result <- suppressMessages(
    use_defined_columns(
      df,
      defined_colnames = c("situation"),
      defined_colname_patterns = c("esituation\\.01"),
      keep = TRUE
    )
  )
  expect_true("situation" %in% names(result))
})

test_that("single column_required value expands to all columns", {
  df <- data.frame(PatientAge = 25:30, ResponseTime_sec = 1:6)
  result <- suppressMessages(
    use_defined_columns(
      df,
      defined_colnames = c("age", "response_time"),
      defined_colname_patterns = c("patientage", "responsetime"),
      column_required = "optional",
      keep = TRUE
    )
  )
  expect_true("age" %in% names(result))
  expect_true("response_time" %in% names(result))
})

test_that("vector column_required of matching length is accepted", {
  df <- data.frame(PatientAge = 25:30, ResponseTime_sec = 1:6)
  result <- suppressMessages(
    use_defined_columns(
      df,
      defined_colnames = c("age", "response_time"),
      defined_colname_patterns = c("patientage", "responsetime"),
      column_required = c("optional", "optional"),
      keep = TRUE
    )
  )
  expect_true("age" %in% names(result))
  expect_true("response_time" %in% names(result))
})
