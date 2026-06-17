test_that("dfr_cols() with no args returns full named list", {
  result <- dfr_cols()
  expect_type(result, "list")
  expect_length(result, 12)
  expect_true("omd_navy" %in% names(result))
  expect_true("dfr_gold" %in% names(result))
})

test_that("dfr_cols returns correct hex for a named color", {
  result <- dfr_cols("omd_navy")
  expect_equal(unname(result), "#00286F")
})

test_that("dfr_cols returns multiple colors as character vector", {
  result <- dfr_cols("omd_navy", "omd_red")
  expect_length(result, 2)
  expect_type(result, "character")
  expect_equal(unname(result[2]), "#d80000")
})

test_that("dfr_cols warns on invalid color name", {
  expect_warning(dfr_cols("not_a_color"), "Invalid color names")
})

test_that("dfr_pal returns a function", {
  pal_fn <- dfr_pal("main")
  expect_type(pal_fn, "closure")
})

test_that("dfr_pal function returns correct number of colors", {
  pal_fn <- dfr_pal("main")
  colors <- pal_fn(4)
  expect_length(colors, 4)
  expect_type(colors, "character")
})

test_that("dfr_pal navy palette works", {
  pal_fn <- dfr_pal("navy")
  colors <- pal_fn(6)
  expect_length(colors, 6)
})

test_that("dfr_pal reverse = TRUE reverses palette", {
  fwd <- dfr_pal("main")(4)
  rev_colors <- dfr_pal("main", reverse = TRUE)(4)
  expect_false(identical(fwd, rev_colors))
  expect_equal(fwd, rev(rev_colors))
})

test_that("scale_color_dfr returns a discrete ggplot2 scale", {
  s <- samuelr:::scale_color_dfr()
  expect_s3_class(s, "ScaleDiscrete")
})

test_that("scale_color_dfr continuous = FALSE returns continuous scale", {
  s <- samuelr:::scale_color_dfr(discrete = FALSE)
  expect_s3_class(s, "Scale")
})

test_that("scale_fill_dfr returns a discrete ggplot2 scale", {
  s <- samuelr:::scale_fill_dfr()
  expect_s3_class(s, "ScaleDiscrete")
})

test_that("scale_fill_dfr continuous = FALSE returns continuous scale", {
  s <- samuelr:::scale_fill_dfr(discrete = FALSE)
  expect_s3_class(s, "Scale")
})
