test_that("hospital_cols returns the full mapping with no args", {
  all <- hospital_cols()
  expect_type(all, "character")
  expect_true(all(c("Parkland", "BUMC", "VA") %in% names(all)))
  expect_equal(unname(all[["Parkland"]]), "#B02AA5")
})

test_that("hospital_cols subsets by name", {
  expect_equal(
    hospital_cols("Methodist", "Childrens"),
    c(Methodist = "#215AA8", Childrens = "#ED2939")
  )
})

test_that("hospital_cols warns on unknown names and drops them", {
  expect_warning(out <- hospital_cols("Parkland", "Nonexistent"), "Invalid")
  expect_equal(out, c(Parkland = "#B02AA5"))
})

test_that("scale_color_hospital builds a manual scale with brand values", {
  sc <- scale_color_hospital()
  expect_s3_class(sc, "Scale")
  expect_equal(sc$aesthetics, "colour")
})

test_that("scale_*_hospital default values map system -> brand color", {
  df <- data.frame(s = c("Parkland", "VA"), y = 1:2)
  p <- ggplot2::ggplot(df, ggplot2::aes(s, y, color = s)) +
    ggplot2::geom_point() +
    scale_color_hospital()
  cols <- ggplot2::ggplot_build(p)$data[[1]]$colour
  expect_setequal(cols, c("#B02AA5", "#162E51"))

  expect_equal(scale_fill_hospital()$aesthetics, "fill")
})

test_that("scale_color_hospital accepts a values override", {
  df <- data.frame(s = "Foo", y = 1)
  p <- ggplot2::ggplot(df, ggplot2::aes(s, y, color = s)) +
    ggplot2::geom_point() +
    scale_color_hospital(values = c(Foo = "#000000"))
  expect_equal(ggplot2::ggplot_build(p)$data[[1]]$colour, "#000000")
})
