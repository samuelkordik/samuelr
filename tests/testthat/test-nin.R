`%nin%` <- samuelr:::`%nin%`

test_that("%nin% returns TRUE when x is not in table", {
  expect_true("a" %nin% c("b", "c"))
})

test_that("%nin% returns FALSE when x is in table", {
  expect_false("a" %nin% c("a", "b"))
})

test_that("%nin% is vectorized", {
  expect_equal(c("a", "b") %nin% c("b", "c"), c(TRUE, FALSE))
})

test_that("%nin% handles NA correctly (consistent with !%in%)", {
  expect_equal(NA %nin% c("a"), !(NA %in% c("a")))
})

test_that("%nin% works with numeric values", {
  expect_true(5 %nin% c(1, 2, 3))
  expect_false(2 %nin% c(1, 2, 3))
})
