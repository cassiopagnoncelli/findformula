test_that("load candidate formulas", {
  forms <- load_formulas()
  expect_gt(length(forms), 0)
})
