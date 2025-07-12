test_that("generate_data works", {
  # Run generate_data()
  df <- generate_data(.4, 100)
  # It generates a `data.frame`
  expect_s3_class(df, "data.frame")
  # All columns are `numeric`
  expect_true(all(sapply(df, inherits, what = "numeric")))
  # The number of rows corresponds to `n`
  expect_true(nrow(df) == 100)
  # At high n, the regression coefficient approaches beta within tolerance
  set.seed(1)
  df <- generate_data(.4, 100000)
  res <- lm(wellbeing ~ intrinsic_motivation + needs, data = df)
  expect_equivalent(res$coefficients[2], .4, tolerance = .01)
})
