library(testthat)
library(mtcarstools)

test_that("fit_mpg_model returns a list of tibbles", {
  res <- fit_mpg_model(c("hp", "wt"))
  expect_type(res, "list")
  expect_s3_class(res$coefficients, "tbl_df")
  expect_s3_class(res$summary,      "tbl_df")
})

test_that("fit_mpg_model default uses all numeric predictors except mpg", {
  res_def <- fit_mpg_model()
  # intercept should be present
  expect_true("(Intercept)" %in% res_def$coefficients$term)
})

test_that("fit_mpg_model errors on invalid predictors", {
  expect_error(fit_mpg_model(c("foo")), "must be columns in mtcars")
})
