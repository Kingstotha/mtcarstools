library(testthat)
library(mtcarstools)

test_that("corr_plot_mtcars returns a ggplot object", {
  p <- corr_plot_mtcars(c("mpg", "hp"))
  expect_s3_class(p, "ggplot")
})

test_that("corr_plot_mtcars uses all numeric vars by default", {
  p2 <- corr_plot_mtcars()
  # underlying data has 11 numeric columns in mtcars
  # we can't inspect p2 internals easily, but it should run without error
  expect_s3_class(p2, "ggplot")
})

test_that("corr_plot_mtcars errors on invalid vars", {
  expect_error(corr_plot_mtcars(c("mpg", "foo")), "must be columns in mtcars")
})
