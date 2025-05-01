library(testthat)
library(mtcarstools)

test_that("scatter_mtcars returns a ggplot object", {
  p <- scatter_mtcars("wt", "mpg")
  expect_s3_class(p, "ggplot")
})

test_that("scatter_mtcars errors on bad inputs", {
  expect_error(scatter_mtcars("foo", "mpg"), "`x` must be a column")
  expect_error(scatter_mtcars("wt", "bar"), "`y` must be a column")
})
