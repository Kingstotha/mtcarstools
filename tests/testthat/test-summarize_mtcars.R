test_that("summarize_mtcars returns correct columns and types", {
  out <- summarize_mtcars()
  # it should be a tibble
  expect_s3_class(out, "tbl_df")
  # columns should include variable, mean, median, sd
  expect_true(all(c("variable", "mean", "median", "sd") %in% names(out)))
  # mean of mpg should equal the known value
  mpg_row <- out[out$variable == "mpg", ]
  expect_equal(round(mpg_row$mean, 2), round(mean(mtcars$mpg), 2))
})

test_that("grouped summary works", {
  out2 <- summarize_mtcars("cyl")
  expect_true("cyl" %in% names(out2))
  # number of rows equals number of cyl groups × number of variables
  nvars <- length(dplyr::select_if(mtcars, is.numeric))
  ngroups <- length(unique(mtcars$cyl))
  expect_equal(nrow(out2), nvars * ngroups)
})
