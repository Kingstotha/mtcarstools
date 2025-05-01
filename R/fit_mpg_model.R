#' Fit an MPG linear model
#'
#' Fit a linear regression predicting `mpg` from selected predictors.
#'
#' @param predictors Character vector of column names in `mtcars` to use as predictors,
#'   or `NULL` to use all other numeric columns.
#' @return A list with two elements:
#'   - `coefficients`: a tibble of model coefficients (via `broom::tidy()`)
#'   - `summary`: a tibble of model‐level statistics (via `broom::glance()`)
#' @examples
#' fit_mpg_model()
#' fit_mpg_model(c("hp", "wt"))
#' @importFrom stats lm
#' @importFrom broom tidy glance
#' @export
fit_mpg_model <- function(predictors = NULL) {
  data <- mtcars

  # choose predictors
  if (is.null(predictors)) {
    predictors <- names(data)[vapply(data, is.numeric, logical(1))]
    predictors <- setdiff(predictors, "mpg")
  } else {
    if (!all(predictors %in% names(data))) {
      stop("All `predictors` must be columns in mtcars")
    }
  }

  # build formula
  form <- stats::as.formula(
    paste("mpg ~", paste(predictors, collapse = " + "))
  )

  # fit model
  model <- stats::lm(form, data = data)

  # return tidy outputs
  list(
    coefficients = broom::tidy(model),
    summary      = broom::glance(model)
  )
}
