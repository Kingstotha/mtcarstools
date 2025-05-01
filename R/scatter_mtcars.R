#' Scatter plot for mtcars
#'
#' Quick ggplot2 scatter of two variables, optionally colored by a factor.
#'
#' @param x,y Names of numeric columns in `mtcars`.
#' @param color Optional name of a factor column (e.g. "cyl"), or `NULL`.
#' @return A ggplot2 plot object.
#' @examples
#' scatter_mtcars("wt", "mpg")
#' scatter_mtcars("hp", "mpg", "cyl")
#' @importFrom ggplot2 ggplot aes_string geom_point theme_minimal
#' @export
scatter_mtcars <- function(x, y, color = NULL) {
  df <- mtcars

  # validate arguments
  if (!x %in% names(df)) stop("`x` must be a column in mtcars")
  if (!y %in% names(df)) stop("`y` must be a column in mtcars")
  if (!is.null(color)) {
    if (!color %in% names(df)) stop("`color` must be a column in mtcars")
    df[[color]] <- as.factor(df[[color]])
  }

  # build aesthetic mapping
  mapping <- if (is.null(color)) {
    ggplot2::aes_string(x = x, y = y)
  } else {
    ggplot2::aes_string(x = x, y = y, color = color)
  }

  # construct plot
  ggplot2::ggplot(df, mapping) +
    ggplot2::geom_point() +
    ggplot2::theme_minimal()
}
