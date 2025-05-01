#' @importFrom magrittr %>%
#' @importFrom tidyr   pivot_longer
#' @importFrom dplyr   group_by summarise across

#' Summarize mtcars
#'
#' Compute mean, median, and SD for numeric columns, optionally by a grouping variable.
#' @param by A column name in `mtcars` to group by (e.g. "cyl"), or `NULL`.
#' @return A tibble of summary statistics.
#' @examples
#' summarize_mtcars()
#' summarize_mtcars("cyl")
#' @export
summarize_mtcars <- function(by = NULL) {
  data <- mtcars

  # validate grouping column
  if (!is.null(by)) {
    if (!by %in% names(data)) stop("`by` must be a column in mtcars")
    data[[by]] <- as.factor(data[[by]])
  }

  # pivot longer to get one row per value
  df <- data %>%
    tidyr::pivot_longer(
      cols      = where(is.numeric),
      names_to  = "variable",
      values_to = "value"
    )

  # decide grouping columns
  group_cols <- if (!is.null(by)) c(by, "variable") else "variable"

  # compute summaries
  df %>%
    dplyr::group_by(dplyr::across(all_of(group_cols))) %>%
    dplyr::summarise(
      mean   = mean(value, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      sd     = sd(value, na.rm = TRUE),
      .groups = "drop"
    )
}
