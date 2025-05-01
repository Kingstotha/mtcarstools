#' Correlation heatmap for mtcars
#'
#' Compute correlations among selected variables and plot a heatmap.
#'
#' @param vars Character vector of column names in `mtcars`, or `NULL` to use all numeric columns.
#' @return A ggplot2 heatmap of the correlation matrix.
#' @examples
#' corr_plot_mtcars()
#' corr_plot_mtcars(c("mpg", "hp", "wt"))
#' @importFrom stats cor
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_gradient2 theme_minimal labs
#' @importFrom tidyr pivot_longer
#' @export
corr_plot_mtcars <- function(vars = NULL) {
  df <- mtcars

  # select variables
  if (is.null(vars)) {
    df <- df[ , vapply(df, is.numeric, logical(1))]
  } else {
    if (!all(vars %in% names(df))) {
      stop("All `vars` must be columns in mtcars")
    }
    df <- df[ , vars]
  }

  # compute correlation matrix
  corr_mat <- cor(df, use = "pairwise.complete.obs")

  # reshape to long format for plotting
  corr_df <- as.data.frame(corr_mat) %>%
    tibble::rownames_to_column(var = "Var1") %>%
    tidyr::pivot_longer(
      cols      = -Var1,
      names_to  = "Var2",
      values_to = "Correlation"
    )

  # plot heatmap
  ggplot2::ggplot(corr_df, ggplot2::aes(Var1, Var2, fill = Correlation)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(
      low  = "blue",
      mid  = "white",
      high = "red",
      midpoint = 0
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x    = NULL,
      y    = NULL,
      fill = "Corr"
    )
}
