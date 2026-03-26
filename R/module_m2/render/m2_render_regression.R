# ============================================================================
# module_m2/render/m2_render_regression.R - Regression plots (FIXED)
# ============================================================================

#' @export
render_m2_regression <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"comparison_table" %in% names(result)) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  plots <- list()

  # Model comparison
  comp <- result$comparison_table
  if (nrow(comp) > 0) {
    plots$model_comparison <- ggplot2::ggplot(comp, ggplot2::aes(x = reorder(Model, R2), y = R2)) +
      ggplot2::geom_bar(stat = "identity", fill = ieee_colors$blue, color = "black", linewidth = 0.2) +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "Regression Model Comparison (R²)", x = "Model", y = "R²") +
      ieee_theme(base_size = 8)
  }

  # Residuals
  if (!is.null(result$performance) && length(result$performance$residuals) > 0) {
    resid_df <- data.frame(index = seq_along(result$performance$residuals), std_residuals = result$performance$std_residuals)
    plots$residuals <- ggplot2::ggplot(resid_df, ggplot2::aes(x = index, y = std_residuals)) +
      ggplot2::geom_point(color = ieee_colors$blue, size = 1.5) +
      ggplot2::geom_hline(yintercept = c(-2, 2), linetype = "dashed", color = ieee_colors$red, linewidth = 0.3) +
      ggplot2::labs(title = "Standardized Residuals", x = "Observation", y = "Std. Residual") +
      ieee_theme(base_size = 8)
  }

  list(status = "success", plots = plots, tables = list())
}
