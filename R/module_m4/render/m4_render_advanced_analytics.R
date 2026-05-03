# ============================================================================
# m4_render_advanced_analytics.R - Advanced source analytics plots
# ============================================================================

render_m4_advanced_analytics <- function(result, config = biblio_config()) {
  list(status = result$status %||% "stub", plots = list(
    archetype_map = m4_plot_archetype_map(result$features %||% tibble::tibble()),
    anomaly_rank = m4_plot_anomaly_rank(result$outliers %||% tibble::tibble(), result$features %||% tibble::tibble()),
    ml_probability = m4_plot_ml_probability(result$svm %||% list(), result$features %||% tibble::tibble()),
    ml_cv_comparison = m4_plot_ml_cv_comparison(result$ml_cv %||% list()),
    regression_fit = m4_plot_regression_fit(result$regression %||% list(), result$features %||% tibble::tibble()),
    silhouette = m4_plot_silhouette(result$silhouette %||% list())
  ))
}

m4_plot_ml_cv_comparison <- function(ml_cv) {
  summary <- ml_cv$summary %||% tibble::tibble()
  if (!is.data.frame(summary) || nrow(summary) == 0) return(NULL)
  df <- summary |>
    tidyr::pivot_longer(cols = c("accuracy", "balanced_accuracy", "f1", "auc"), names_to = "metric", values_to = "value")
  ggplot2::ggplot(df, ggplot2::aes(x = model, y = value, fill = metric)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.72), color = "#222222", linewidth = 0.16, width = 0.65) +
    ggplot2::scale_y_continuous("Cross-validated score", limits = c(0, 1), labels = scales::label_percent(accuracy = 1)) +
    ggplot2::scale_fill_manual(values = get_ieee_palette(length(unique(df$metric)), "primary"), name = "Metric") +
    ggplot2::labs(title = "Cross-validated ML Model Comparison", subtitle = sprintf("%d-fold source-level validation for high-impact source classification.", ml_cv$k %||% NA_integer_), x = NULL) +
    ieee_theme_wide(base_size = 8.5) +
    ggplot2::theme(legend.position = "bottom")
}

m4_plot_archetype_map <- function(features) {
  if (!is.data.frame(features) || nrow(features) == 0) return(NULL)
  df <- features |>
    dplyr::arrange(dplyr::desc(.data$tp + .data$tc)) |>
    dplyr::slice(seq_len(min(30L, nrow(features))))
  ggplot2::ggplot(df, ggplot2::aes(x = tp_slope, y = cpp, fill = archetype)) +
    ggplot2::geom_hline(yintercept = stats::median(df$cpp, na.rm = TRUE), linetype = "dashed", color = "#666666", linewidth = 0.3) +
    ggplot2::geom_vline(xintercept = stats::median(df$tp_slope, na.rm = TRUE), linetype = "dashed", color = "#666666", linewidth = 0.3) +
    ggplot2::geom_point(ggplot2::aes(size = tp), shape = 21, color = "#222222", stroke = 0.25, alpha = 0.9) +
    ggplot2::geom_text(ggplot2::aes(label = source_label), size = 2.15, vjust = -0.75, check_overlap = TRUE) +
    ggplot2::scale_fill_manual(values = get_ieee_palette(length(unique(df$archetype)), "primary"), name = "Archetype") +
    ggplot2::scale_size_continuous(name = "TP", range = c(2.2, 8), labels = scales::label_number(big.mark = ",")) +
    ggplot2::labs(
      title = "Source Archetype Map",
      subtitle = "Rules classify venues into core engines, selective high-impact venues, volume-heavy venues, emerging venues, and niches.",
      x = "TP slope (papers/year)",
      y = "Citations per paper (CPP)"
    ) +
    ieee_theme_wide(base_size = 8.5) +
    ggplot2::theme(legend.position = "bottom")
}

m4_plot_anomaly_rank <- function(outliers, features) {
  if (!is.data.frame(outliers) || nrow(outliers) == 0) return(NULL)
  df <- outliers |>
    dplyr::left_join(features |> dplyr::select("source", "archetype"), by = "source") |>
    dplyr::arrange(dplyr::desc(.data$anomaly_score)) |>
    dplyr::slice(seq_len(min(20L, nrow(outliers))))
  df$source_label <- factor(substr(df$source, 1, 45), levels = rev(substr(df$source, 1, 45)))
  ggplot2::ggplot(df, ggplot2::aes(x = source_label, y = anomaly_score, fill = anomaly_flag)) +
    ggplot2::geom_col(color = "#222222", linewidth = 0.18, width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c(`TRUE` = "#B22222", `FALSE` = "#4C78A8"), name = "Flagged") +
    ggplot2::labs(
      title = "Source Anomaly Profile",
      subtitle = "Distance from the multivariate source profile identifies unusual venue behavior.",
      x = NULL,
      y = "Anomaly score"
    ) +
    ieee_theme_wide(base_size = 8.5) +
    ggplot2::theme(legend.position = "bottom")
}

m4_plot_ml_probability <- function(svm, features) {
  predictions <- svm$predictions %||% tibble::tibble()
  if (!is.data.frame(predictions) || nrow(predictions) == 0) return(NULL)
  df <- predictions |>
    dplyr::left_join(features |> dplyr::select("source", "tp", "tc", "archetype"), by = "source") |>
    dplyr::arrange(dplyr::desc(.data$high_impact_probability)) |>
    dplyr::slice(seq_len(min(25L, nrow(predictions))))
  df$source_label <- factor(substr(df$source, 1, 45), levels = rev(substr(df$source, 1, 45)))
  ggplot2::ggplot(df, ggplot2::aes(x = source_label, y = high_impact_probability, fill = observed)) +
    ggplot2::geom_col(color = "#222222", linewidth = 0.18, width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", color = "#666666", linewidth = 0.3) +
    ggplot2::scale_y_continuous("Predicted high-impact probability", labels = scales::label_percent(accuracy = 1), limits = c(0, 1)) +
    ggplot2::scale_fill_manual(values = c(high_impact = "#B22222", lower_impact = "#4C78A8"), name = "Observed class") +
    ggplot2::labs(
      title = "ML Classifier: High-Impact Source Probability",
      subtitle = sprintf("Model: %s; in-sample accuracy: %.1f%%. Treat as pattern evidence.", svm$model %||% "unavailable", 100 * ieee_safe_num(svm$accuracy, 0)),
      x = NULL
    ) +
    ieee_theme_wide(base_size = 8.5) +
    ggplot2::theme(legend.position = "bottom")
}

m4_plot_regression_fit <- function(regression, features) {
  fitted <- regression$fitted %||% tibble::tibble()
  if (!is.data.frame(fitted) || nrow(fitted) == 0) return(NULL)
  fitted <- fitted |>
    dplyr::left_join(features |> dplyr::select("source", "archetype", "source_label"), by = "source")
  ggplot2::ggplot(fitted, ggplot2::aes(x = predicted_log_tc, y = observed_log_tc, fill = archetype)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#666666", linewidth = 0.35) +
    ggplot2::geom_point(shape = 21, color = "#222222", stroke = 0.25, size = 3.4, alpha = 0.9) +
    ggplot2::geom_text(ggplot2::aes(label = source_label), size = 2.15, vjust = -0.85, check_overlap = TRUE) +
    ggplot2::scale_fill_manual(values = get_ieee_palette(length(unique(fitted$archetype)), "primary"), name = "Archetype") +
    ggplot2::labs(
      title = "Impact Regression Fit",
      subtitle = sprintf("Observed vs predicted log(TC); R-squared = %.3f.", ieee_safe_num(regression$r_squared)),
      x = "Predicted log(TC + 1)",
      y = "Observed log(TC + 1)"
    ) +
    ieee_theme_wide(base_size = 8.5) +
    ggplot2::theme(legend.position = "bottom")
}

m4_plot_silhouette <- function(silhouette) {
  table <- silhouette$table %||% tibble::tibble()
  if (!is.data.frame(table) || nrow(table) == 0) return(NULL)
  table$source_label <- factor(substr(table$source, 1, 42), levels = table$source[order(table$silhouette_width)])
  ggplot2::ggplot(table, ggplot2::aes(x = source_label, y = silhouette_width, fill = cluster)) +
    ggplot2::geom_col(color = "#222222", linewidth = 0.12, width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::geom_hline(yintercept = silhouette$mean_silhouette %||% NA_real_, linetype = "dashed", color = "#B22222", linewidth = 0.35) +
    ggplot2::scale_fill_manual(values = get_ieee_palette(length(unique(table$cluster)), "primary"), name = "Cluster") +
    ggplot2::labs(
      title = "K-means Cluster Separation",
      subtitle = sprintf("Mean silhouette = %.3f; higher values indicate more coherent source clusters.", ieee_safe_num(silhouette$mean_silhouette)),
      x = NULL,
      y = "Silhouette width"
    ) +
    ieee_theme_wide(base_size = 8.5) +
    ggplot2::theme(legend.position = "bottom")
}
