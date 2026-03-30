# ============================================================================
# m2_render_regression.R - Enhanced Regression Plotting for IEEE Q1
# ============================================================================
# Renders publication-quality regression plots with:
# - Data points with error visualization
# - Fitted curves with confidence bands
# - Model equations and R² displayed
# - Multi-model comparison panels
# - Residual diagnostic plots
# ============================================================================

#' Render all regression plots (Enhanced IEEE Q1 Quality)
#'
#' @param result Regression data from compute_m2_regression
#' @param config Configuration list
#' @return List of plot objects
#' @export
render_m2_regression <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"comparison_table" %in% names(result)) {
    return(list(status = "stub", plots = list(), tables = list()))
  }
  
  if (result$status != "success" || is.null(result$models) || length(result$models) == 0) {
    return(list(status = "error", plots = list(), tables = list()))
  }
  
  plots <- list()
  
  model_data <- tryCatch({
    extract_model_data(result)
  }, error = function(e) {
    list(raw = data.frame(), fitted = data.frame(), all_fitted = data.frame())
  })
  
  plots$best_model <- tryCatch({
    render_best_model_plot(result, model_data, config)
  }, error = function(e) NULL)
  
  plots$model_comparison <- tryCatch({
    render_model_comparison_plot(result, config)
  }, error = function(e) NULL)
  
  plots$all_models <- tryCatch({
    render_all_models_plot(result, model_data, config)
  }, error = function(e) NULL)
  
  plots$residuals <- tryCatch({
    render_residual_diagnostics(result, model_data, config)
  }, error = function(e) NULL)
  
  plots$equation <- tryCatch({
    render_equation_plot(result, config)
  }, error = function(e) NULL)
  
  list(status = "success", plots = plots, tables = list())
}

#' Render plot with best model highlighted
#' @keywords internal
render_best_model_plot <- function(data, model_data, config) {
  if (is.null(data$best_model) || data$best_model$name == "none") return(NULL)
  if (nrow(model_data$raw) == 0) return(NULL)
  
  best_name <- data$best_model$name
  comparison <- data$comparison_table
  best_r2 <- comparison$R2[comparison$Model == best_name]
  best_rmse <- comparison$RMSE[comparison$Model == best_name]
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = model_data$raw,
      ggplot2::aes(x = Year, y = Articles),
      color = "#0072BD",
      size = 2,
      alpha = 0.8,
      shape = 16
    ) +
    ggplot2::geom_line(
      data = model_data$fitted,
      ggplot2::aes(x = Year, y = Fitted),
      color = "#D95319",
      linewidth = 0.8
    ) +
    ggplot2::geom_ribbon(
      data = model_data$fitted,
      ggplot2::aes(x = Year, ymin = Lower, ymax = Upper),
      fill = "#D95319",
      alpha = 0.15
    ) +
    ggplot2::annotate(
      "text",
      x = Inf, y = Inf,
      label = sprintf("%s\n%s\nR² = %.4f\nRMSE = %.2f", 
                      best_name,
                      get_model_equation(best_name),
                      best_r2,
                      best_rmse),
      hjust = 1.05, vjust = 1.5,
      size = 3, fontface = "bold", family = "mono"
    ) +
    ggplot2::scale_x_continuous(
      name = "Year",
      breaks = scales::breaks_pretty(n = 6),
      labels = scales::label_number(accuracy = 1)
    ) +
    ggplot2::scale_y_continuous(
      name = "Number of Publications",
      labels = scales::label_number(big.mark = ",")
    ) +
    ieee_theme() +
    ggplot2::labs(
      title = paste("Best-Fit Model:", best_name),
      subtitle = "Observed data (blue) with fitted curve (orange) and 95% CI"
    )
  
  p
}

#' Render model comparison bar chart
#' @keywords internal
render_model_comparison_plot <- function(data, config) {
  comparison <- data$comparison_table
  if (is.null(comparison) || nrow(comparison) == 0) return(NULL)
  
  plot_data <- comparison[order(-comparison$R2), ]
  plot_data$Model <- factor(plot_data$Model, levels = plot_data$Model)
  best_name <- plot_data$Model[1]
  plot_data$Highlight <- ifelse(plot_data$Model == best_name, "Best", "Other")
  
  p1 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Model, y = R2, fill = Highlight)) +
    ggplot2::geom_bar(stat = "identity", width = 0.7, color = "black", linewidth = 0.3) +
    ggplot2::scale_fill_manual(values = c("Best" = "#0072BD", "Other" = "#CCCCCC"), guide = "none") +
    ggplot2::scale_y_continuous(
      name = expression(R^2),
      limits = c(0, 1),
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ieee_theme_bar() +
    ggplot2::labs(title = "Model Fit Comparison (R²)") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  p2 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Model, y = RMSE, fill = Highlight)) +
    ggplot2::geom_bar(stat = "identity", width = 0.7, color = "black", linewidth = 0.3) +
    ggplot2::scale_fill_manual(values = c("Best" = "#77AC30", "Other" = "#CCCCCC"), guide = "none") +
    ggplot2::scale_y_continuous(name = "RMSE", expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ieee_theme_bar() +
    ggplot2::labs(title = "Prediction Error (RMSE)") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  p3 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Model, y = AIC, fill = Highlight)) +
    ggplot2::geom_bar(stat = "identity", width = 0.7, color = "black", linewidth = 0.3) +
    ggplot2::scale_fill_manual(values = c("Best" = "#D95319", "Other" = "#CCCCCC"), guide = "none") +
    ggplot2::scale_y_continuous(name = "AIC", expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ieee_theme_bar() +
    ggplot2::labs(title = "Model Complexity (AIC)") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  list(R2 = p1, RMSE = p2, AIC = p3)
}

#' Render all fitted models overlay
#' @keywords internal
render_all_models_plot <- function(data, model_data, config) {
  if (is.null(data$models) || length(data$models) == 0) return(NULL)
  if (nrow(model_data$raw) == 0) return(NULL)
  
  raw_data <- model_data$raw
  model_names <- names(data$models)
  colors <- get_ieee_palette(length(model_names))
  
  years <- raw_data$Year
  year_seq <- seq(min(years, na.rm = TRUE), max(years, na.rm = TRUE), length.out = 100)
  
  all_fitted <- data.frame()
  
  for (i in seq_along(data$models)) {
    model_name <- model_names[i]
    model <- data$models[[model_names[i]]]
    
    fitted_vals <- tryCatch(
      predict(model, newdata = data.frame(Year = year_seq)),
      error = function(e) rep(NA, length(year_seq))
    )
    
    if (!all(is.na(fitted_vals))) {
      all_fitted <- rbind(all_fitted, data.frame(
        Year = year_seq,
        Articles = as.numeric(fitted_vals),
        Model = model_name
      ))
    }
  }
  
  if (nrow(all_fitted) == 0) return(NULL)
  
  all_fitted$Model <- factor(all_fitted$Model, levels = unique(all_fitted$Model))
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = raw_data,
      ggplot2::aes(x = Year, y = Articles),
      color = "#333333",
      size = 2.5,
      shape = 16,
      alpha = 0.8
    ) +
    ggplot2::geom_line(
      data = all_fitted,
      ggplot2::aes(x = Year, y = Articles, color = Model, linetype = Model),
      linewidth = 0.8
    ) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_linetype_manual(values = rep(c("solid", "dashed", "dotted", "dotdash"), length.out = length(model_names))) +
    ggplot2::scale_x_continuous(
      name = "Year",
      breaks = scales::breaks_pretty(n = 6)
    ) +
    ggplot2::scale_y_continuous(
      name = "Number of Publications",
      labels = scales::label_number(big.mark = ",")
    ) +
    ieee_theme() +
    ggplot2::labs(
      title = "Growth Model Comparison",
      subtitle = "Observed data with fitted curves for all candidate models"
    ) +
    ggplot2::theme(
      legend.position = "right",
      legend.direction = "vertical",
      legend.key.height = ggplot2::unit(0.4, "cm")
    )
  
  p
}

#' Render residual diagnostics
#' @keywords internal
render_residual_diagnostics <- function(data, model_data, config) {
  if (is.null(data$performance) || length(data$performance$residuals) == 0) return(NULL)
  
  residuals <- data$performance$residuals
  std_resid <- data$performance$std_residuals
  years <- model_data$raw$Year
  fitted <- model_data$raw$Articles - residuals
  
  res_df <- data.frame(
    Year = years,
    Residual = residuals,
    Std_Resid = std_resid,
    Fitted = fitted
  )
  
  p1 <- ggplot2::ggplot(res_df, ggplot2::aes(x = Fitted, y = Std_Resid)) +
    ggplot2::geom_point(color = "#0072BD", size = 2, alpha = 0.7) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "#666666", linewidth = 0.3) +
    ggplot2::geom_smooth(method = "loess", se = FALSE, color = "#D95319", linewidth = 0.6, span = 0.8) +
    ggplot2::scale_x_continuous(name = "Fitted Values", labels = scales::label_number(big.mark = ",")) +
    ggplot2::scale_y_continuous(name = "Standardized Residuals") +
    ieee_theme() +
    ggplot2::labs(
      title = "Residuals vs Fitted",
      subtitle = sprintf("Shapiro-Wilk p = %.3f | DW = %.3f",
                         ifelse(is.null(data$performance$shapiro_p), NA, 
                                format(data$performance$shapiro_p, digits = 3)),
                         ifelse(is.null(data$performance$dw_statistic), NA,
                                format(data$performance$dw_statistic, digits = 3)))
    )
  
  p2 <- ggplot2::ggplot(res_df, ggplot2::aes(sample = Std_Resid)) +
    ggplot2::geom_qq(color = "#0072BD", size = 2, alpha = 0.7) +
    ggplot2::geom_qq_line(color = "#D95319", linewidth = 0.6) +
    ggplot2::scale_x_continuous(name = "Theoretical Quantiles") +
    ggplot2::scale_y_continuous(name = "Sample Quantiles") +
    ieee_theme() +
    ggplot2::labs(title = "Normal Q-Q Plot")
  
  p3 <- ggplot2::ggplot(res_df, ggplot2::aes(x = Residual)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(density)),
      bins = 15,
      fill = "#0072BD",
      color = "black",
      linewidth = 0.3,
      alpha = 0.7
    ) +
    ggplot2::geom_density(color = "#D95319", linewidth = 0.8) +
    ggplot2::scale_x_continuous(name = "Residuals") +
    ggplot2::scale_y_continuous(name = "Density") +
    ieee_theme() +
    ggplot2::labs(title = "Residual Distribution")
  
  acf_vals <- data$performance$acf
  p4 <- NULL
  if (!is.null(acf_vals) && length(acf_vals) > 0) {
    acf_df <- data.frame(Lag = seq_along(acf_vals), ACF = as.numeric(acf_vals))
    ci <- qnorm(0.975) / sqrt(nrow(res_df))
    
    p4 <- ggplot2::ggplot(acf_df, ggplot2::aes(x = Lag, y = ACF)) +
      ggplot2::geom_bar(stat = "identity", fill = "#0072BD", width = 0.6) +
      ggplot2::geom_hline(yintercept = c(-ci, ci), linetype = "dashed", color = "#D95319", linewidth = 0.4) +
      ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
      ggplot2::scale_x_continuous(name = "Lag") +
      ggplot2::scale_y_continuous(name = "Autocorrelation", limits = c(-1, 1)) +
      ieee_theme() +
      ggplot2::labs(title = "ACF of Residuals", subtitle = "Dashed lines: 95% CI")
  }
  
  list(residuals_vs_fitted = p1, qq_plot = p2, histogram = p3, acf = p4)
}

#' Render equation display
#' @keywords internal
render_equation_plot <- function(data, config) {
  if (is.null(data$comparison_table) || nrow(data$comparison_table) == 0) return(NULL)
  
  comparison <- data$comparison_table
  best_name <- if (!is.null(data$best_model)) data$best_model$name else comparison$Model[1]
  
  eq_text <- paste0(
    "Best Model: ", best_name, "\n\n",
    "Model Comparison:\n",
    paste(sprintf("%-12s R²=%.4f RMSE=%.2f", 
                  comparison$Model, 
                  comparison$R2, 
                  comparison$RMSE),
          collapse = "\n")
  )
  
  p <- ggplot2::ggplot() +
    ggplot2::annotate(
      "text",
      x = 0.5, y = 0.5,
      label = eq_text,
      hjust = 0, vjust = 0.5,
      size = 3.5,
      family = "mono",
      fontface = "plain"
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ieee_theme() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::labs(title = "Model Comparison Summary")
  
  p
}

#' Extract model data for plotting
#' @keywords internal
extract_model_data <- function(data) {
  if (is.null(data$models) || length(data$models) == 0) {
    return(list(raw = data.frame(), fitted = data.frame()))
  }
  
  first_model <- data$models[[1]]
  if (is.null(first_model)) {
    return(list(raw = data.frame(), fitted = data.frame()))
  }
  
  raw_df <- tryCatch({
    mf <- model.frame(first_model)
    data.frame(
      Year = mf$Year,
      Articles = mf$Articles
    )
  }, error = function(e) {
    data.frame(Year = numeric(), Articles = numeric())
  })
  
  if (nrow(raw_df) == 0 || !("Year" %in% names(raw_df))) {
    return(list(raw = data.frame(), fitted = data.frame()))
  }
  
  min_year <- min(raw_df$Year, na.rm = TRUE)
  max_year <- max(raw_df$Year, na.rm = TRUE)
  year_seq <- seq(min_year, max_year, length.out = 100)
  
  best_model <- data$models[[data$best_model$name]]
  if (is.null(best_model)) best_model <- data$models[[1]]
  
  fitted_vals <- tryCatch(
    predict(best_model, newdata = data.frame(Year = year_seq)),
    error = function(e) rep(NA, length(year_seq))
  )
  
  se_vals <- tryCatch({
    pred_obj <- predict(best_model, newdata = data.frame(Year = year_seq), se.fit = TRUE)
    if (is.list(pred_obj) && "se.fit" %in% names(pred_obj)) {
      pred_obj$se.fit
    } else {
      rep(NA, length(year_seq))
    }
  }, error = function(e) rep(NA, length(year_seq)))
  
  lower <- fitted_vals - 1.96 * ifelse(is.na(se_vals), 0, se_vals)
  upper <- fitted_vals + 1.96 * ifelse(is.na(se_vals), 0, se_vals)
  
  fitted_df <- data.frame(
    Year = year_seq,
    Fitted = as.numeric(fitted_vals),
    Lower = as.numeric(lower),
    Upper = as.numeric(upper)
  )
  
  list(raw = raw_df, fitted = fitted_df)
}

#' Get model equation string
#' @keywords internal
get_model_equation <- function(model_name) {
  switch(tolower(model_name),
    "linear" = "y = α + β·x",
    "polynomial" = "y = α + β₁x + β₂x² + β₃x³",
    "exponential" = "y = N₀·e^(r·x)",
    "logarithmic" = "y = α + β·ln(x)",
    "logistic" = "y = K / (1 + e^(-r(x-t₀)))",
    "gompertz" = "y = N₀·exp(-exp(-k(x-t₀)))",
    "weibull" = "y = K·(1 - exp(-(x/t₀)^α))",
    "vonbertalanffy" = "y = L∞·(1 - exp(-k(x-t₀)))",
    "normal" = "y = A·exp(-(x-μ)²/(2σ²))",
    "spline" = "y = Σ βᵢBᵢ(x)",
    "richards" = "y = K/(1+ν·e^(-r(x-t₀)))^(1/ν)",
    "fourier" = "y = a₀ + Σaₙcos(nωx) + bₙsin(nωx)",
    model_name
  )
}