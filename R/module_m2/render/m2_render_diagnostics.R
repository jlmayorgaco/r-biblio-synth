# ============================================================================
# m2_render_diagnostics.R - Model Diagnostics Visualizations
# ============================================================================
# Residual ACF/PACF plots, model comparison, structural break timeline

#' Render model diagnostics plots
#'
#' @param diagnostics Output from compute_m2_diagnostics
#' @param config Configuration list
#' @return List with plots
#' @export
render_m2_diagnostics <- function(diagnostics, config = biblio_config()) {
  if (is.null(diagnostics) || diagnostics$status != "success") {
    return(list(plots = list(), status = "error: invalid diagnostics"))
  }
  
  plots <- list()
  
  # Model comparison plot
  if (!is.null(diagnostics$comparison) && nrow(diagnostics$comparison) > 0) {
    plots$model_comparison <- create_model_comparison_plot(diagnostics$comparison)
  }
  
  # CV results plot
  if (!is.null(diagnostics$cv_results) &&
      is.data.frame(diagnostics$cv_results$cv_results) &&
      nrow(diagnostics$cv_results$cv_results) > 0) {
    plots$cv_accuracy <- create_cv_accuracy_plot(diagnostics$cv_results)
  }
  
  # Forecast accuracy comparison
  if (!is.null(diagnostics$accuracy) && nrow(diagnostics$accuracy) > 0) {
    plots$accuracy_comparison <- create_accuracy_comparison_plot(diagnostics$accuracy)
  }
  
  # Residual ACF/PACF plots
  if (!is.null(diagnostics$residuals)) {
    plots$diagnostics <- create_residual_diagnostics_plot(diagnostics$residuals)
  }
  
  # Model weights plot
  if (!is.null(diagnostics$weights)) {
    plots$weights <- create_model_weights_plot(diagnostics$weights)
  }
  
  list(plots = plots, status = "success")
}

#' Create model comparison plot
#' @keywords internal
create_model_comparison_plot <- function(comparison) {
  # Reshape for plotting
  models <- comparison$model
  n_models <- length(models)
  
  # AIC comparison
  df_aic <- data.frame(
    model = factor(models, levels = models),
    value = comparison$AIC,
    metric = "AIC"
  )
  
  # BIC comparison
  df_bic <- data.frame(
    model = factor(models, levels = models),
    value = comparison$BIC,
    metric = "BIC"
  )
  
  df <- rbind(df_aic, df_bic)
  
  # Normalize within metric
  df$value_norm <- ave(df$value, df$metric, FUN = function(x) (x - min(x)) / (max(x) - min(x) + 0.001))
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = model, y = value_norm, fill = metric)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.7),
                       width = 0.6, color = "black", linewidth = 0.2) +
    ggplot2::scale_fill_manual(values = c("AIC" = "#0072BD", "BIC" = "#D95319")) +
    ggplot2::coord_flip() +
    ieee_theme_bar() +
    ggplot2::labs(
      title = "Model Comparison (AIC vs BIC)",
      x = NULL,
      y = "Normalized Score"
    ) +
    ggplot2::theme(legend.position = "bottom")
  
  p
}

#' Create CV accuracy plot
#' @keywords internal
create_cv_accuracy_plot <- function(cv_results) {
  df <- cv_results$cv_results
  
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  # Plot MAE across folds
  p <- ggplot2::ggplot(df, ggplot2::aes(x = factor(fold), y = MAE, color = model, group = model)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_color_brewer(palette = "Set1", name = "Model") +
    ggplot2::scale_x_discrete(name = "CV Fold") +
    ggplot2::scale_y_continuous(name = "Mean Absolute Error") +
    ieee_theme() +
    ggplot2::labs(title = "Cross-Validation Performance") +
    ggplot2::theme(legend.position = "right")
  
  p
}

#' Create accuracy comparison plot
#' @keywords internal
create_accuracy_comparison_plot <- function(accuracy) {
  if (is.null(accuracy) || nrow(accuracy) == 0) return(NULL)
  
  # Select metrics to plot
  metrics <- c("MAE", "RMSE", "MAPE", "MASE")
  available <- intersect(metrics, names(accuracy))
  
  if (length(available) == 0) return(NULL)
  
  df_list <- list()
  for (m in available) {
    df_list[[m]] <- data.frame(
      model = accuracy$model,
      metric = m,
      value = accuracy[[m]]
    )
  }
  
  df <- do.call(rbind, df_list)
  
  # Normalize within metric
  df$value_norm <- ave(df$value, df$metric, FUN = function(x) {
    (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE) + 0.001)
  })
  
  colors <- c("MAE" = "#0072BD", "RMSE" = "#D95319", "MAPE" = "#77AC30", "MASE" = "#A2142F")
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = model, y = value_norm, fill = metric)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.7),
                       width = 0.6, color = "black", linewidth = 0.2) +
    ggplot2::scale_fill_manual(values = colors[available]) +
    ggplot2::coord_flip() +
    ieee_theme_bar() +
    ggplot2::labs(
      title = "Forecast Accuracy Comparison",
      x = NULL,
      y = "Normalized Score"
    ) +
    ggplot2::theme(legend.position = "bottom")
  
  p
}

#' Create residual diagnostics plot
#' @keywords internal
create_residual_diagnostics_plot <- function(residuals) {
  models <- names(residuals)
  n_models <- length(models)
  
  if (n_models == 0) return(NULL)
  
  plots <- list()
  
  # Select first model with residuals
  first_model <- models[1]
  res <- residuals[[first_model]]
  
  if (!is.null(res)) {
    # ACF plot
    if (!is.null(res$acf) && !is.null(res$acf$values)) {
      df_acf <- data.frame(
        lag = res$acf$lags,
        acf = res$acf$values
      )
      
      ci <- qnorm(0.975) / sqrt(length(res$acf$values))
      
      p_acf <- ggplot2::ggplot(df_acf, ggplot2::aes(x = lag, y = acf)) +
        ggplot2::geom_segment(ggplot2::aes(xend = lag, yend = 0)) +
        ggplot2::geom_hline(yintercept = c(ci, -ci), linetype = "dashed", color = "gray50") +
        ggplot2::geom_hline(yintercept = 0) +
        ggplot2::scale_x_continuous(name = "Lag") +
        ggplot2::scale_y_continuous(name = "ACF") +
        ieee_theme() +
        ggplot2::labs(title = paste("Residual ACF -", first_model))
      
      plots$acf <- p_acf
    }
    
    # PACF plot
    if (!is.null(res$pacf) && !is.null(res$pacf$values)) {
      df_pacf <- data.frame(
        lag = res$pacf$lags,
        pacf = res$pacf$values
      )
      
      ci <- qnorm(0.975) / sqrt(length(res$pacf$values))
      
      p_pacf <- ggplot2::ggplot(df_pacf, ggplot2::aes(x = lag, y = pacf)) +
        ggplot2::geom_segment(ggplot2::aes(xend = lag, yend = 0)) +
        ggplot2::geom_hline(yintercept = c(ci, -ci), linetype = "dashed", color = "gray50") +
        ggplot2::geom_hline(yintercept = 0) +
        ggplot2::scale_x_continuous(name = "Lag") +
        ggplot2::scale_y_continuous(name = "PACF") +
        ieee_theme() +
        ggplot2::labs(title = paste("Residual PACF -", first_model))
      
      plots$pacf <- p_pacf
    }
  }
  
  plots
}

#' Create model weights plot
#' @keywords internal
create_model_weights_plot <- function(weights) {
  if (is.null(weights) || length(weights) == 0) return(NULL)
  
  # Use AIC weights if available
  w <- if (!is.null(weights$aic)) weights$aic else weights[[1]]
  
  df <- data.frame(
    model = names(w),
    weight = as.numeric(w)
  )
  
  df$model <- factor(df$model, levels = df$model[order(df$weight)])
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = model, y = weight, fill = model)) +
    ggplot2::geom_col(width = 0.7, color = "black", linewidth = 0.2) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_brewer(palette = "Set1", guide = "none") +
    ggplot2::scale_y_continuous(name = "Weight", expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ieee_theme_bar() +
    ggplot2::labs(title = "Model Weights (AIC-based)", x = NULL)
  
  p
}

#' Create structural break timeline plot
#'
#' @param years Time points
#' @param articles Values
#' @param breakpoints Vector of break years
#' @param config Configuration
#' @return ggplot object
#' @export
create_breakpoint_timeline <- function(years, articles, breakpoints, config = biblio_config()) {
  df <- data.frame(year = years, articles = articles)
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = year, y = articles)) +
    ggplot2::geom_line(color = "#0072BD", linewidth = 0.8) +
    ggplot2::geom_point(color = "#0072BD", size = 1.5)
  
  if (length(breakpoints) > 0) {
    for (bp in breakpoints) {
      p <- p + ggplot2::geom_vline(xintercept = bp, color = "#D95319",
                                    linetype = "dashed", linewidth = 0.6)
    }
    
    # Add segment labels
    segment_labels <- data.frame(
      x = breakpoints,
      label = paste("Break", seq_along(breakpoints))
    )
    
    p <- p + ggplot2::annotate("text", x = segment_labels$x, y = max(articles),
                               label = segment_labels$label, vjust = -0.5,
                               color = "#D95319", size = 3)
  }
  
  p <- p +
    ggplot2::scale_x_continuous(name = "Year") +
    ggplot2::scale_y_continuous(name = "Articles") +
    ieee_theme_timeseries() +
    ggplot2::labs(
      title = "Structural Break Timeline",
      subtitle = sprintf("%d breakpoints detected", length(breakpoints))
    )
  
  p
}

#' Create growth phase diagram
#'
#' @param years Time points
#' @param articles Values
#' @param phases Optional phase assignments
#' @param config Configuration
#' @return ggplot object
#' @export
create_growth_phase_diagram <- function(years, articles, phases = NULL, config = biblio_config()) {
  df <- data.frame(year = years, articles = articles)
  
  # Identify phases from data
  if (is.null(phases)) {
    # Simple phase detection
    n <- length(articles)
    
    # Calculate growth rate
    growth <- c(NA, diff(articles) / articles[-n])
    
    # Identify phases
    phases <- rep("growth", n)
    
    # Early phase (first 20%)
    early_end <- ceiling(n * 0.2)
    phases[1:early_end] <- "early"
    
    # Detect saturation
    if (n > 5) {
      recent_growth <- mean(tail(growth, 5), na.rm = TRUE)
      if (recent_growth < 0.02) {
        saturation_start <- max(1, n - 5)
        phases[saturation_start:n] <- "saturation"
      }
    }
  }
  
  df$phase <- phases
  
  colors <- c("early" = "#77AC30", "growth" = "#0072BD", "saturation" = "#D95319")
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = year, y = articles, color = phase)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_color_manual(values = colors, name = "Phase") +
    ggplot2::scale_x_continuous(name = "Year") +
    ggplot2::scale_y_continuous(name = "Articles") +
    ieee_theme_timeseries() +
    ggplot2::labs(title = "Growth Phase Diagram") +
    ggplot2::theme(legend.position = "bottom")
  
  p
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
