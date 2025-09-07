# plots/regression_plot.R
plot_regression <- function(
  df,
  model,
  model_name = "Model",
  title = NULL,
  xlab = "Year",
  ylab = "Articles",
  r2_threshold = 0.7
) {
  library(latex2exp)

  if (is.null(model)) return(NULL)
  stopifnot(all(c("Year", "Value") %in% names(df)))

  # --- Predictions via R6 method ---
  df$Pred <- tryCatch(model$predict(df), error = function(e) NA_real_)

  # --- Summary & metrics (robust access + fallbacks) ---
  smry <- tryCatch(model$summary(), error = function(e) NULL)
  r2   <- tryCatch(round(smry$stats$R2, 3),    error = function(e) NA_real_)
  adjr2<- tryCatch(round(smry$stats$AdjR2, 3), error = function(e) NA_real_)
  aic  <- tryCatch(round(smry$stats$AIC, 2),   error = function(e) NA_real_)
  bic  <- tryCatch(round(smry$stats$BIC, 2),   error = function(e) NA_real_)
  rse  <- tryCatch(round(smry$stats$RSE, 3),   error = function(e) NA_real_)

  # Residuals from predictions
  resid <- df$Value - df$Pred
  resid <- resid[is.finite(resid)]
  rmse  <- if (length(resid)) round(sqrt(mean(resid^2, na.rm = TRUE)), 3) else NA_real_
  nrmse_mean <- rmse / mean(df$Value, na.rm=TRUE)

  # Pick the "other fit metric" in priority order
  fit2_label <-
    if (!is.na(adjr2)) paste0("Adj R² = ", formatC(adjr2, format = "f", digits = 3))
    else if (!is.na(aic)) paste0("AIC = ", aic)
    else if (!is.na(bic)) paste0("BIC = ", bic)
    else if (!is.na(rse)) paste0("RSE = ", rse)
    else "—"

  # Capitalize first letter of model name
  model_name_cap <- if (nzchar(model_name)) {
    paste0(toupper(substr(model_name, 1, 1)), substr(model_name, 2, nchar(model_name)))
  } else {
    "Model"
  }

  # Compose the 4-line label
  lbl <- paste(
    model_name_cap,
    paste0("R² = ", ifelse(is.na(r2), "NA", formatC(r2, format = "f", digits = 3))),
    fit2_label,
    paste0("RMSE = ", ifelse(is.na(nrmse_mean), "NA", formatC(nrmse_mean, format = "f", digits = 3))),
    sep = "\n"
  )

  # --- Base plot (IEEE compact style) ---
  p <- ggplot2::ggplot(df, ggplot2::aes(x = Year, y = Value)) +
    ggplot2::geom_point(shape = 20, size = 1, color = "black") +
    ggplot2::labs(x = xlab, y = ylab, title = title) +
    ggplot2::theme_minimal(base_size = 8, base_family = "Times New Roman") +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white", color = "black"),
      axis.title       = ggplot2::element_text(size = 8, family = "serif"),
      axis.text        = ggplot2::element_text(size = 7, family = "serif"),
      axis.ticks       = ggplot2::element_line(linewidth = 0.3),
      axis.line        = ggplot2::element_line(color = "black", linewidth = 0.5),
      panel.grid.major = ggplot2::element_line(linewidth = 0.3, color = "#dddddd"),
      panel.grid.minor = ggplot2::element_line(linewidth = 0.2, color = "#eeeeee"),
      panel.border     = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.5),
      plot.title       = ggplot2::element_text(face = "bold", hjust = 0.5, size = 9, family = "serif"),
      plot.margin      = ggplot2::margin(5, 5, 5, 5)
    )

  # Regression line only if fit is reasonable (keeps your behavior)
  if (!is.na(r2) && r2 >= r2_threshold) {
    p <- p + ggplot2::geom_line(
      ggplot2::aes(y = Pred),
      color = "black",
      linewidth = 0.5,
      na.rm = TRUE
    )

    # Optional model guides (if available)
    if (exists("add_model_guides", mode = "function")) {
      p <- add_model_guides(
        p         = p,
        model     = model,
        df        = df,
        colors    = list(x = "darkred", y = "darkgreen", none = "blue"),
        line_width= 0.4,
        text_size = 2.5,
        font_family = "Times New Roman"
      )
    }
  }

  # --- Single 4-line label, no white box ---
  xr <- range(df$Year,  na.rm = TRUE)
  yr <- range(df$Value, na.rm = TRUE)
  x_ann <- xr[1] + diff(xr) * 0.02
  y_ann <- yr[1] + diff(yr) * 0.10

  p <- p + ggplot2::geom_text(
    data = data.frame(Year = x_ann, Value = y_ann, label = lbl),
    ggplot2::aes(x = Year, y = Value, label = label),
    hjust = 0, vjust = 0,
    family = "Times New Roman",
    size = 2.5
  )

  return(p)
}



plot_tp_vs_tc <- function(df_tp, df_tc, corr) {
  merged <- dplyr::inner_join(df_tp, df_tc, by = "Year", suffix = c("_TP", "_TC"))

  p <- ggplot2::ggplot(merged, ggplot2::aes(x = Value_TP, y = Value_TC)) +
    ggplot2::geom_point(shape = 20, size = 1.2, color = "black") +
    ggplot2::geom_smooth(method = "loess", se = FALSE, color = "darkred", linewidth = 0.5) +
    ggplot2::labs(
      title = paste0("TP vs TC (r = ", round(corr, 3), ")"),
      x = "Total Publications",
      y = "Total Citations"
    ) +
    ggplot2::theme_minimal(base_size = 8, base_family = "Times New Roman") +
    ggplot2::theme(
      panel.background = element_rect(fill = "white", color = "black"),  # fondo blanco con borde
      axis.title = element_text(size = 8, family = "serif"),
      axis.text = element_text(size = 7, family = "serif"),
      axis.ticks = element_line(linewidth = 0.3),
      axis.line = element_line(color = "black", linewidth = 0.5),
      panel.grid.major = element_line(linewidth = 0.3, color = "#dddddd"),
      panel.grid.minor = element_line(linewidth = 0.2, color = "#eeeeee"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 9, family = "serif"),
      plot.margin = margin(5, 5, 5, 5)
    )

  return(p)
}


# ================================================================
# Residual diagnostics plots
# ================================================================



# 1. ACF de residuales
plot_resid_acf <- function(df, model, title = "Residual ACF") {
  resid <- get_residuals(model)
  if (length(resid) == 0) stop("[plot_resid_acf] ERROR: No residuals available for ACF.")

  acf_data <- stats::acf(resid, plot = FALSE)
  df_acf <- data.frame(lag = acf_data$lag, acf = acf_data$acf)

  ggplot2::ggplot(df_acf, ggplot2::aes(x = lag, y = acf)) +
    ggplot2::geom_bar(stat = "identity", fill = "black", width = 0.1) +
    ggplot2::labs(x = "Lag", y = "ACF", title = title) +
    ggplot2::theme_minimal(base_size = 8, base_family = "Times New Roman")
}

# 2. Durbin–Watson
plot_resid_dw <- function(df, model, title = "Durbin–Watson Test") {
  resid <- get_residuals(model)
  if (length(resid) < 2) {
    stop("[plot_resid_dw] ERROR: Not enough residuals for DW test.")
  }

  dw <- lmtest::dwtest(resid ~ 1)$statistic

  ggplot2::ggplot(data.frame(DW = dw), ggplot2::aes(x = 1, y = DW)) +
    ggplot2::geom_point(shape = 16, size = 2, color = "black") +
    ggplot2::geom_hline(yintercept = 2, linetype = "dashed", color = "red") +
    ggplot2::labs(x = "", y = "DW statistic", title = title) +
    ggplot2::theme_minimal(base_size = 8, base_family = "Times New Roman") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )
}

# 3. Histograma de residuales
plot_resid_hist <- function(df, model, title = "Residual Histogram") {
  resid <- get_residuals(model)
  ggplot2::ggplot(data.frame(resid = resid), ggplot2::aes(x = resid)) +
    ggplot2::geom_histogram(bins = 20, fill = "grey60", color = "black") +
    ggplot2::labs(x = "Residuals", y = "Frequency", title = title) +
    ggplot2::theme_minimal(base_size = 8, base_family = "Times New Roman")
}

# 4. QQ-plot de residuales
plot_resid_qq <- function(df, model, title = "Residual QQ-plot") {
  resid <- get_residuals(model)
  if (length(resid) == 0) {
    stop("[plot_resid_qq] ERROR: No residuals available for QQ-plot.")
  }

  qq <- qqnorm(resid, plot.it = FALSE)
  qq_df <- data.frame(theoretical = qq$x, sample = qq$y)

  ggplot2::ggplot(qq_df, ggplot2::aes(x = theoretical, y = sample)) +
    ggplot2::geom_point(shape = 16, size = 1.2, color = "black") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    ggplot2::labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = title) +
    ggplot2::theme_minimal(base_size = 8, base_family = "Times New Roman")
}



plot_model_comparison <- function(df, models, labels, title = "Model Comparison") {
  df_plot <- df
  preds <- list()
  
  for (i in seq_along(models)) {
    model <- models[[i]]
    label <- labels[[i]]
    df_plot[[label]] <- tryCatch(model$predict(df), error = function(e) NA)
  }
  
  df_long <- tidyr::pivot_longer(
    df_plot, cols = labels,
    names_to = "Model", values_to = "Prediction"
  )
  
  ggplot2::ggplot(df, ggplot2::aes(x = Year, y = Value)) +
    ggplot2::geom_point(shape = 20, size = 1.2, color = "black") +
    ggplot2::geom_line(data = df_long,
                       ggplot2::aes(x = Year, y = Prediction, color = Model),
                       linewidth = 0.6) +
    ggplot2::labs(x = "Year", y = "Articles", title = title) +
    ggplot2::theme_minimal(base_size = 8, base_family = "Times New Roman")
}
