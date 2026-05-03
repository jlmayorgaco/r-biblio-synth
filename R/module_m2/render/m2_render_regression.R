# ============================================================================
# m2_render_regression.R - Publication-grade regression plotting for M2
# ============================================================================

#' Render all regression plots
#'
#' @param result Regression data from compute_m2_regression
#' @param config Configuration list
#' @return List of plot objects
#' @export
render_m2_regression <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"comparison_table" %in% names(result)) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  if ((result$status %||% "error") != "success" || is.null(result$models) || length(result$models) == 0) {
    return(list(status = "error", plots = list(), tables = list()))
  }

  model_data <- tryCatch(extract_model_data(result), error = function(e) {
    list(raw = data.frame(), fitted = data.frame(), all_fitted = data.frame())
  })

  plots <- list(
    best_model = tryCatch(render_best_model_plot(result, model_data, config), error = function(e) NULL),
    model_comparison = tryCatch(render_model_comparison_plot(result, config), error = function(e) NULL),
    all_models = tryCatch(render_all_models_plot(result, model_data, config), error = function(e) NULL),
    residuals = tryCatch(render_residual_diagnostics(result, model_data, config), error = function(e) NULL),
    equation = tryCatch(render_equation_plot(result, config), error = function(e) NULL)
  )

  list(status = "success", plots = plots, tables = list())
}

#' Render the main regression figure
#' @keywords internal
render_best_model_plot <- function(data, model_data, config) {
  if (is.null(data$best_model) || identical(data$best_model$name, "none") || nrow(model_data$raw) == 0) {
    return(NULL)
  }

  raw_df <- model_data$raw
  fitted_df <- model_data$fitted
  best_name <- data$best_model$name
  params <- data$best_model$parameter_summary %||% list()

  capacity <- m2_scalar_num(params$carrying_capacity)
  inflection <- m2_scalar_num(params$inflection_year)
  growth_rate <- m2_scalar_num(params$growth_rate)
  transition_year <- m2_compute_transition_year(best_name, params)

  x_range <- range(raw_df$Year, na.rm = TRUE)
  y_range <- range(c(raw_df$Articles, fitted_df$Fitted, fitted_df$Upper, fitted_df$Lower), na.rm = TRUE)
  x_span <- diff(x_range)
  y_span <- diff(y_range)
  if (!is.finite(y_span) || y_span <= 0) {
    y_span <- max(abs(y_range), 1)
  }

  label_x <- x_range[1] + 0.04 * x_span
  label_y <- y_range[1] + 0.14 * y_span
  top_y <- y_range[2] - 0.04 * y_span

  label_lines <- c(
    best_name,
    sprintf("Adj. R2 = %.3f", m2_scalar_num(data$best_model$Adj_R2)),
    sprintf("RMSE = %.2f", m2_scalar_num(data$best_model$RMSE)),
    m2_compact_parameter_line(best_name, params)
  )
  label_lines <- label_lines[nzchar(label_lines)]

  subtitle_parts <- c(
    sprintf("Benchmark: %s", data$best_model$benchmark_name %||% best_name),
    if (is.finite(growth_rate)) sprintf("growth rate = %.4f", growth_rate) else NULL,
    if (is.finite(capacity)) sprintf("capacity = %.1f", capacity) else NULL
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = fitted_df,
      ggplot2::aes(x = Year, ymin = Lower, ymax = Upper),
      fill = "#BDBDBD",
      alpha = 0.18
    ) +
    ggplot2::geom_line(
      data = fitted_df,
      ggplot2::aes(x = Year, y = Fitted),
      color = "black",
      linewidth = 1.05,
      lineend = "round"
    ) +
    ggplot2::geom_point(
      data = raw_df,
      ggplot2::aes(x = Year, y = Articles),
      color = "black",
      fill = "black",
      shape = 16,
      size = 1.95,
      alpha = 0.95
    ) +
    ggplot2::scale_x_continuous(
      name = "Year",
      breaks = scales::breaks_pretty(n = 8),
      expand = ggplot2::expansion(mult = c(0.01, 0.03))
    ) +
    ggplot2::scale_y_continuous(
      name = "Number of publications",
      labels = scales::label_number(big.mark = ","),
      expand = ggplot2::expansion(mult = c(0.03, 0.08))
    ) +
    ggplot2::annotate(
      "label",
      x = label_x,
      y = label_y,
      label = paste(label_lines, collapse = "\n"),
      hjust = 0,
      vjust = 0,
      size = 3.2,
      family = "mono",
      fill = scales::alpha("white", 0.92),
      color = "black",
      linewidth = 0.2,
      label.padding = grid::unit(0.14, "lines")
    ) +
    ggplot2::labs(
      title = "Annual scientific production: selected growth model",
      subtitle = paste(subtitle_parts, collapse = " | "),
      caption = "Interpretable headline model with fitted uncertainty band."
    ) +
    ieee_theme_wide(base_size = 8.5) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
      legend.position = "none"
    )

  if (is.finite(capacity) && capacity > (y_range[1] + 0.6 * y_span)) {
    p <- p +
      ggplot2::geom_hline(yintercept = capacity, color = "#59A14F", linetype = "22", linewidth = 0.7) +
      ggplot2::annotate(
        "text",
        x = x_range[1] + 0.04 * x_span,
        y = capacity + 0.02 * y_span,
        label = sprintf("K = %.1f", capacity),
        hjust = 0,
        color = "#59A14F",
        size = 3.3,
        fontface = "bold"
      )
  }

  if (is.finite(inflection)) {
    p <- p +
      ggplot2::geom_vline(xintercept = inflection, color = "#4E79A7", linetype = "42", linewidth = 0.85) +
      ggplot2::annotate(
        "text",
        x = inflection,
        y = top_y,
        label = "t0",
        color = "#4E79A7",
        vjust = 1,
        hjust = -0.1,
        size = 3.5,
        fontface = "bold"
      )
  }

  if (is.finite(transition_year)) {
    p <- p +
      ggplot2::geom_vline(xintercept = transition_year, color = "#E15759", linetype = "42", linewidth = 0.85) +
      ggplot2::annotate(
        "text",
        x = transition_year,
        y = top_y,
        label = "t95",
        color = "#E15759",
        vjust = 1,
        hjust = -0.1,
        size = 3.5,
        fontface = "bold"
      )
  }

  ieee_mark_plot_layout(p, "full")
}

#' Render model comparison charts
#' @keywords internal
render_model_comparison_plot <- function(data, config) {
  comparison <- data$comparison_table
  if (is.null(comparison) || nrow(comparison) == 0) return(NULL)

  plot_data <- comparison[order(comparison$Rank, comparison$RMSE), , drop = FALSE]
  plot_data <- head(plot_data, min(10, nrow(plot_data)))
  plot_data$Model <- factor(plot_data$Model, levels = rev(plot_data$Model))
  plot_data$Highlight <- ifelse(
    plot_data$Model == data$best_model$name,
    "Selected",
    ifelse(plot_data$Model == data$benchmark_best_model$name, "Benchmark", "Other")
  )

  make_bar <- function(metric, title, x_label) {
    plot_df <- plot_data
    plot_df$value <- suppressWarnings(as.numeric(plot_df[[metric]]))

    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = Model, y = value, fill = Highlight)) +
      ggplot2::geom_col(width = 0.72, color = "white", linewidth = 0.25) +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(
        values = c("Selected" = "#4E79A7", "Benchmark" = "#E15759", "Other" = "#C9C9C9"),
        guide = "none"
      ) +
      ggplot2::labs(title = title, x = NULL, y = x_label) +
      ieee_theme_wide(base_size = 8.2)

    ieee_mark_plot_layout(p, "full")
  }

  list(
    score = make_bar("CompositeScore", "Model ranking by composite score", "Composite score"),
    adj_r2 = make_bar("Adj_R2", "Explained variance by model", "Adjusted R2"),
    rmse = make_bar("RMSE", "Prediction error by model", "RMSE")
  )
}

#' Render overlay of strongest candidate models
#' @keywords internal
render_all_models_plot <- function(data, model_data, config) {
  if (is.null(data$models) || length(data$models) == 0 || nrow(model_data$raw) == 0) return(NULL)

  selected_models <- m2_pick_models_for_overlay(data, max_models = 6L)
  selected_models <- selected_models[selected_models %in% names(data$models)]
  if (length(selected_models) == 0) return(NULL)

  curve_rows <- lapply(selected_models, function(model_name) {
    model <- data$models[[model_name]]
    curve_df <- if (is.list(model) && !is.null(model$curve$dense) && is.data.frame(model$curve$dense)) {
      model$curve$dense
    } else if (is.list(model) && !is.null(model$curve$observed) && is.data.frame(model$curve$observed)) {
      model$curve$observed
    } else {
      NULL
    }
    if (is.null(curve_df) || nrow(curve_df) == 0) {
      return(NULL)
    }
    data.frame(Year = curve_df$Year, Articles = curve_df$Fitted, Model = model_name, stringsAsFactors = FALSE)
  })
  curve_rows <- Filter(Negate(is.null), curve_rows)
  if (length(curve_rows) == 0) return(NULL)

  all_fitted <- do.call(rbind, curve_rows)
  all_fitted$Model <- factor(all_fitted$Model, levels = selected_models)

  palette <- setNames(get_ieee_palette(length(selected_models)), selected_models)
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = model_data$raw,
      ggplot2::aes(x = Year, y = Articles),
      color = "black",
      size = 1.7,
      alpha = 0.9
    ) +
    ggplot2::geom_line(
      data = all_fitted,
      ggplot2::aes(x = Year, y = Articles, color = Model, linetype = Model),
      linewidth = 0.8,
      alpha = 0.95
    ) +
    ggplot2::scale_color_manual(values = palette) +
    ggplot2::scale_linetype_manual(values = rep(c("solid", "22", "42", "F2", "dotdash", "dotted"), length.out = length(selected_models))) +
    ggplot2::labs(
      title = "Selected model versus strongest alternatives",
      subtitle = "Interpretable winner, flexible benchmark, and top family representatives",
      x = "Year",
      y = "Number of publications"
    ) +
    ieee_theme_wide(base_size = 8.2) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
      legend.position = "bottom"
    )

  ieee_mark_plot_layout(p, "full")
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
    ggplot2::geom_hline(yintercept = 0, linetype = "22", color = "#666666", linewidth = 0.45) +
    ggplot2::geom_point(color = "black", fill = "white", shape = 21, size = 1.8, stroke = 0.45) +
    ggplot2::geom_smooth(method = "loess", se = FALSE, color = "#4E79A7", linewidth = 0.65, span = 0.8) +
    ggplot2::labs(
      title = "Residuals versus fitted values",
      subtitle = sprintf("Shapiro p = %.3f | Durbin-Watson = %.3f",
                         m2_scalar_num(data$performance$shapiro_p),
                         m2_scalar_num(data$performance$dw_statistic)),
      x = "Fitted values",
      y = "Standardized residuals"
    ) +
    ieee_theme(base_size = 8)
  p1 <- ieee_mark_plot_layout(p1, "single")

  p2 <- ggplot2::ggplot(res_df, ggplot2::aes(sample = Std_Resid)) +
    ggplot2::geom_qq(color = "black", size = 1.7, alpha = 0.85) +
    ggplot2::geom_qq_line(color = "black", linewidth = 0.7) +
    ggplot2::labs(title = "Normal Q-Q plot", x = "Theoretical quantiles", y = "Sample quantiles") +
    ieee_theme(base_size = 8)
  p2 <- ieee_mark_plot_layout(p2, "single")

  p3 <- ggplot2::ggplot(res_df, ggplot2::aes(x = Residual)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(density)),
      bins = 15,
      fill = "#D9D9D9",
      color = "black",
      linewidth = 0.25,
      alpha = 0.9
    ) +
    ggplot2::geom_density(color = "black", linewidth = 0.8) +
    ggplot2::labs(title = "Residual distribution", x = "Residuals", y = "Density") +
    ieee_theme(base_size = 8)
  p3 <- ieee_mark_plot_layout(p3, "single")

  p4 <- NULL
  if (!is.null(data$performance$acf) && length(data$performance$acf) > 0) {
    acf_df <- data.frame(Lag = seq_along(data$performance$acf), ACF = as.numeric(data$performance$acf))
    ci <- qnorm(0.975) / sqrt(max(1, nrow(res_df)))
    ymax <- max(0.3, max(abs(c(acf_df$ACF, ci)), na.rm = TRUE) * 1.15)
    p4 <- ggplot2::ggplot(acf_df, ggplot2::aes(x = Lag, y = ACF)) +
      ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.45) +
      ggplot2::geom_hline(yintercept = c(-ci, ci), linetype = "22", color = "#3B5BDC", linewidth = 0.55) +
      ggplot2::geom_segment(ggplot2::aes(xend = Lag, yend = 0), color = "black", linewidth = 0.8) +
      ggplot2::geom_point(color = "black", size = 1.5) +
      ggplot2::scale_x_continuous(name = "Lag", breaks = seq_len(max(acf_df$Lag))) +
      ggplot2::scale_y_continuous(name = "Autocorrelation", limits = c(-ymax, ymax)) +
      ggplot2::labs(title = "Residual autocorrelation", subtitle = "Dashed lines show 95% confidence bounds") +
      ieee_theme(base_size = 8)
    p4 <- ieee_mark_plot_layout(p4, "single")
  }

  list(residuals_vs_fitted = p1, qq_plot = p2, histogram = p3, acf = p4)
}

#' Render regression summary page
#' @keywords internal
render_equation_plot <- function(data, config) {
  comparison <- data$comparison_table
  if (is.null(comparison) || nrow(comparison) == 0) return(NULL)

  top_rows <- head(comparison[order(comparison$Rank, comparison$RMSE), c("Model", "Family", "Adj_R2", "RMSE", "Equation")], 6)
  lines <- c(
    sprintf("Selected model   : %s", data$best_model$name %||% "NA"),
    sprintf("Flexible benchmark: %s", data$benchmark_best_model$name %||% "NA"),
    "",
    "Top candidates:"
  )
  lines <- c(lines, vapply(seq_len(nrow(top_rows)), function(i) {
    sprintf(
      "%s | %s | AdjR2=%.3f | RMSE=%.2f | %s",
      top_rows$Model[i],
      top_rows$Family[i],
      suppressWarnings(as.numeric(top_rows$Adj_R2[i])),
      suppressWarnings(as.numeric(top_rows$RMSE[i])),
      top_rows$Equation[i]
    )
  }, character(1)))

  p <- ggplot2::ggplot() +
    ggplot2::annotate(
      "text",
      x = 0,
      y = 1,
      label = paste(lines, collapse = "\n"),
      hjust = 0,
      vjust = 1,
      family = "mono",
      size = 3.1
    ) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
    ggplot2::theme_void() +
    ggplot2::labs(
      title = "Regression model summary",
      subtitle = "Interpretable headline model and strongest competing candidates"
    )

  ieee_mark_plot_layout(p, "single")
}

#' Extract model data for plotting
#' @keywords internal
extract_model_data <- function(data) {
  if (is.null(data$models) || length(data$models) == 0) {
    return(list(raw = data.frame(), fitted = data.frame(), all_fitted = data.frame()))
  }

  raw_df <- if (!is.null(data$data) && is.data.frame(data$data)) {
    data$data
  } else {
    first_model <- data$models[[1]]
    if (is.list(first_model) && !is.null(first_model$years) && !is.null(first_model$actual)) {
      data.frame(Year = first_model$years, Articles = first_model$actual)
    } else {
      data.frame(Year = numeric(), Articles = numeric())
    }
  }

  if (nrow(raw_df) == 0 || !all(c("Year", "Articles") %in% names(raw_df))) {
    return(list(raw = data.frame(), fitted = data.frame(), all_fitted = data.frame()))
  }

  best_name <- if (!is.null(data$best_model$name)) data$best_model$name else names(data$models)[1]
  best_model <- data$models[[best_name]]
  if (is.null(best_model)) {
    best_model <- data$models[[1]]
  }

  fitted_df <- if (is.list(best_model) && !is.null(best_model$curve$dense) && is.data.frame(best_model$curve$dense)) {
    best_model$curve$dense
  } else if (is.list(best_model) && !is.null(best_model$curve$observed) && is.data.frame(best_model$curve$observed)) {
    best_model$curve$observed
  } else {
    data.frame(
      Year = raw_df$Year,
      Fitted = if (!is.null(best_model$fitted)) best_model$fitted else rep(NA_real_, nrow(raw_df)),
      Lower = if (!is.null(best_model$fitted)) best_model$fitted else rep(NA_real_, nrow(raw_df)),
      Upper = if (!is.null(best_model$fitted)) best_model$fitted else rep(NA_real_, nrow(raw_df))
    )
  }

  list(raw = raw_df, fitted = fitted_df, all_fitted = data.frame())
}

#' Parameter labels for figure subtitles
#' @keywords internal
m2_render_parameter_lines <- function(parameter_summary) {
  if (!is.list(parameter_summary) || length(parameter_summary) == 0) {
    return(character())
  }

  lines <- character()
  capacity <- m2_scalar_num(parameter_summary$carrying_capacity)
  rate <- m2_scalar_num(parameter_summary$growth_rate)
  inflection <- m2_scalar_num(parameter_summary$inflection_year)
  shape <- m2_scalar_num(parameter_summary$shape)

  if (is.finite(capacity)) lines <- c(lines, sprintf("capacity = %.2f", capacity))
  if (is.finite(rate)) lines <- c(lines, sprintf("rate = %.4f", rate))
  if (is.finite(inflection)) lines <- c(lines, sprintf("inflection = %.2f", inflection))
  if (is.finite(shape)) lines <- c(lines, sprintf("shape = %.3f", shape))
  lines
}

#' Select a readable subset of models for overlays
#' @keywords internal
m2_pick_models_for_overlay <- function(data, max_models = 8L) {
  comparison <- data$comparison_table
  if (is.null(comparison) || !is.data.frame(comparison) || nrow(comparison) == 0) {
    return(character())
  }

  ordered <- comparison[order(comparison$Rank, comparison$RMSE), , drop = FALSE]
  chosen <- c(data$best_model$name %||% NULL, data$benchmark_best_model$name %||% NULL)
  family_rows <- ordered[!duplicated(ordered$Family), , drop = FALSE]
  chosen <- unique(c(chosen, as.character(family_rows$Model), as.character(head(ordered$Model, max_models))))
  chosen[seq_len(min(length(chosen), max_models))]
}

#' Get model equation string
#' @keywords internal
get_model_equation <- function(model_name) {
  switch(tolower(model_name),
    "linear" = "y = a + b*x",
    "polynomial" = "y = a + b1*x + b2*x^2 + b3*x^3",
    "quadratic" = "y = a + b*x + c*x^2",
    "exponential" = "y = N0*exp(r*x)",
    "logarithmic" = "y = a + b*ln(x)",
    "logistic" = "y = K / (1 + exp(-r*(x - t0)))",
    "gompertz" = "y = K*exp(-exp(-r*(x - t0)))",
    "weibull" = "y = K*(1 - exp(-((x - t0)/alpha)^r))",
    "vonbertalanffy" = "y = Linf*(1 - exp(-k*(x - t0)))",
    "normal" = "y = A*exp(-((x - mu)^2)/(2*sigma^2))",
    "spline" = "Flexible spline benchmark",
    "richards" = "y = K / (1 + v*exp(-r*(x - t0)))^(1/v)",
    "fourier" = "y = a0 + a1*cos(w*x) + b1*sin(w*x)",
    model_name
  )
}

#' Compact parameter string for the in-plot annotation
#' @keywords internal
m2_compact_parameter_line <- function(model_name, parameter_summary) {
  if (!is.list(parameter_summary) || length(parameter_summary) == 0) {
    return("")
  }

  capacity <- m2_scalar_num(parameter_summary$carrying_capacity)
  rate <- m2_scalar_num(parameter_summary$growth_rate)
  inflection <- m2_scalar_num(parameter_summary$inflection_year)
  shape <- m2_scalar_num(parameter_summary$shape)

  bits <- character()
  if (is.finite(capacity)) bits <- c(bits, sprintf("K=%.1f", capacity))
  if (is.finite(rate)) bits <- c(bits, sprintf("r=%.3f", rate))
  if (is.finite(inflection)) bits <- c(bits, sprintf("t0=%.1f", inflection))
  if (is.finite(shape) && tolower(model_name) %in% c("weibull", "richards", "mmf", "hill")) {
    bits <- c(bits, sprintf("shape=%.2f", shape))
  }

  paste(bits, collapse = " | ")
}

#' Approximate the year at which the model reaches 95% of capacity
#' @keywords internal
m2_compute_transition_year <- function(model_name, parameter_summary) {
  t0 <- m2_scalar_num(parameter_summary$inflection_year)
  r <- m2_scalar_num(parameter_summary$growth_rate)
  shape <- m2_scalar_num(parameter_summary$shape)

  if (!is.finite(t0) || !is.finite(r) || abs(r) <= .Machine$double.eps) {
    return(NA_real_)
  }

  switch(tolower(model_name),
    logistic = t0 + log(19) / r,
    logistic4p = t0 + log(19) / r,
    gompertz = t0 - log(-log(0.95)) / r,
    gompertzoffset = t0 - log(-log(0.95)) / r,
    richards = {
      if (!is.finite(shape) || abs(shape) <= .Machine$double.eps) return(NA_real_)
      t0 - log((0.95^(-shape) - 1) / shape) / r
    },
    NA_real_
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
