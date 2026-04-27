# ============================================================================
# m3_render_country_regressions.R - Render Country Regression Results
# ============================================================================
# Creates plots for each country's regression analysis

#' Render country regression plots
#'
#' @param data Output from m3_compute_country_regressions
#' @param config Configuration list
#' @return List with plots for each country
#' @export
render_m3_country_regressions <- function(data, config = biblio_config()) {
  if (is.null(data) || data$status != "success") {
    return(list(plots = list(), status = "error: invalid data"))
  }
  
  plots <- list()
  
  if (!is.null(data$country_regressions)) {
    summary_plot <- create_regression_summary_plot(data, config)
    if (!is.null(summary_plot)) {
      plots$summary <- summary_plot
    }
    
    top_n <- config$top_n_country_plots %||% 20
    countries_with_data <- names(Filter(function(x) x$status == "success", 
                                         data$country_regressions))
    
    countries_by_volume <- vapply(
      data$country_regressions[countries_with_data],
      function(x) {
        value <- x$total_articles %||% NA_real_
        suppressWarnings(as.numeric(value)[1])
      },
      numeric(1)
    )
    countries_by_volume <- countries_by_volume[is.finite(countries_by_volume)]
    countries_by_volume <- sort(countries_by_volume, decreasing = TRUE)
    top_countries <- names(countries_by_volume)[1:min(top_n, length(countries_by_volume))]
    
    for (country in top_countries) {
      country_data <- data$country_regressions[[country]]
      country_plot <- create_country_regression_plot(country_data, country, config)
      if (!is.null(country_plot)) {
        safe_name <- gsub("[^a-zA-Z0-9]", "_", country)
        plots[[paste0("country_", safe_name)]] <- country_plot
      }
    }
    
    if (!is.null(data$hypotheses)) {
      hypo_plot <- create_hypothesis_plot(data$hypotheses, config)
      if (!is.null(hypo_plot)) {
        plots$hypotheses <- hypo_plot
      }
    }
    
    trend_plot <- create_trend_direction_plot(data$summary, config)
    if (!is.null(trend_plot)) {
      plots$trend_directions <- trend_plot
    }
    
    growth_dist_plot <- create_growth_distribution_plot(data, config)
    if (!is.null(growth_dist_plot)) {
      plots$growth_distribution <- growth_dist_plot
    }
  }
  
  list(
    plots = plots,
    status = "success"
  )
}

#' Create regression summary plot for all countries
#' @keywords internal
create_regression_summary_plot <- function(data, config) {
  successful <- Filter(function(x) x$status == "success", data$country_regressions)
  
  if (length(successful) == 0) return(NULL)
  
  summary_df <- do.call(rbind, lapply(names(successful), function(cntry) {
    x <- successful[[cntry]]
    data.frame(
      country = cntry,
      n_years = x$n_years,
      total_articles = x$total_articles,
      mean_annual = x$mean_annual,
      slope = x$slope %||% NA_real_,
      growth_rate = x$growth_rate %||% NA_real_,
      trend_direction = x$trend_direction %||% "unknown",
      best_model = x$best_model %||% NA_character_,
      stringsAsFactors = FALSE
    )
  }))
  
  summary_df <- summary_df[order(-summary_df$total_articles), ]
  summary_df$country <- factor(summary_df$country, levels = rev(summary_df$country))
  
  top_n <- min(25, nrow(summary_df))
  summary_df <- head(summary_df, top_n)
  
  colors <- c("increasing" = "#0072BD", "decreasing" = "#A2142F", "stable" = "#77AC30", "unknown" = "#7E2F8E")
  
  p <- ggplot2::ggplot(summary_df, ggplot2::aes(x = country, y = growth_rate, fill = trend_direction)) +
    ggplot2::geom_col(color = "black", linewidth = 0.2, width = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = colors, name = "Trend") +
    ggplot2::scale_y_continuous(
      name = "Annual Growth Rate (%)",
      expand = ggplot2::expansion(mult = c(0, 0.1))
    ) +
    ieee_theme_bar() +
    ggplot2::labs(
      title = "Country Production Growth Rates",
      subtitle = sprintf("Top %d countries by publication volume", top_n),
      x = NULL
    ) +
    ggplot2::theme(legend.position = "bottom")
  
  p
}

#' Create individual country regression plot
#' @keywords internal
create_country_regression_plot <- function(country_data, country_name, config) {
  if (is.null(country_data) || country_data$status != "success") return(NULL)
  if (is.null(country_data$fitted_models)) return(NULL)
  
  linear_fit <- country_data$fitted_models$linear
  if (is.null(linear_fit)) return(NULL)
  
  years <- linear_fit$fit$model$x
  articles <- linear_fit$fit$model$y
  
  pred <- linear_fit$predictions
  if (is.null(pred)) pred <- predict(linear_fit$fit)
  
  df <- data.frame(
    Year = years,
    Articles = articles,
    Predicted = pred
  )
  
  # Residuals
  df$Residual <- df$Articles - df$Predicted
  
  # Determine trend label
  trend_label <- country_data$trend_direction
  slope_val <- country_data$slope
  if (!is.na(slope_val)) {
    trend_label <- sprintf("%s (slope=%.2f)", trend_label, slope_val)
  }
  
  colors <- c("Observed" = "#0072BD", "Fitted" = "#D95319")
  
  # Main regression plot
  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = Year)) +
    ggplot2::geom_point(ggplot2::aes(y = Articles), color = "#0072BD", size = 2, alpha = 0.8) +
    ggplot2::geom_line(ggplot2::aes(y = Predicted), color = "#D95319", linewidth = 0.8, linetype = "dashed") +
    ggplot2::scale_x_continuous(name = "Year") +
    ggplot2::scale_y_continuous(
      name = "Articles",
      expand = ggplot2::expansion(mult = c(0, 0.1)),
      labels = scales::label_number(big.mark = ",")
    ) +
    ieee_theme_timeseries() +
    ggplot2::labs(
      title = sprintf("%s: Annual Production Trend", country_name),
      subtitle = sprintf("%s | R² = %.3f", trend_label, linear_fit$R2)
    )
  
  # Add p-value if available
  if (!is.na(country_data$slope_pvalue)) {
    sig_marker <- if (country_data$slope_pvalue < 0.001) "***" 
                  else if (country_data$slope_pvalue < 0.01) "**"
                  else if (country_data$slope_pvalue < 0.05) "*"
                  else ""
    p1 <- p1 + ggplot2::annotate(
      "text", x = Inf, y = -Inf, hjust = 1.1, vjust = -0.5,
      label = sprintf("p = %.4f %s", country_data$slope_pvalue, sig_marker),
      size = 2.5, fontface = "italic"
    )
  }
  
  # Harmonic analysis plot (if available)
  p2 <- NULL
  if (!is.null(country_data$harmonics) && !is.na(country_data$harmonics$dominant_period)) {
    harmonic_data <- country_data$harmonics
    
    # Create spectrum plot
    if (!is.null(harmonic_data$spectrum)) {
      spec <- harmonic_data$spectrum
      spec_df <- data.frame(
        Period = spec$period[spec$frequency > 0 & spec$period < 50],
        Power = spec$power[spec$frequency > 0 & spec$period < 50]
      )
      
      p2 <- ggplot2::ggplot(spec_df, ggplot2::aes(x = Period, y = Power)) +
        ggplot2::geom_line(color = "#0072BD", linewidth = 0.6) +
        ggplot2::geom_vline(xintercept = harmonic_data$dominant_period, 
                           color = "#D95319", linetype = "dashed", linewidth = 0.4) +
        ggplot2::scale_x_log10(name = "Period (years, log scale)") +
        ggplot2::scale_y_continuous(name = "Power") +
        ieee_theme() +
        ggplot2::labs(
          title = "Data Harmonic Spectrum",
          subtitle = sprintf("Dominant: %.1f years (%.0f%% var)", 
                            harmonic_data$dominant_period, 
                            harmonic_data$variance_explained * 100)
        )
    }
  }
  
  # Residual harmonic plot (if available)
  p3 <- NULL
  if (!is.null(country_data$residual_harmonics) && 
      !is.null(country_data$residual_harmonics$has_periodicity)) {
    res_harm <- country_data$residual_harmonics
    
    if (!is.na(res_harm$dominant_period) && res_harm$has_periodicity) {
      # Create warning annotation
      p3 <- ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, hjust = 0.5, vjust = 0.5,
                         label = sprintf("Warning: Residual periodicity\ndetected: %.1f year cycle\n(%.0f%% variance)",
                                        res_harm$dominant_period,
                                        res_harm$variance_explained * 100),
                         size = 3, fontface = "bold", color = "#A2142F") +
        ggplot2::theme_void() +
        ggplot2::labs(title = "Residual Analysis")
    }
  }
  
  # Return list of plots
  plots <- list(
    regression = p1
  )
  
  if (!is.null(p2)) plots$harmonics <- p2
  if (!is.null(p3)) plots$residual_harmonics <- p3
  
  # Return main plot for compatibility
  p1
}

#' Create hypothesis results plot
#' @keywords internal
create_hypothesis_plot <- function(hypotheses, config) {
  if (is.null(hypotheses) || length(hypotheses$hyphypotheses) == 0) return(NULL)
  
  hyp_list <- hypotheses$hyphypotheses
  if (is.null(hyp_list)) hyp_list <- hypotheses
  
  df <- do.call(rbind, lapply(names(hyp_list), function(h) {
    data.frame(
      hypothesis = gsub("H03_", "H", h),
      description = substr(hyp_list[[h]]$hyphypothesis %||% hyp_list[[h]]$hyphothesis %||% h, 1, 50),
      result = hyp_list[[h]]$result,
      stringsAsFactors = FALSE
    )
  }))
  
  df$result_color <- ifelse(df$result == "reject", "#A2142F", "#77AC30")
  df$result_label <- ifelse(df$result == "reject", "Rejected", "Not Rejected")
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = hypothesis, y = 1, fill = result)) +
    ggplot2::geom_col(width = 0.8, color = "black", linewidth = 0.2) +
    ggplot2::geom_text(ggplot2::aes(label = result_label), vjust = 2, size = 2.5) +
    ggplot2::scale_fill_manual(
      values = c("reject" = "#A2142F", "fail_to_reject" = "#77AC30"),
      name = "Result"
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.3))) +
    ieee_theme_bar() +
    ggplot2::labs(
      title = "Country Production Hypotheses",
      subtitle = sprintf("%d hypotheses tested", nrow(df)),
      x = "Hypothesis",
      y = NULL
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      legend.position = "bottom"
    )
  
  p
}

#' Create trend direction plot
#' @keywords internal
create_trend_direction_plot <- function(summary_stats, config) {
  if (is.null(summary_stats)) return(NULL)
  
  df <- data.frame(
    direction = c("Increasing", "Stable", "Decreasing"),
    count = c(
      summary_stats$n_increasing %||% 0,
      summary_stats$n_stable %||% 0,
      summary_stats$n_decreasing %||% 0
    ),
    stringsAsFactors = FALSE
  )
  
  df$direction <- factor(df$direction, levels = c("Increasing", "Stable", "Decreasing"))
  
  colors <- c("Increasing" = "#0072BD", "Stable" = "#77AC30", "Decreasing" = "#A2142F")
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = direction, y = count, fill = direction)) +
    ggplot2::geom_col(color = "black", linewidth = 0.2, width = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = count), vjust = -0.3, size = 3) +
    ggplot2::scale_fill_manual(values = colors, guide = "none") +
    ggplot2::scale_y_continuous(
      name = "Number of Countries",
      expand = ggplot2::expansion(mult = c(0, 0.15))
    ) +
    ieee_theme_bar() +
    ggplot2::labs(
      title = "Production Trend Distribution",
      subtitle = sprintf("Total countries analyzed: %d", summary_stats$n_successful),
      x = "Trend Direction"
    )
  
  p
}

#' Create growth distribution plot
#' @keywords internal
create_growth_distribution_plot <- function(data, config) {
  successful <- Filter(function(x) x$status == "success", data$country_regressions)
  
  if (length(successful) < 4) return(NULL)
  
  slopes <- sapply(successful, function(x) x$slope)
  slopes <- slopes[!is.na(slopes)]
  
  if (length(slopes) < 4) return(NULL)
  
  density_data <- density(slopes)
  
  p <- ggplot2::ggplot(data.frame(slope = slopes), ggplot2::aes(x = slope)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                           bins = 20, fill = "#0072BD", color = "black", 
                           linewidth = 0.2, alpha = 0.7) +
    ggplot2::geom_density(color = "#D95319", linewidth = 0.8) +
    ggplot2::geom_vline(xintercept = 0, color = "#A2142F", linetype = "dashed", linewidth = 0.5) +
    ggplot2::scale_x_continuous(name = "Annual Slope (Articles/Year)") +
    ggplot2::scale_y_continuous(name = "Density") +
    ieee_theme() +
    ggplot2::labs(
      title = "Distribution of Country Growth Rates",
      subtitle = sprintf("Mean = %.2f, Median = %.2f, SD = %.2f",
                        mean(slopes), median(slopes), sd(slopes))
    )
  
  p
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
