# ============================================================================
# m2_render_eda.R - Enhanced EDA Plots (IEEE Q1 Legacy Compatible)
# ============================================================================
# Creates legacy-style plots:
# - SinglePlot_Avg_1_Years.png
# - SinglePlot_Avg_5_Years.png  
# - SinglePlot_Avg_10_Years.png
# - Multiplot_Moving_Averages.png
# ============================================================================

#' Render all M2 EDA plots (Enhanced IEEE Q1 Quality)
#'
#' @param result EDA data from compute_m2_eda
#' @param config Configuration list
#' @return List of plot objects
#' @export
render_m2_eda <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"moving_averages" %in% names(result)) {
    return(list(status = "stub", plots = list(), tables = list()))
  }
  
  plots <- list()
  ma_data <- result$moving_averages
  
  if (length(ma_data) == 0) {
    return(list(status = "no data", plots = list()))
  }
  
  # Extract raw data for overlay
  raw_data <- result$raw_data
  if (is.null(raw_data) && "annual_production" %in% names(result)) {
    raw_data <- result$annual_production
  }
  
  # Define window sizes and colors
  window_info <- list(
    ma_1 = list(name = "MA-1", color = "#0072BD", label = "1-Year Moving Average"),
    ma_3 = list(name = "MA-3", color = "#D95319", label = "3-Year Moving Average"),
    ma_5 = list(name = "MA-5", color = "#EDB120", label = "5-Year Moving Average"),
    ma_10 = list(name = "MA-10", color = "#7E2F8E", label = "10-Year Moving Average")
  )
  
  # Create individual moving average plots (legacy style)
  for (window_name in names(window_info)) {
    if (window_name %in% names(ma_data)) {
      ma <- ma_data[[window_name]]
      info <- window_info[[window_name]]
      
      if (!is.null(ma) && nrow(ma) > 0) {
        p <- create_single_ma_plot(
          ma, 
          raw_data,
          window_size = as.numeric(gsub("ma_", "", window_name)),
          color = info$color,
          title = info$label,
          config = config
        )
        plots[[paste0("singleplot_avg_", gsub("ma_", "", window_name), "_years")]] <- p
      }
    }
  }
  
  # Create multiplot combining all moving averages (legacy style)
  plots$multiplot_moving_averages <- create_multi_ma_plot(ma_data, raw_data, window_info, config)
  
  # Create main M2 overview plot
  plots$m2_p1 <- create_m2_p1_plot(result, config)
  
  # Annual production with trend
  if (!is.null(raw_data) && nrow(raw_data) > 0) {
    plots$annual_production <- create_annual_production_plot(raw_data, config)
  }
  
  # Descriptive statistics plot
  if ("summary_stats" %in% names(result)) {
    plots$descriptive_stats <- create_descriptive_stats_plot(result, config)
  }
  
  list(status = "success", plots = plots, tables = list())
}

#' Create single moving average plot (IEEE style)
#' @keywords internal
create_single_ma_plot <- function(ma_data, raw_data, window_size, color, title, config) {
  p <- ggplot2::ggplot()
  
  # Add raw data as points
  if (!is.null(raw_data) && nrow(raw_data) > 0) {
    if ("Year" %in% names(raw_data) && "Articles" %in% names(raw_data)) {
      p <- p + ggplot2::geom_point(
        data = raw_data,
        ggplot2::aes(x = Year, y = Articles),
        color = "#666666",
        size = 1.5,
        alpha = 0.5
      )
    }
  }
  
  # Add moving average line
  if (!is.null(ma_data) && nrow(ma_data) > 0) {
    year_col <- if ("year" %in% names(ma_data)) "year" else "Year"
    articles_col <- if ("articles" %in% names(ma_data)) "articles" else "Articles"
    
    p <- p + ggplot2::geom_line(
      data = ma_data,
      ggplot2::aes_string(x = year_col, y = articles_col),
      color = color,
      linewidth = 1
    )
    
    # Add confidence band if available
    if ("lower" %in% names(ma_data) && "upper" %in% names(ma_data)) {
      p <- p + ggplot2::geom_ribbon(
        data = ma_data,
        ggplot2::aes_string(x = year_col, ymin = "lower", ymax = "upper"),
        fill = color,
        alpha = 0.2
      )
    }
  }
  
  p <- p +
    ggplot2::scale_x_continuous(
      name = "Year",
      breaks = scales::breaks_pretty(n = 6),
      labels = scales::label_number(accuracy = 1)
    ) +
    ggplot2::scale_y_continuous(
      name = "Number of Publications",
      labels = scales::label_number(big.mark = ",")
    ) +
    ieee_theme_timeseries() +
    ggplot2::labs(
      title = title,
      subtitle = sprintf("%d-year centered moving average of annual publications", window_size)
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 10, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5, face = "italic")
    )
  
  p
}

#' Create multiplot with all moving averages (IEEE style)
#' @keywords internal
create_multi_ma_plot <- function(ma_data, raw_data, window_info, config) {
  # Combine all MA data
  combined_list <- list()
  
  for (window_name in names(window_info)) {
    if (window_name %in% names(ma_data)) {
      ma <- ma_data[[window_name]]
      if (!is.null(ma) && nrow(ma) > 0) {
        year_col <- if ("year" %in% names(ma)) "year" else "Year"
        articles_col <- if ("articles" %in% names(ma)) "articles" else "Articles"
        
        combined_list[[window_name]] <- data.frame(
          Year = ma[[year_col]],
          Articles = ma[[articles_col]],
          Window = window_info[[window_name]]$name
        )
      }
    }
  }
  
  if (length(combined_list) == 0) return(NULL)
  
  combined <- do.call(rbind, combined_list)
  combined$Window <- factor(combined$Window, levels = sapply(window_info, function(x) x$name))
  
  colors <- sapply(window_info, function(x) x$color)
  names(colors) <- sapply(window_info, function(x) x$name)
  
  p <- ggplot2::ggplot()
  
  # Add raw data
  if (!is.null(raw_data) && nrow(raw_data) > 0) {
    if ("Year" %in% names(raw_data) && "Articles" %in% names(raw_data)) {
      p <- p + ggplot2::geom_point(
        data = raw_data,
        ggplot2::aes(x = Year, y = Articles),
        color = "#888888",
        size = 1,
        alpha = 0.4
      )
    }
  }
  
  # Add MA lines
  p <- p + ggplot2::geom_line(
    data = combined,
    ggplot2::aes(x = Year, y = Articles, color = Window),
    linewidth = 0.8
  ) +
    ggplot2::scale_color_manual(
      name = "Moving Average",
      values = colors,
      breaks = sapply(window_info, function(x) x$name)
    ) +
    ggplot2::scale_x_continuous(
      name = "Year",
      breaks = scales::breaks_pretty(n = 8)
    ) +
    ggplot2::scale_y_continuous(
      name = "Number of Publications",
      labels = scales::label_number(big.mark = ",")
    ) +
    ieee_theme_timeseries() +
    ggplot2::labs(
      title = "Annual Publication Trend with Moving Averages",
      subtitle = "Publication volume smoothed by different window sizes"
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      plot.title = ggplot2::element_text(size = 10, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5, face = "italic")
    )
  
  p
}

#' Create main M2 plot (overview)
#' @keywords internal
create_m2_p1_plot <- function(result, config) {
  if (!"annual_production" %in% names(result)) return(NULL)
  
  annual <- result$annual_production
  if (is.null(annual) || nrow(annual) == 0) return(NULL)
  
  year_col <- if ("year" %in% names(annual)) "year" else "Year"
  articles_col <- if ("articles" %in% names(annual)) "articles" else "Articles"
  
  p <- ggplot2::ggplot(annual, ggplot2::aes_string(x = year_col, y = articles_col)) +
    ggplot2::geom_col(fill = "#0072BD", color = "black", linewidth = 0.2, alpha = 0.8) +
    ggplot2::geom_smooth(
      method = "loess",
      se = TRUE,
      color = "#D95319",
      fill = "#D95319",
      alpha = 0.2,
      linewidth = 0.8
    ) +
    ggplot2::scale_x_continuous(
      name = "Year",
      breaks = scales::breaks_pretty(n = 8)
    ) +
    ggplot2::scale_y_continuous(
      name = "Number of Publications",
      labels = scales::label_number(big.mark = ","),
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ieee_theme_bar() +
    ggplot2::labs(
      title = "Annual Scientific Production",
      subtitle = "Total publications per year with trend line"
    )
  
  p
}

#' Create annual production plot
#' @keywords internal
create_annual_production_plot <- function(raw_data, config) {
  if (is.null(raw_data) || nrow(raw_data) == 0) return(NULL)
  
  year_col <- if ("Year" %in% names(raw_data)) "Year" else "year"
  articles_col <- if ("Articles" %in% names(raw_data)) "Articles" else "articles"
  
  p <- ggplot2::ggplot(raw_data, ggplot2::aes_string(x = year_col, y = articles_col)) +
    ggplot2::geom_line(color = "#0072BD", linewidth = 0.8) +
    ggplot2::geom_point(color = "#0072BD", size = 2, alpha = 0.8) +
    ggplot2::scale_x_continuous(name = "Year", breaks = scales::breaks_pretty(n = 8)) +
    ggplot2::scale_y_continuous(name = "Publications", labels = scales::label_number(big.mark = ",")) +
    ieee_theme_timeseries() +
    ggplot2::labs(
      title = "Annual Publication Trend",
      subtitle = "Time series of scientific output"
    )
  
  p
}

#' Create descriptive stats plot
#' @keywords internal
create_descriptive_stats_plot <- function(result, config) {
  stats <- result$summary_stats
  if (is.null(stats)) return(NULL)
  
  # Create a summary text plot
  stats_text <- sprintf(
    "Descriptive Statistics\n\nTotal Papers: %d\nMean: %.1f\nMedian: %.1f\nSD: %.1f\nGrowth Rate: %.2f%%",
    stats$total_papers %||% NA,
    stats$mean %||% NA,
    stats$median %||% NA,
    stats$sd %||% NA,
    stats$growth_rate %||% NA
  )
  
  p <- ggplot2::ggplot() +
    ggplot2::annotate(
      "text",
      x = 0.5, y = 0.5,
      label = stats_text,
      hjust = 0.5, vjust = 0.5,
      size = 4,
      family = "mono",
      fontface = "bold"
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ieee_theme() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank()
    ) +
    ggplot2::labs(title = "Summary Statistics")
  
  p
}

#' Null-coalescing operator
`%||%` <- function(a, b) if (!is.null(a)) a else b