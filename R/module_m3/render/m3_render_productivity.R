# ============================================================================
# m3_render_productivity.R - Enhanced Production Plots (IEEE Q1 Legacy Compatible)
# ============================================================================
# Creates legacy-style plots:
# - M3_p1_tp_bars.png - Total publications bar chart
# - M3_p1_tp_gini.png - Gini coefficient for publications
# - M3_p1_gini_vs_year.png - Gini vs year trend
# - M3_continent_tp_totals.png - Total publications by continent
# ============================================================================

#' Render all M3 productivity plots (Enhanced IEEE Q1 Quality)
#'
#' @param production_data Output from m3_compute_production
#' @param config Configuration list
#' @return List with plots
#' @export
m3_render_productivity <- function(production_data, config = biblio_config()) {
  if (!is.list(production_data) || production_data$status == "stub") {
    return(list(status = "stub", plots = list()))
  }
  
  plots <- list()
  
  # Extract data
  top_countries <- production_data$top_countries_by_production
  country_prod <- production_data$country_production
  annual_prod <- production_data$annual_production
  
  # Define top N for bar charts
  top_n <- config$top_n_countries %||% 10
  
  # 1. M3_p1_tp_bars - Top countries by total publications (bar chart)
  plots$bar_production <- create_tp_bars_plot(top_countries, top_n, config)
  
  # 2. Legendre form (long-tail visualization)
  if (!is.null(country_prod) && nrow(country_prod) > 0) {
    plots$cumulative_share <- create_cumulative_share_plot(country_prod, config)
  }
  
  # 3. M3_p1_tp_gini - Gini coefficient for production
  if ("gini" %in% names(production_data)) {
    plots$gini_production <- create_gini_law_plot(production_data, config)
  }
  
  # 4. M3_p1_gini_vs_year - Gini trend over time
  if (!is.null(annual_prod) && nrow(annual_prod) > 0 && "gini" %in% names(annual_prod)) {
    plots$gini_vs_year <- create_gini_vs_year_plot(annual_prod, "Production", config)
  }
  
  # 5. Production by continent (if continent data available)
  if ("continent_production" %in% names(production_data)) {
    plots$continent_tp_totals <- create_continent_plot(production_data$continent_production, "Productions", config)
  }
  
  # 6. Top countries rank-cumulative share
  if (!is.null(country_prod) && nrow(country_prod) >= 3) {
    plots$rank_cumshare <- create_rank_cumshare_plot(country_prod, config)
  }
  
  list(status = "success", plots = plots)
}

#' Create TP bars plot (Top Publications)
#' @keywords internal
create_tp_bars_plot <- function(top_countries, top_n, config) {
  if (is.null(top_countries) || nrow(top_countries) == 0) return(NULL)
  
  # Take top N countries
  top_countries <- head(top_countries[order(-top_countries$value), ], top_n)
  top_countries$label <- vapply(as.character(top_countries$label), function(x) {
    parts <- unlist(strsplit(tolower(trimws(x)), "\\s+"))
    paste0(toupper(substr(parts, 1, 1)), substring(parts, 2), collapse = " ")
  }, character(1))
  top_countries$label <- substr(trimws(top_countries$label), 1, 30)
  top_countries$label <- factor(top_countries$label, levels = rev(top_countries$label))
  
  # Calculate percentage
  total <- sum(top_countries$value, na.rm = TRUE)
  top_countries$pct <- top_countries$value / total * 100
  top_countries$value_label <- sprintf("%s (%.1f%%)", scales::comma(top_countries$value), top_countries$pct)
  
  p <- ggplot2::ggplot(top_countries, ggplot2::aes(x = label, y = value)) +
    ggplot2::geom_col(fill = "#0072BD", color = "black", linewidth = 0.3, width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = value_label),
      hjust = -0.08,
      size = 2.7
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      name = "Total Publications",
      labels = scales::label_number(big.mark = ","),
      expand = ggplot2::expansion(mult = c(0, 0.32))
    ) +
    ggplot2::scale_x_discrete(name = NULL) +
    ieee_theme_bar() +
    ggplot2::labs(
      title = "Top Countries by Scientific Production",
      subtitle = sprintf("Showing top %d countries", top_n)
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 8),
      plot.title = ggplot2::element_text(size = 10, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5, face = "italic"),
      plot.margin = ggplot2::margin(6, 28, 6, 6)
    )
  
  p
}

#' Create cumulative share plot (Legendre form)
#' @keywords internal
create_cumulative_share_plot <- function(country_prod, config) {
  if (is.null(country_prod) || nrow(country_prod) < 3) return(NULL)
  
  # Rank countries by production
  ranked <- country_prod[order(-country_prod$article_count), ]
  ranked$rank <- seq_len(nrow(ranked))
  ranked$cumulative_share <- cumsum(ranked$article_count) / sum(ranked$article_count)
  
  p <- ggplot2::ggplot(ranked, ggplot2::aes(x = rank, y = cumulative_share)) +
    ggplot2::geom_line(color = "#0072BD", linewidth = 1) +
    ggplot2::geom_point(color = "#0072BD", size = 1.5, alpha = 0.7) +
    ggplot2::geom_hline(yintercept = c(0.5, 0.8), linetype = "dashed", color = "#D95319", linewidth = 0.4) +
    ggplot2::geom_hline(yintercept = 1, linetype = "solid", color = "black", linewidth = 0.3) +
    ggplot2::annotate(
      "text",
      x = max(ranked$rank) * 0.05, y = 0.52,
      label = "50%", hjust = 0, size = 3, color = "#D95319"
    ) +
    ggplot2::annotate(
      "text",
      x = max(ranked$rank) * 0.05, y = 0.82,
      label = "80%", hjust = 0, size = 3, color = "#D95319"
    ) +
    ggplot2::scale_x_continuous(
      name = "Country Rank",
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::scale_y_continuous(
      name = "Cumulative Share",
      labels = scales::label_percent(accuracy = 1),
      limits = c(0, 1),
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ieee_theme() +
    ggplot2::labs(
      title = "Cumulative Share of Publications by Country Rank",
      subtitle = "Concentration pattern (50% and 80% thresholds marked)"
    )
  
  p
}

#' Create Gini Lorenz plot
#' @keywords internal
create_gini_law_plot <- function(production_data, config) {
  if (!"gini" %in% names(production_data)) return(NULL)
  
  gini_val <- production_data$gini
  
  # Create Lorenz curve data
  country_prod <- production_data$country_production
  if (is.null(country_prod) || nrow(country_prod) < 3) return(NULL)
  
  ranked <- country_prod[order(-country_prod$article_count), ]
  ranked$rank <- seq_len(nrow(ranked))
  ranked$cumulative_share <- cumsum(ranked$article_count) / sum(ranked$article_count)
  ranked$cumulative_prop <- ranked$rank / nrow(ranked)
  
  lorenz_data <- data.frame(
    prop_pop = c(0, ranked$cumulative_prop),
    prop_value = c(0, ranked$cumulative_share)
  )
  
  # Perfect equality line
  equality_line <- data.frame(
    x = c(0, 1),
    y = c(0, 1)
  )
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = equality_line,
      ggplot2::aes(x = x, y = y),
      linetype = "dashed",
      color = "#666666",
      linewidth = 0.5
    ) +
    ggplot2::geom_line(
      data = lorenz_data,
      ggplot2::aes(x = prop_pop, y = prop_value),
      color = "#0072BD",
      linewidth = 1
    ) +
    ggplot2::geom_polygon(
      data = rbind(
        data.frame(prop_pop = 0, prop_value = 0),
        lorenz_data,
        data.frame(prop_pop = 1, prop_value = 0)
      ),
      ggplot2::aes(x = prop_pop, y = prop_value),
      fill = "#0072BD",
      alpha = 0.2
    ) +
    ggplot2::annotate(
      "text",
      x = 0.5, y = 0.15,
      label = sprintf("Gini = %.4f", gini_val),
      size = 5,
      fontface = "bold",
      family = "mono"
    ) +
    ggplot2::scale_x_continuous(
      name = "Cumulative Proportion of Countries",
      limits = c(0, 1),
      labels = scales::label_percent(accuracy = 1)
    ) +
    ggplot2::scale_y_continuous(
      name = "Cumulative Proportion of Publications",
      limits = c(0, 1),
      labels = scales::label_percent(accuracy = 1)
    ) +
    ieee_theme() +
    ggplot2::labs(
      title = "Lorenz Curve for Publication Concentration",
      subtitle = "Gini coefficient measures inequality (0 = perfect equality, 1 = maximum inequality)"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 10, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5, face = "italic")
    )
  
  p
}

#' Create Gini vs Year plot
#' @keywords internal
create_gini_vs_year_plot <- function(annual_prod, metric_name, config) {
  if (is.null(annual_prod) || nrow(annual_prod) == 0) return(NULL)
  if (!"gini" %in% names(annual_prod)) return(NULL)
  
  year_col <- if ("year" %in% names(annual_prod)) "year" else "Year"
  
  p <- ggplot2::ggplot(annual_prod, ggplot2::aes_string(x = year_col, y = "gini")) +
    ggplot2::geom_line(color = "#0072BD", linewidth = 0.8) +
    ggplot2::geom_point(color = "#0072BD", size = 2, alpha = 0.8) +
    ggplot2::geom_smooth(
      method = "loess",
      se = TRUE,
      color = "#D95319",
      fill = "#D95319",
      alpha = 0.2,
      linewidth = 0.6
    ) +
    ggplot2::scale_x_continuous(
      name = "Year",
      breaks = scales::breaks_pretty(n = 8)
    ) +
    ggplot2::scale_y_continuous(
      name = "Gini Coefficient",
      limits = c(0, 1),
      labels = scales::label_number(accuracy = 0.01)
    ) +
    ieee_theme_timeseries() +
    ggplot2::labs(
      title = sprintf("Publication Inequality Over Time (%s)", metric_name),
      subtitle = "Gini coefficient evolution"
    )
  
  p
}

#' Create continent totals plot
#' @keywords internal
create_continent_plot <- function(continent_data, metric, config) {
  if (is.null(continent_data) || nrow(continent_data) == 0) return(NULL)
  
  continent_data$continent <- factor(continent_data$continent, 
                                      levels = continent_data$continent[order(-continent_data$value)])
  
  p <- ggplot2::ggplot(continent_data, ggplot2::aes(x = continent, y = value)) +
    ggplot2::geom_col(fill = "#77AC30", color = "black", linewidth = 0.3, width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::number(value, big.mark = ",")),
      hjust = -0.15,
      size = 2.5,
      family = "mono"
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      name = sprintf("Total %s", metric),
      labels = scales::label_number(big.mark = ","),
      expand = ggplot2::expansion(mult = c(0, 0.2))
    ) +
    ggplot2::scale_x_discrete(name = NULL) +
    ieee_theme_bar() +
    ggplot2::labs(
      title = sprintf("%s by Continent", metric),
      subtitle = "Geographic distribution"
    )
  
  p
}

#' Create rank-cumulative share plot
#' @keywords internal
create_rank_cumshare_plot <- function(country_prod, config) {
  ranked <- country_prod[order(-country_prod$article_count), ]
  ranked$rank <- seq_len(nrow(ranked))
  ranked$cum_proportion <- cumsum(ranked$article_count) / sum(ranked$article_count)
  ranked$cum_share <- ranked$cum_proportion * 100
  
  p <- ggplot2::ggplot(ranked, ggplot2::aes(x = rank, y = cum_share)) +
    ggplot2::geom_line(color = "#0072BD", linewidth = 0.8) +
    ggplot2::geom_point(color = "#0072BD", size = 1.5, alpha = 0.7) +
    ggplot2::scale_x_continuous(name = "Country Rank (sorted by production)") +
    ggplot2::scale_y_continuous(
      name = "Cumulative Share (%)",
      limits = c(0, 100),
      labels = scales::label_number(accuracy = 1)
    ) +
    ieee_theme() +
    ggplot2::labs(
      title = "Concentration of Scientific Production",
      subtitle = "Cumulative share of publications by country rank"
    )
  
  p
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
