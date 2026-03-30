# ============================================================================
# m3_render_citations.R - Enhanced Citation Plots (IEEE Q1 Legacy Compatible)
# ============================================================================
# Creates legacy-style plots:
# - M3_p1_tc_bars.png - Total citations bar chart
# - M3_p1_tc_gini.png - Gini coefficient for citations
# - M3_p1_tc_gini_vs_year.png - Citation Gini vs year trend
# - M3_continent_tc_totals.png - Total citations by continent
# ============================================================================

#' Render all M3 citation plots (Enhanced IEEE Q1 Quality)
#'
#' @param citation_data Output from m3_compute_citations
#' @param config Configuration list
#' @return List with plots
#' @export
m3_render_citations <- function(citation_data, config = biblio_config()) {
  if (!is.list(citation_data) || citation_data$status == "stub") {
    return(list(status = "stub", plots = list()))
  }
  
  plots <- list()
  
  top_countries <- citation_data$top_countries_by_citations
  top_n <- config$top_n_countries %||% 10
  
  # 1. M3_p1_tc_bars - Total citations bar chart
  plots$bar_total_citations <- create_tc_bars_plot(top_countries, top_n, config)
  
  # 2. Average citations bar chart
  if ("top_countries_by_avg_citations" %in% names(citation_data) && 
      nrow(citation_data$top_countries_by_avg_citations) > 0) {
    plots$bar_avg_citations <- create_avg_citations_plot(
      citation_data$top_countries_by_avg_citations, top_n, config
    )
  }
  
  # 3. Citation Gini Lorenz curve
  if ("gini" %in% names(citation_data)) {
    plots$gini_citations <- create_citation_gini_plot(citation_data, config)
  }
  
  # 4. Citation Gini vs Year
  if ("annual_citations" %in% names(citation_data) && 
      !is.null(citation_data$annual_citations) &&
      nrow(citation_data$annual_citations) > 0) {
    if ("gini" %in% names(citation_data$annual_citations)) {
      plots$tc_gini_vs_year <- create_gini_vs_year_plot(
        citation_data$annual_citations, "Citations", config
      )
    }
  }
  
  # 5. Continent citations
  if ("continent_citations" %in% names(citation_data)) {
    plots$continent_tc_totals <- create_continent_citations_plot(
      citation_data$continent_citations, config
    )
  }
  
  list(status = "success", plots = plots)
}

#' Create total citations bar plot (M3_p1_tc_bars)
#' @keywords internal
create_tc_bars_plot <- function(top_countries, top_n, config) {
  if (is.null(top_countries) || nrow(top_countries) == 0) return(NULL)
  
  if (!"value" %in% names(top_countries) && "total_citations" %in% names(top_countries)) {
    top_countries$value <- top_countries$total_citations
  }
  
  top <- head(top_countries[order(-top_countries$value), ], top_n)
  top$label_clean <- substr(trimws(top$label), 1, 25)
  top$label_clean <- factor(top$label_clean, levels = rev(top$label_clean))
  
  total <- sum(top$value, na.rm = TRUE)
  top$pct <- top$value / total * 100
  
  p <- ggplot2::ggplot(top, ggplot2::aes(x = label_clean, y = value)) +
    ggplot2::geom_col(fill = "#D95319", color = "black", linewidth = 0.3, width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%d\n(%.1f%%)", round(value), pct)),
      hjust = -0.1,
      size = 2.5,
      family = "mono"
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      name = "Total Citations",
      labels = scales::label_number(big.mark = ","),
      expand = ggplot2::expansion(mult = c(0, 0.25))
    ) +
    ggplot2::labs(
      title = "Top Countries by Total Citations",
      subtitle = sprintf("Showing top %d countries", top_n),
      x = NULL
    ) +
    ieee_theme_bar() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 7),
      plot.title = ggplot2::element_text(size = 10, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5, face = "italic")
    )
  
  p
}

#' Create average citations plot
#' @keywords internal
create_avg_citations_plot <- function(top_avg, top_n, config) {
  if (is.null(top_avg) || nrow(top_avg) == 0) return(NULL)
  
  top <- head(top_avg[order(-top_avg$value), ], top_n)
  top$label_clean <- substr(trimws(top$label), 1, 25)
  top$label_clean <- factor(top$label_clean, levels = rev(top$label_clean))
  
  p <- ggplot2::ggplot(top, ggplot2::aes(x = label_clean, y = value)) +
    ggplot2::geom_col(fill = "#EDB120", color = "black", linewidth = 0.3, width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.1f", value)),
      hjust = -0.1,
      size = 2.5,
      family = "mono"
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      name = "Average Citations per Article",
      expand = ggplot2::expansion(mult = c(0, 0.2))
    ) +
    ggplot2::labs(
      title = "Top Countries by Citation Impact",
      subtitle = sprintf("Average citations per article (top %d)", top_n),
      x = NULL
    ) +
    ieee_theme_bar() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 7))
  
  p
}

#' Create citation Gini Lorenz curve
#' @keywords internal
create_citation_gini_plot <- function(citation_data, config) {
  gini_val <- citation_data$gini
  country_cit <- citation_data$country_citations
  
  if (is.null(country_cit) || nrow(country_cit) < 3) return(NULL)
  
  cit_col <- if ("total_citations" %in% names(country_cit)) "total_citations" else "value"
  
  ranked <- country_cit[order(-country_cit[[cit_col]]), ]
  ranked$rank <- seq_len(nrow(ranked))
  ranked$cumulative_share <- cumsum(ranked[[cit_col]]) / sum(ranked[[cit_col]])
  ranked$cumulative_prop <- ranked$rank / nrow(ranked)
  
  lorenz_data <- data.frame(
    prop_pop = c(0, ranked$cumulative_prop),
    prop_value = c(0, ranked$cumulative_share)
  )
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = data.frame(x = c(0, 1), y = c(0, 1)),
      ggplot2::aes(x = x, y = y),
      linetype = "dashed",
      color = "#666666",
      linewidth = 0.5
    ) +
    ggplot2::geom_line(
      data = lorenz_data,
      ggplot2::aes(x = prop_pop, y = prop_value),
      color = "#D95319",
      linewidth = 1
    ) +
    ggplot2::geom_polygon(
      data = rbind(
        data.frame(prop_pop = 0, prop_value = 0),
        lorenz_data,
        data.frame(prop_pop = 1, prop_value = 0)
      ),
      ggplot2::aes(x = prop_pop, y = prop_value),
      fill = "#D95319",
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
      name = "Cumulative Proportion of Citations",
      limits = c(0, 1),
      labels = scales::label_percent(accuracy = 1)
    ) +
    ieee_theme() +
    ggplot2::labs(
      title = "Lorenz Curve for Citation Concentration",
      subtitle = "Gini coefficient measures inequality (0 = equality, 1 = max inequality)"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 10, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5, face = "italic")
    )
  
  p
}

#' Create Gini vs Year plot for citations
#' @keywords internal
create_gini_vs_year_plot <- function(annual_data, metric_name, config) {
  year_col <- if ("year" %in% names(annual_data)) "year" else "Year"
  
  p <- ggplot2::ggplot(annual_data, ggplot2::aes_string(x = year_col, y = "gini")) +
    ggplot2::geom_line(color = "#D95319", linewidth = 0.8) +
    ggplot2::geom_point(color = "#D95319", size = 2, alpha = 0.8) +
    ggplot2::geom_smooth(
      method = "loess",
      se = TRUE,
      color = "#7E2F8E",
      fill = "#7E2F8E",
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
      title = sprintf("%s Inequality Over Time", metric_name),
      subtitle = "Gini coefficient evolution"
    )
  
  p
}

#' Create continent citations plot
#' @keywords internal
create_continent_citations_plot <- function(continent_data, config) {
  if (is.null(continent_data) || nrow(continent_data) == 0) return(NULL)
  
  if (!"value" %in% names(continent_data) && "total_citations" %in% names(continent_data)) {
    continent_data$value <- continent_data$total_citations
  }
  
  continent_data$continent <- factor(
    continent_data$continent,
    levels = continent_data$continent[order(-continent_data$value)]
  )
  
  p <- ggplot2::ggplot(continent_data, ggplot2::aes(x = continent, y = value)) +
    ggplot2::geom_col(fill = "#7E2F8E", color = "black", linewidth = 0.3, width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::number(value, big.mark = ",")),
      hjust = -0.15,
      size = 2.5,
      family = "mono"
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      name = "Total Citations",
      labels = scales::label_number(big.mark = ","),
      expand = ggplot2::expansion(mult = c(0, 0.2))
    ) +
    ggplot2::labs(
      title = "Citations by Continent",
      subtitle = "Geographic distribution of citations",
      x = NULL
    ) +
    ieee_theme_bar()
  
  p
}

`%||%` <- function(a, b) if (!is.null(a)) a else b