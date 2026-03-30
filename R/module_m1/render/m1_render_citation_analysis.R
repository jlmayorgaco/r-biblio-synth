# ============================================================================
# m1_render_citation_analysis.R - Citation Analysis Visualization
# ============================================================================

#' Render citation analysis results
#'
#' @param data Output from compute_m1_citation_analysis
#' @param config Configuration list
#' @return List with citation analysis plots
#' @export
render_m1_citation_analysis <- function(data, config = biblio_config()) {
  if (is.null(data) || startsWith(data$status %||% "error", "error")) {
    return(list(plots = list(), status = data$status %||% "error"))
  }
  
  plots <- list()
  
  if (!is.null(data$distribution_fit)) {
    plots$distribution_fit <- create_citation_distribution_plot(data, config)
  }
  
  if (!is.null(data$citation_velocity)) {
    plots$velocity <- create_citation_velocity_plot(data, config)
  }
  
  if (!is.null(data$self_citation)) {
    plots$self_citation <- create_self_citation_plot(data, config)
  }
  
  if (!is.null(data$age_analysis)) {
    plots$age_analysis <- create_citation_age_plot(data, config)
  }
  
  if (!is.null(data$h_index_evolution)) {
    plots$h_index <- create_h_index_evolution_plot(data, config)
  }
  
  list(
    plots = plots,
    status = "success"
  )
}

#' Create citation distribution fit plot
#' @keywords internal
create_citation_distribution_plot <- function(data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }
  
  dist_fit <- data$distribution_fit
  if (is.null(dist_fit)) return(NULL)
  
  observed <- dist_fit$observed
  if (is.null(observed)) return(NULL)
  
  hist_data <- data.frame(
    citations = observed,
    count = seq_along(observed)
  )
  
  p <- ggplot2::ggplot(hist_data, ggplot2::aes(x = citations)) +
    ggplot2::geom_histogram(bins = 50, fill = "#2166AC", alpha = 0.7) +
    ggplot2::scale_x_log10() +
    ggplot2::labs(
      title = "Citation Distribution (Log Scale)",
      x = "Citations (log10)",
      y = "Frequency"
    ) +
    ieee_theme()
  
  if (!is.null(dist_fit$fitted_power_law)) {
    p <- p + ggplot2::geom_line(
      data = data.frame(x = dist_fit$x_values, y = dist_fit$fitted_power_law),
      ggplot2::aes(x = x, y = y),
      color = "#B2182B",
      size = 1
    )
  }
  
  p
}

#' Create citation velocity plot
#' @keywords internal
create_citation_velocity_plot <- function(data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }
  
  velocity <- data$citation_velocity
  if (is.null(velocity) || is.null(velocity$by_year)) return(NULL)
  
  ggplot2::ggplot(velocity$by_year, ggplot2::aes(x = year, y = velocity)) +
    ggplot2::geom_line(color = "#2166AC", size = 1) +
    ggplot2::geom_point(color = "#2166AC", size = 2) +
    ggplot2::labs(
      title = "Citation Velocity Over Time",
      x = "Year",
      y = "Average Citations per Paper per Year"
    ) +
    ieee_theme()
}

#' Create self-citation plot
#' @keywords internal
create_self_citation_plot <- function(data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }
  
  self_cite <- data$self_citation
  if (is.null(self_cite)) return(NULL)
  
  plot_data <- data.frame(
    type = c("Self-citations", "External citations"),
    count = c(self_cite$self_citations, self_cite$external_citations),
    stringsAsFactors = FALSE
  )
  
  ggplot2::ggplot(plot_data, ggplot2::aes(x = type, y = count, fill = type)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = c("#2166AC", "#67A9CF")) +
    ggplot2::labs(
      title = "Self-citation Analysis",
      x = "",
      y = "Count"
    ) +
    ieee_theme()
}

#' Create citation age plot
#' @keywords internal
create_citation_age_plot <- function(data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }
  
  age <- data$age_analysis
  if (is.null(age) || is.null(age$age_distribution)) return(NULL)
  
  ggplot2::ggplot(age$age_distribution, ggplot2::aes(x = age, y = total_citations)) +
    ggplot2::geom_col(fill = "#2166AC", alpha = 0.7) +
    ggplot2::labs(
      title = "Citations by Age of Publication",
      x = "Age (Years since Publication)",
      y = "Total Citations"
    ) +
    ieee_theme()
}

#' Create H-index evolution plot
#' @keywords internal
create_h_index_evolution_plot <- function(data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }
  
  h_evolution <- data$h_index_evolution
  if (is.null(h_evolution)) return(NULL)
  
  ggplot2::ggplot(h_evolution, ggplot2::aes(x = year, y = h_index)) +
    ggplot2::geom_line(color = "#2166AC", size = 1) +
    ggplot2::geom_point(color = "#2166AC", size = 2) +
    ggplot2::labs(
      title = "H-index Evolution Over Time",
      x = "Year",
      y = "Cumulative H-index"
    ) +
    ieee_theme()
}

#' Build citation analysis table
#' @export
build_m1_citation_analysis_table <- function(data, config = biblio_config()) {
  if (is.null(data) || startsWith(data$status %||% "error", "error")) {
    return(data.frame(
      metric = character(),
      value = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  
  summary_stats <- data$summary
  if (is.null(summary_stats)) {
    return(data.frame(
      metric = character(),
      value = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  
  data.frame(
    metric = c("Total Citations", "Mean Citations", "Median Citations", 
               "SD Citations", "Max Citations", "Gini Coefficient",
               "Zero-citation %", "Half-life"),
    value = c(
      summary_stats$total_citations %||% NA,
      summary_stats$mean_citations %||% NA,
      summary_stats$median_citations %||% NA,
      summary_stats$sd_citations %||% NA,
      summary_stats$max_citations %||% NA,
      summary_stats$gini_coefficient %||% NA,
      summary_stats$pct_zero_citations %||% NA,
      data$age_analysis$citation_half_life %||% NA
    ),
    stringsAsFactors = FALSE
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b