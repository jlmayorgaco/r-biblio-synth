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

  if (!is.null(data$citations) && length(data$citations) > 0) {
    plots$distribution_fit <- create_citation_distribution_plot(data, config)
  }

  if (!is.null(data$distribution_fit$comparison) && nrow(data$distribution_fit$comparison) > 0) {
    plots$model_comparison <- create_distribution_comparison_plot(data, config)
  }

  if (!is.null(data$velocity) && !is.null(data$velocity$velocity_by_year)) {
    plots$velocity <- create_citation_velocity_plot(data, config)
  }

  if (!is.null(data$self_citation)) {
    plots$self_citation <- create_self_citation_plot(data, config)
  }

  if (!is.null(data$age_analysis)) {
    plots$age_analysis <- create_citation_age_plot(data, config)
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

  citations <- as.numeric(data$citations)
  citations <- citations[!is.na(citations) & citations > 0]
  if (length(citations) == 0) return(NULL)

  hist_data <- data.frame(citations = citations, stringsAsFactors = FALSE)

  p <- ggplot2::ggplot(hist_data, ggplot2::aes(x = citations)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(density)),
      bins = 30,
      fill = "#2166AC",
      alpha = 0.65
    ) +
    ggplot2::scale_x_log10() +
    ggplot2::labs(
      title = "Citation Distribution",
      subtitle = paste("Best fit:", format_distribution_name(data$distribution_fit$best_fit %||% "unknown")),
      x = "Citations (log10 scale)",
      y = "Density"
    ) +
    ieee_theme()

  density_df <- build_best_fit_density(data$distribution_fit, citations)
  if (!is.null(density_df) && nrow(density_df) > 0) {
    p <- p + ggplot2::geom_line(
      data = density_df,
      ggplot2::aes(x = x, y = density),
      inherit.aes = FALSE,
      color = "#B2182B",
      linewidth = 0.9
    )
  }

  p
}

#' Create distribution comparison plot
#' @keywords internal
create_distribution_comparison_plot <- function(data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }

  comparison <- data$distribution_fit$comparison
  if (is.null(comparison) || nrow(comparison) == 0) return(NULL)

  comparison$distribution <- format_distribution_name(comparison$distribution)
  comparison$distribution <- stats::reorder(comparison$distribution, comparison$AIC)

  ggplot2::ggplot(comparison, ggplot2::aes(x = distribution, y = AIC)) +
    ggplot2::geom_col(fill = "#67A9CF", alpha = 0.8) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Citation Model Comparison",
      x = "Distribution",
      y = "AIC"
    ) +
    ieee_theme()
}

#' Build density overlay for best-fitting distribution
#' @keywords internal
build_best_fit_density <- function(distribution_fit, citations) {
  if (is.null(distribution_fit) || length(citations) == 0) {
    return(NULL)
  }

  x_grid <- exp(seq(log(max(min(citations), 1e-3)), log(max(citations)), length.out = 200))
  best_fit <- distribution_fit$best_fit %||% ""
  params <- distribution_fit[[best_fit]]

  density <- switch(
    best_fit,
    power_law = {
      alpha <- as.numeric(params$alpha %||% NA_real_)
      xmin <- as.numeric(params$xmin %||% min(citations))
      y <- rep(NA_real_, length(x_grid))
      valid <- is.finite(alpha) && is.finite(xmin) && alpha > 1
      if (valid) {
        idx <- x_grid >= xmin
        y[idx] <- (alpha - 1) * xmin^(alpha - 1) / (x_grid[idx]^alpha)
      }
      y
    },
    log_normal = dlnorm(x_grid, meanlog = params$meanlog %||% 0, sdlog = params$sdlog %||% 1),
    exponential = dexp(x_grid, rate = params$lambda %||% 1),
    weibull = dweibull(x_grid, shape = params$shape %||% 1, scale = params$scale %||% 1),
    rep(NA_real_, length(x_grid))
  )

  density <- as.numeric(density)
  density[!is.finite(density)] <- NA_real_
  if (all(is.na(density))) {
    return(NULL)
  }

  data.frame(x = x_grid, density = density, stringsAsFactors = FALSE)
}

#' Create citation velocity plot
#' @keywords internal
create_citation_velocity_plot <- function(data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }

  velocity <- data$velocity
  if (is.null(velocity) || is.null(velocity$velocity_by_year)) return(NULL)

  ggplot2::ggplot(velocity$velocity_by_year, ggplot2::aes(x = year, y = median_velocity)) +
    ggplot2::geom_line(color = "#2166AC", linewidth = 1) +
    ggplot2::geom_point(color = "#2166AC", size = 2) +
    ggplot2::labs(
      title = "Citation Velocity Over Time",
      x = "Publication Year",
      y = "Median Annual Citation Velocity"
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

  self_count <- self_cite$self_citations %||% self_cite$n_self_citations %||% 0
  total_refs <- self_cite$n_total_references %||% NA_real_
  external_count <- self_cite$external_citations %||%
    if (is.finite(total_refs)) max(total_refs - self_count, 0) else 0

  plot_data <- data.frame(
    type = c("Self-citations", "External citations"),
    count = c(self_count, external_count),
    stringsAsFactors = FALSE
  )

  ggplot2::ggplot(plot_data, ggplot2::aes(x = type, y = count, fill = type)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = c("#2166AC", "#67A9CF")) +
    ggplot2::labs(
      title = "Self-citation Analysis",
      x = NULL,
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
  if (is.null(age) || is.null(age$age_distribution) || nrow(age$age_distribution) == 0) return(NULL)

  ggplot2::ggplot(age$age_distribution, ggplot2::aes(x = age, y = total_citations)) +
    ggplot2::geom_col(fill = "#2166AC", alpha = 0.7) +
    ggplot2::labs(
      title = "Citations by Publication Age",
      subtitle = paste("Half-life:", round(age$half_life %||% NA_real_, 2), "years"),
      x = "Age (Years since Publication)",
      y = "Total Citations"
    ) +
    ieee_theme()
}

#' Build citation analysis table
#' @export
build_m1_citation_analysis_table <- function(data, config = biblio_config()) {
  empty <- list(
    status = "stub",
    table = tibble::tibble(),
    summary = list()
  )

  if (is.null(data) || startsWith(data$status %||% "error", "error")) {
    return(empty)
  }

  summary_stats <- data$summary
  if (is.null(summary_stats)) {
    return(empty)
  }

  table <- tibble::tibble(
    metric = c(
      "Total Citations", "Mean Citations", "Median Citations", "SD Citations",
      "Max Citations", "Gini Coefficient", "Zero-citation %",
      "H-index", "G-index", "I10-index", "Citation Half-life", "Price Index"
    ),
    value = c(
      summary_stats$total_citations %||% NA_real_,
      summary_stats$mean_citations %||% NA_real_,
      summary_stats$median_citations %||% NA_real_,
      summary_stats$sd_citations %||% NA_real_,
      summary_stats$max_citations %||% NA_real_,
      summary_stats$gini_coefficient %||% NA_real_,
      summary_stats$pct_zero_citations %||% NA_real_,
      summary_stats$h_index %||% NA_real_,
      summary_stats$g_index %||% NA_real_,
      summary_stats$i10_index %||% NA_real_,
      data$age_analysis$half_life %||% NA_real_,
      data$age_analysis$price_index %||% NA_real_
    )
  )

  list(
    status = "success",
    table = table,
    summary = list(
      best_fit = data$distribution_fit$best_fit %||% NA_character_,
      self_citation_rate = data$self_citation$self_citation_rate %||% NA_real_,
      mean_velocity = data$velocity$mean_velocity %||% NA_real_
    )
  )
}

#' Format distribution names for display
#' @keywords internal
format_distribution_name <- function(x) {
  x <- as.character(x)
  x <- gsub("_", " ", x)
  tools::toTitleCase(x)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
