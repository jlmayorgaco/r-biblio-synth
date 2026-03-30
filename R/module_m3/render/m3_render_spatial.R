# ============================================================================
# m3_render_spatial.R - Spatial Analysis Visualization
# ============================================================================

#' Render spatial analysis results
#'
#' @param data Output from m3_compute_spatial
#' @param config Configuration list
#' @return List with plots
#' @export
render_m3_spatial <- function(data, config = biblio_config()) {
  if (is.null(data) || data$status != "success") {
    return(list(plots = list(), status = data$status %||% "error"))
  }
  
  plots <- list()
  
  if (!is.null(data$morans_i)) {
    plots$morans_i <- create_morans_i_plot(data$morans_i, config)
  }
  
  if (!is.null(data$lisa) && !is.null(data$lisa$clusters)) {
    plots$lisa <- create_lisa_plot(data$lisa, config)
  }
  
  if (!is.null(data$getis_ord)) {
    plots$hotspots <- create_hotspot_plot(data$getis_ord, config)
  }
  
  list(
    plots = plots,
    status = "success"
  )
}

#' Create Moran's I visualization
#' @keywords internal
create_morans_i_plot <- function(morans_data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  
  stat <- morans_data$statistic
  p_val <- morans_data$p_value
  
  df <- data.frame(
    metric = c("Moran's I", "Expected", "p-value"),
    value = c(stat, -1/19, p_val)
  )
  
  ggplot2::ggplot(df[1:2, ], ggplot2::aes(x = metric, y = value)) +
    ggplot2::geom_col(fill = "#2166AC", alpha = 0.7) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::annotate("text", x = 1.5, y = max(stat, 0.5) * 0.8, 
                      label = sprintf("p = %.4f", p_val), size = 4) +
    ggplot2::labs(
      title = "Spatial Autocorrelation (Moran's I)",
      x = NULL,
      y = "Value"
    ) +ieee_theme()
}

#' Create LISA cluster plot
#' @keywords internal
create_lisa_plot <- function(lisa_data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  
  clusters <- lisa_data$clusters
  if (is.null(clusters) || nrow(clusters) == 0) return(NULL)
  
  ggplot2::ggplot(clusters, ggplot2::aes(x = reorder(country, n_countries), y = n_countries, fill = cluster_type)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c("HH" = "#B2182B", "LL" = "#2166AC", 
                                           "HL" = "#EF8A62", "LH" = "#67A9CF",
                                           "Not significant" = "gray70")) +
    ggplot2::labs(
      title = "LISA Cluster Types by Country",
      x = "Country",
      y = "Count",
      fill = "Cluster Type"
    ) +ieee_theme()
}

#' Create hotspot (Getis-Ord) plot
#' @keywords internal
create_hotspot_plot <- function(getis_data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  
  hotspots <- getis_data$hotspots
  if (is.null(hotspots) || nrow(hotspots) == 0) return(NULL)
  
  top_n <- config$top_n_countries %||% min(20, nrow(hotspots))
  hotspots_top <- hotspots[1:min(top_n, nrow(hotspots)), ]
  
  hotspots_top$country <- factor(hotspots_top$country, 
                                  levels = hotspots_top$country[order(hotspots_top$gi_star)])
  
  ggplot2::ggplot(hotspots_top, ggplot2::aes(x = country, y = gi_star, fill = gi_star > 0)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c("TRUE" = "#B2182B", "FALSE" = "#2166AC")) +
    ggplot2::labs(
      title = "Hotspot Analysis (Getis-Ord Gi*)",
      x = "Country",
      y = "Gi* Statistic",
      fill = "Hotspot"
    ) +ieee_theme() +
    ggplot2::theme(legend.position = "none")
}

#' Render regional analysis
#'
#' @param data Output from m3_compute_regional
#' @param config Configuration list
#' @return List with plots
#' @export
render_m3_regional <- function(data, config = biblio_config()) {
  if (is.null(data) || data$status != "success") {
    return(list(plots = list(), status = data$status %||% "error"))
  }
  
  plots <- list()
  
  if (!is.null(data$by_continent)) {
    plots$continent <- create_regional_bar_plot(data$by_continent, "Continent", config)
    plots$continent_pie <- create_regional_pie_plot(data$by_continent, "Continent", config)
  }
  
  if (!is.null(data$by_oecd)) {
    plots$oecd <- create_regional_bar_plot(data$by_oecd, "OECD Status", config)
  }
  
  if (!is.null(data$by_brics)) {
    plots$brics <- create_regional_bar_plot(data$by_brics, "BRICS Status", config)
  }
  
  list(
    plots = plots,
    status = "success"
  )
}

#' Create regional bar plot
#' @keywords internal
create_regional_bar_plot <- function(regional_data, title_suffix, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  
  summary <- regional_data$summary
  if (is.null(summary) || nrow(summary) == 0) return(NULL)
  
  summary$region <- factor(summary$region, levels = summary$region[order(-summary$total_production)])
  
  ggplot2::ggplot(summary, ggplot2::aes(x = region, y = total_production, fill = region)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", share)), 
                        vjust = -0.3, size = 3) +
    ggplot2::labs(
      title = paste("Production by", title_suffix),
      x = NULL,
      y = "Total Production"
    ) +ieee_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

#' Create regional pie plot
#' @keywords internal
create_regional_pie_plot <- function(regional_data, title_suffix, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  
  summary <- regional_data$summary
  if (is.null(summary) || nrow(summary) < 2) return(NULL)
  
  ggplot2::ggplot(summary, ggplot2::aes(x = "", y = total_production, fill = region)) +
    ggplot2::geom_col(width = 1, color = "white") +
    ggplot2::coord_polar("y") +
    ggplot2::labs(
      title = paste("Share by", title_suffix),
      fill = "Region"
    ) +ieee_theme() +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank())
}

#' Render economic correlation
#'
#' @param data Output from m3_compute_economic_correlation
#' @param config Configuration list
#' @return List with plots
#' @export
render_m3_economic <- function(data, config = biblio_config()) {
  if (is.null(data) || data$status != "success") {
    return(list(plots = list(), status = data$status %||% "error"))
  }
  
  plots <- list()
  
  if (!is.null(data$matched_data)) {
    plots$gdp_production <- create_gdp_production_plot(data, config)
    plots$hdi_production <- create_hdi_production_plot(data, config)
  }
  
  if (!is.null(data$regression)) {
    plots$regression <- create_regression_plot(data, config)
  }
  
  list(
    plots = plots,
    status = "success"
  )
}

#' Create GDP vs production plot
#' @keywords internal
create_gdp_production_plot <- function(data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  
  df <- data$matched_data
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  ggplot2::ggplot(df, ggplot2::aes(x = log(gdp_usd), y = log(production + 1))) +
    ggplot2::geom_point(alpha = 0.6, color = "#2166AC") +
    ggplot2::geom_smooth(method = "lm", color = "#B2182B", se = FALSE) +
    ggplot2::labs(
      title = "GDP vs. Scientific Production",
      x = "log(GDP in USD)",
      y = "log(Publications)"
    ) +ieee_theme()
}

#' Create HDI vs production plot
#' @keywords internal
create_hdi_production_plot <- function(data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  
  df <- data$matched_data
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  ggplot2::ggplot(df, ggplot2::aes(x = hdi, y = log(production + 1))) +
    ggplot2::geom_point(alpha = 0.6, color = "#67A9CF") +
    ggplot2::geom_smooth(method = "lm", color = "#B2182B", se = FALSE) +
    ggplot2::labs(
      title = "HDI vs. Scientific Production",
      x = "Human Development Index",
      y = "log(Publications)"
    ) +ieee_theme()
}

#' Create regression visualization
#' @keywords internal
create_regression_plot <- function(data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  
  reg <- data$regression
  if (is.null(reg)) return(NULL)
  
  df <- data.frame(
    variable = names(reg$coefficients),
    coefficient = reg$coefficients,
    stringsAsFactors = FALSE
  )
  
  df <- df[df$variable != "(Intercept)", ]
  df$significance <- ifelse(reg$p_values[df$variable] < 0.05, "Significant", "Not significant")
  
ggplot2::ggplot(df, ggplot2::aes(x = variable, y = coefficient, fill = significance)) +
    ggplot2::geom_col() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c("Significant" = "#2166AC", "Not significant" = "gray70")) +
    ggplot2::labs(
      title = sprintf("Development Predictors (R² = %.2f)", reg$R_squared),
      x = "Variable",
      y = "Coefficient",
      fill = NULL
    ) +
    ieee_theme()
}

# ============================================================================
# Table Builders
# ============================================================================

#' Build spatial analysis table
#' @export
build_m3_spatial_table <- function(data, config = biblio_config()) {
  if (is.null(data) || data$status != "success") {
    return(data.frame(
      metric = character(),
      value = numeric(),
      interpretation = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  rows <- list()
  
  if (!is.null(data$morans_i)) {
    rows <- c(rows, list(data.frame(
      metric = "Moran's I",
      value = data$morans_i$statistic,
      interpretation = data$morans_i$interpretation %||% "",
      stringsAsFactors = FALSE
    )))
  }
  
  if (!is.null(data$gearys_c)) {
    rows <- c(rows, list(data.frame(
      metric = "Geary's C",
      value = data$gearys_c$statistic,
      interpretation = data$gearys_c$interpretation %||% "",
      stringsAsFactors = FALSE
    )))
  }
  
  if (!is.null(data$inequality)) {
    rows <- c(rows, list(data.frame(
      metric = "Gini Coefficient",
      value = data$inequality$gini,
      interpretation = data$inequality$interpretation %||% "",
      stringsAsFactors = FALSE
    )))
  }
  
  if (length(rows) > 0) {
    do.call(rbind, rows)
  } else {
    data.frame(
      metric = character(),
      value = numeric(),
      interpretation = character(),
      stringsAsFactors = FALSE
    )
  }
}

#' Build regional analysis table
#' @export
build_m3_regional_table <- function(data, config = biblio_config()) {
  if (is.null(data) || data$status != "success") {
    return(data.frame(
      region = character(),
      n_countries = integer(),
      total_production = integer(),
      share = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  
  if (!is.null(data$by_continent) && !is.null(data$by_continent$summary)) {
    return(data$by_continent$summary)
  }
  
  data.frame(
    region = character(),
    n_countries = integer(),
    total_production = integer(),
    share = numeric(),
    stringsAsFactors = FALSE
  )
}

#' Build economic analysis table
#' @export
build_m3_economic_table <- function(data, config = biblio_config()) {
  if (is.null(data) || data$status != "success") {
    return(data.frame(
      country = character(),
      production = integer(),
      gdp_usd = numeric(),
      hdi = numeric(),
      production_per_capita = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  
  if (!is.null(data$matched_data)) {
    cols <- intersect(c("country", "production", "gdp_usd", "hdi", "production_per_capita"),
                      names(data$matched_data))
    return(data$matched_data[, cols, drop = FALSE])
  }
  
  data.frame(
    country = character(),
    production = integer(),
    gdp_usd = numeric(),
    hdi = numeric(),
    production_per_capita = numeric(),
    stringsAsFactors = FALSE
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b