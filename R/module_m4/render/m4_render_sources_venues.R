# ============================================================================
# m4_render_sources_venues.R - Source / journal / venue plots
# ============================================================================

render_m4_sources <- function(data, config = biblio_config()) {
  impact <- data$impact$impact %||% tibble::tibble()
  top_n <- as.integer(config$top_n_sources %||% 20L)
  plots <- list(
    top_sources = m4_plot_top_sources(impact, top_n),
    impact_bubble = m4_plot_impact_bubble(impact, data$clusters$clusters %||% tibble::tibble())
  )
  list(status = "success", plots = plots)
}

render_m4_bradford <- function(bradford, impact, config = biblio_config()) {
  list(status = bradford$status %||% "stub", plots = list(
    bradford_zones = m4_plot_bradford_zones(bradford$zones %||% tibble::tibble()),
    zone_impact = m4_plot_bradford_zone_impact(bradford$zone_summary %||% tibble::tibble())
  ))
}

render_m4_growth <- function(growth, impact, config = biblio_config()) {
  list(status = growth$status %||% "stub", plots = list(
    growth_impact = m4_plot_growth_impact(growth$growth %||% tibble::tibble()),
    emerging_sources = m4_plot_emerging_sources(growth$growth %||% tibble::tibble(), config)
  ))
}

render_m4_clusters <- function(clusters, impact, config = biblio_config()) {
  list(status = clusters$status %||% "stub", plots = list(
    kmeans_sources = m4_plot_kmeans_sources(clusters$clusters %||% tibble::tibble())
  ))
}

render_m4_narrative <- function(narrative, config = biblio_config()) {
  metrics <- narrative$metrics %||% data.frame()
  list(status = narrative$status %||% "stub", plots = list(
    evidence_dashboard = ieee_metric_dashboard(
      metrics,
      title = "M4 Source Narrative Evidence Dashboard",
      subtitle = "Venue coverage, concentration, Bradford-core, impact, and growth signals.",
      layout = "full"
    ),
    signal_map = ieee_metric_signal_plot(
      metrics,
      title = "M4 Source Interpretive Signal Map",
      subtitle = "Normalized source evidence used to support report narratives.",
      layout = "full"
    )
  ))
}

m4_plot_top_sources <- function(impact, top_n) {
  if (!is.data.frame(impact) || nrow(impact) == 0) return(NULL)
  df <- impact |> dplyr::arrange(dplyr::desc(.data$tp)) |> dplyr::slice(seq_len(min(top_n, nrow(impact))))
  df$source_label <- factor(substr(df$source, 1, 45), levels = rev(substr(df$source, 1, 45)))
  ggplot2::ggplot(df, ggplot2::aes(x = source_label, y = tp, fill = cpp)) +
    ggplot2::geom_col(color = "#222222", linewidth = 0.18, width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_gradientn(colors = c("#4C78A8", "#F2CF5B", "#B22222"), name = "CPP") +
    ggplot2::scale_y_continuous("Total publications (TP)", labels = scales::label_number(big.mark = ","), expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::labs(title = "Most Productive Sources", subtitle = "Bar length = TP; color = citations per paper.", x = NULL) +
    ieee_theme_wide(base_size = 8.5) +
    ggplot2::theme(legend.position = "bottom")
}

m4_plot_impact_bubble <- function(impact, clusters) {
  if (!is.data.frame(impact) || nrow(impact) == 0) return(NULL)
  df <- impact |> dplyr::left_join(clusters |> dplyr::select("source", "cluster"), by = "source")
  df <- df |> dplyr::arrange(dplyr::desc(.data$tp + .data$tc)) |> dplyr::slice(seq_len(min(25L, nrow(df))))
  df$cluster <- df$cluster %||% "unclustered"
  x_mid <- stats::median(df$tp, na.rm = TRUE)
  y_mid <- stats::median(df$tc, na.rm = TRUE)
  ggplot2::ggplot(df, ggplot2::aes(x = tp, y = tc)) +
    ggplot2::geom_vline(xintercept = x_mid, linetype = "dashed", linewidth = 0.3, color = "#555555") +
    ggplot2::geom_hline(yintercept = y_mid, linetype = "dashed", linewidth = 0.3, color = "#555555") +
    ggplot2::geom_point(ggplot2::aes(size = cpp, fill = cluster), shape = 21, color = "#222222", stroke = 0.25, alpha = 0.9) +
    ggplot2::geom_text(ggplot2::aes(label = substr(source, 1, 22)), size = 2.2, vjust = -0.75, check_overlap = TRUE) +
    ggplot2::scale_x_continuous("TP - total publications", labels = scales::label_number(big.mark = ","), expand = ggplot2::expansion(mult = c(0.03, 0.12))) +
    ggplot2::scale_y_continuous("TC - total citations", labels = scales::label_number(big.mark = ","), expand = ggplot2::expansion(mult = c(0.03, 0.12))) +
    ggplot2::scale_size_continuous(name = "CPP", range = c(2.2, 9)) +
    ggplot2::scale_fill_manual(values = get_ieee_palette(length(unique(df$cluster)), "primary"), name = "Cluster") +
    ggplot2::labs(title = "Source Impact-Productivity Positioning", subtitle = "Quadrants split by median TP and TC; bubble size = CPP.") +
    ieee_theme_wide(base_size = 8.5) +
    ggplot2::theme(legend.position = "bottom")
}

m4_plot_bradford_zones <- function(zones) {
  if (!is.data.frame(zones) || nrow(zones) == 0) return(NULL)
  df <- zones |> dplyr::mutate(source_rank = dplyr::row_number())
  ggplot2::ggplot(df, ggplot2::aes(x = source_rank, y = cumulative_share, color = bradford_zone)) +
    ggplot2::geom_line(linewidth = 0.65) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::geom_hline(yintercept = c(1/3, 2/3), linetype = "dotted", color = "#666666", linewidth = 0.3) +
    ggplot2::scale_y_continuous("Cumulative TP share", labels = scales::label_percent(accuracy = 1), limits = c(0, 1)) +
    ggplot2::scale_x_continuous("Source rank by productivity") +
    ggplot2::scale_color_manual(values = get_ieee_palette(length(unique(df$bradford_zone)), "primary"), name = "Bradford zone") +
    ggplot2::labs(title = "Bradford Source-Core Curve", subtitle = "Horizontal guides mark one-third and two-thirds of cumulative production.") +
    ieee_theme_wide(base_size = 8.5) +
    ggplot2::theme(legend.position = "bottom")
}

m4_plot_bradford_zone_impact <- function(zone_summary) {
  if (!is.data.frame(zone_summary) || nrow(zone_summary) == 0) return(NULL)
  ggplot2::ggplot(zone_summary, ggplot2::aes(x = bradford_zone, y = cpp, fill = bradford_zone)) +
    ggplot2::geom_col(color = "#222222", linewidth = 0.2, width = 0.65) +
    ggplot2::scale_fill_manual(values = get_ieee_palette(nrow(zone_summary), "primary")) +
    ggplot2::labs(title = "Citation Efficiency by Bradford Zone", subtitle = "CPP identifies whether core or peripheral venues carry stronger impact.", x = NULL, y = "Citations per paper (CPP)") +
    ieee_theme_wide(base_size = 8.5) +
    ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 15, hjust = 1))
}

m4_plot_growth_impact <- function(growth) {
  if (!is.data.frame(growth) || nrow(growth) == 0) return(NULL)
  df <- growth |> dplyr::arrange(dplyr::desc(.data$tp)) |> dplyr::slice(seq_len(min(25L, nrow(growth))))
  ggplot2::ggplot(df, ggplot2::aes(x = tp_slope, y = cpp)) +
    ggplot2::geom_hline(yintercept = stats::median(df$cpp, na.rm = TRUE), linetype = "dashed", color = "#666666", linewidth = 0.3) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "#666666", linewidth = 0.3) +
    ggplot2::geom_point(ggplot2::aes(size = tp, fill = tc_slope), shape = 21, color = "#222222", stroke = 0.25, alpha = 0.9) +
    ggplot2::geom_text(ggplot2::aes(label = substr(source, 1, 22)), size = 2.2, vjust = -0.75, check_overlap = TRUE) +
    ggplot2::scale_fill_gradientn(colors = c("#4C78A8", "#F2CF5B", "#B22222"), name = "TC slope") +
    ggplot2::scale_size_continuous(name = "TP", range = c(2.2, 8)) +
    ggplot2::labs(title = "Source Growth vs Citation Efficiency", subtitle = "Emerging high-impact venues appear in the upper-right region.", x = "TP slope (papers/year)", y = "CPP") +
    ieee_theme_wide(base_size = 8.5) +
    ggplot2::theme(legend.position = "bottom")
}

m4_plot_emerging_sources <- function(growth, config) {
  if (!is.data.frame(growth) || nrow(growth) == 0) return(NULL)
  top_n <- as.integer(config$top_n_sources %||% 15L)
  df <- growth |> dplyr::arrange(dplyr::desc(.data$tp_slope)) |> dplyr::slice(seq_len(min(top_n, nrow(growth))))
  df$source_label <- factor(substr(df$source, 1, 45), levels = rev(substr(df$source, 1, 45)))
  ggplot2::ggplot(df, ggplot2::aes(x = source_label, y = tp_slope, fill = cpp)) +
    ggplot2::geom_col(color = "#222222", linewidth = 0.18, width = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_gradientn(colors = c("#4C78A8", "#F2CF5B", "#B22222"), name = "CPP") +
    ggplot2::labs(title = "Emerging Sources by Publication Slope", subtitle = "Positive slope indicates venues gaining volume over time.", x = NULL, y = "TP slope (papers/year)") +
    ieee_theme_wide(base_size = 8.5) +
    ggplot2::theme(legend.position = "bottom")
}

m4_plot_kmeans_sources <- function(clusters) {
  if (!is.data.frame(clusters) || nrow(clusters) == 0) return(NULL)
  ggplot2::ggplot(clusters, ggplot2::aes(x = pc1, y = pc2, color = cluster)) +
    ggplot2::geom_hline(yintercept = 0, color = "#D0D0D0", linewidth = 0.25) +
    ggplot2::geom_vline(xintercept = 0, color = "#D0D0D0", linewidth = 0.25) +
    ggplot2::geom_point(ggplot2::aes(size = exp(tp) - 1), alpha = 0.88) +
    ggplot2::geom_text(ggplot2::aes(label = substr(source, 1, 22)), size = 2.2, vjust = -0.75, check_overlap = TRUE, color = "#222222") +
    ggplot2::stat_ellipse(linewidth = 0.35, linetype = "dashed", show.legend = FALSE) +
    ggplot2::scale_color_manual(values = get_ieee_palette(length(unique(clusters$cluster)), "primary"), name = "Cluster") +
    ggplot2::scale_size_continuous(name = "TP", range = c(2.2, 8), labels = scales::label_number(big.mark = ",")) +
    ggplot2::labs(title = "K-means Source Clusters", subtitle = "PCA projection of standardized source productivity, impact, efficiency, and growth features.", x = "PC1", y = "PC2") +
    ieee_theme_wide(base_size = 8.5) +
    ggplot2::theme(legend.position = "bottom")
}
