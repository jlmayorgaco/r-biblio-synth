# ============================================================================
# m1_render_semantic_advanced.R - IEEE plots for M1 semantic advanced layer
# ============================================================================

render_m1_semantic_advanced <- function(result, config = biblio_config()) {
  if (!is.list(result) || !identical(result$status %||% "", "success")) {
    return(list(status = result$status %||% "stub", plots = list(), reason = result$reason %||% "Semantic advanced layer unavailable."))
  }
  plots <- list()
  thematic_map <- result$thematic_map %||% tibble::tibble()
  if (is.data.frame(thematic_map) && nrow(thematic_map) > 0) {
    plots$thematic_map <- m1_plot_thematic_map(thematic_map, config)
  }
  evolution <- result$thematic_evolution %||% tibble::tibble()
  if (is.data.frame(evolution) && nrow(evolution) > 0) {
    plots$thematic_evolution <- m1_plot_thematic_evolution(evolution, config)
  }
  stability <- result$topic_stability %||% tibble::tibble()
  if (is.data.frame(stability) && nrow(stability) > 0) {
    plots$topic_stability <- m1_plot_topic_stability(stability, config)
  }
  list(status = if (length(plots) > 0) "success" else "stub", plots = plots)
}

m1_plot_thematic_map <- function(thematic_map, config) {
  df <- thematic_map |>
    dplyr::filter(is.finite(.data$centrality), is.finite(.data$density)) |>
    dplyr::arrange(dplyr::desc(.data$documents_proxy)) |>
    utils::head(20)
  if (nrow(df) == 0) {
    return(ieee_no_data_plot("Thematic map unavailable", "No finite semantic centrality/density metrics were available.", layout = "full"))
  }
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$centrality, y = .data$density, size = .data$documents_proxy, fill = .data$quadrant)) +
    ggplot2::geom_vline(xintercept = stats::median(df$centrality, na.rm = TRUE), linetype = "dashed", color = "#777777", linewidth = 0.35) +
    ggplot2::geom_hline(yintercept = stats::median(df$density, na.rm = TRUE), linetype = "dashed", color = "#777777", linewidth = 0.35) +
    ggplot2::geom_point(shape = 21, color = "#111111", alpha = 0.85) +
    ggplot2::geom_text(ggplot2::aes(label = .data$label), size = 2.25, vjust = -0.75, check_overlap = TRUE) +
    ggplot2::scale_fill_manual(values = get_ieee_palette(length(unique(df$quadrant)))) +
    ggplot2::scale_size_area(max_size = 7) +
    ggplot2::labs(
      title = "Thematic Map",
      subtitle = "Co-word centrality and density classify motor, basic, niche, and emerging themes.",
      x = "Centrality",
      y = "Density",
      fill = "Quadrant",
      size = "Documents proxy"
    ) +
    ieee_theme_wide(base_size = 8.5)
  ieee_mark_plot_layout(p, "full")
}

m1_plot_thematic_evolution <- function(evolution, config) {
  df <- evolution |>
    dplyr::filter(.data$rank <= 12, is.finite(.data$share)) |>
    dplyr::mutate(keyword = stats::reorder(.data$keyword, .data$share, FUN = max))
  if (nrow(df) == 0) {
    return(ieee_no_data_plot("Thematic evolution unavailable", "No finite keyword-period shares were available.", layout = "full"))
  }
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$period, y = .data$keyword, fill = .data$share)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.3) +
    ggplot2::scale_fill_gradient(low = "#F7FBFF", high = "#084594", labels = scales::percent_format(accuracy = 0.1)) +
    ggplot2::labs(
      title = "Thematic Evolution",
      subtitle = "Keyword prominence across early, middle, and late review windows.",
      x = "Period",
      y = NULL,
      fill = "Share"
    ) +
    ieee_theme_wide(base_size = 8.5)
  ieee_mark_plot_layout(p, "full")
}

m1_plot_topic_stability <- function(stability, config) {
  df <- stability |>
    dplyr::filter(is.finite(.data$stability), is.finite(.data$mean_share)) |>
    dplyr::arrange(dplyr::desc(.data$mean_share)) |>
    utils::head(20)
  if (nrow(df) == 0) {
    return(ieee_no_data_plot("Topic stability unavailable", "No finite stability metrics were available.", layout = "single"))
  }
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$stability, y = stats::reorder(as.character(.data$topic), .data$stability))) +
    ggplot2::geom_col(fill = "#0072BD", width = 0.65) +
    ggplot2::labs(
      title = "Topic Stability",
      subtitle = "Stable topics persist across temporal windows; low values flag volatile terms.",
      x = "Stability score",
      y = NULL
    ) +
    ieee_theme(base_size = 8.5)
  ieee_mark_plot_layout(p, "single")
}
