# ============================================================================
# m4_render_similarity_lifecycle.R - Similarity, specialization and lifecycle plots
# ============================================================================

render_m4_similarity <- function(result, config = biblio_config()) {
  list(status = result$status %||% "stub", plots = list(
    keyword_similarity_heatmap = m4_plot_similarity_heatmap(result$pairwise %||% tibble::tibble(), config),
    keyword_similarity_network = m4_plot_similarity_network(result$network %||% tibble::tibble(), config)
  ))
}

render_m4_specialization <- function(result, config = biblio_config()) {
  list(status = result$status %||% "stub", plots = list(
    specialist_generalist_map = m4_plot_specialist_generalist(result$specialization %||% tibble::tibble(), config)
  ))
}

render_m4_lifecycle <- function(result, config = biblio_config()) {
  list(status = result$status %||% "stub", plots = list(
    lifecycle_trajectory = m4_plot_lifecycle_trajectory(result$lifecycle %||% tibble::tibble(), result$horizon %||% 3L, config),
    lifecycle_stage_bar = m4_plot_lifecycle_stage_bar(result$lifecycle %||% tibble::tibble(), config)
  ))
}

m4_plot_similarity_heatmap <- function(pairwise, config) {
  if (!is.data.frame(pairwise) || nrow(pairwise) == 0) return(NULL)
  top_n <- as.integer(config$top_n_sources %||% 20L)
  sources <- unique(c(pairwise$source_a, pairwise$source_b))
  sources <- utils::head(sources, top_n)
  df <- pairwise |>
    dplyr::filter(.data$source_a %in% sources, .data$source_b %in% sources)
  if (nrow(df) == 0) return(NULL)
  mirror <- df |>
    dplyr::transmute(source_a = .data$source_b, source_b = .data$source_a, similarity = .data$similarity)
  diag_df <- tibble::tibble(source_a = sources, source_b = sources, similarity = 1)
  plot_df <- dplyr::bind_rows(df, mirror, diag_df)
  ggplot2::ggplot(plot_df, ggplot2::aes(x = source_a, y = source_b, fill = similarity)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.2) +
    ggplot2::scale_fill_gradientn(colors = c("#F7FBFF", "#9ECAE1", "#2171B5", "#08306B"), limits = c(0, 1), name = "Cosine") +
    ggplot2::labs(title = "Keyword Similarity Between Sources", subtitle = "Cosine similarity from source-keyword profiles.", x = NULL, y = NULL) +
    ieee_theme_wide(base_size = 8.2) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1), legend.position = "bottom")
}

m4_plot_similarity_network <- function(network, config) {
  if (!is.data.frame(network) || nrow(network) == 0) return(NULL)
  nodes <- sort(unique(c(network$source_a, network$source_b)))
  angles <- seq(0, 2 * pi, length.out = length(nodes) + 1)[seq_along(nodes)]
  node_df <- tibble::tibble(source = nodes, x = cos(angles), y = sin(angles))
  edge_df <- network |>
    dplyr::left_join(node_df |> dplyr::rename(source_a = source, x_a = x, y_a = y), by = "source_a") |>
    dplyr::left_join(node_df |> dplyr::rename(source_b = source, x_b = x, y_b = y), by = "source_b")
  ggplot2::ggplot() +
    ggplot2::geom_segment(data = edge_df, ggplot2::aes(x = x_a, y = y_a, xend = x_b, yend = y_b, linewidth = similarity), color = "#4C78A8", alpha = 0.45) +
    ggplot2::geom_point(data = node_df, ggplot2::aes(x = x, y = y), size = 3.1, color = "#B22222") +
    ggplot2::geom_text(data = node_df, ggplot2::aes(x = x, y = y, label = substr(source, 1, 18)), size = 2.25, vjust = -0.9, check_overlap = TRUE) +
    ggplot2::scale_linewidth_continuous(range = c(0.2, 1.3), guide = "none") +
    ggplot2::coord_equal() +
    ggplot2::labs(title = "Source Similarity Network", subtitle = "Edges retain source pairs above the configured keyword-similarity threshold.", x = NULL, y = NULL) +
    ieee_theme_wide(base_size = 8.5) +
    ggplot2::theme(axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(), panel.grid = ggplot2::element_blank())
}

m4_plot_specialist_generalist <- function(specialization, config) {
  if (!is.data.frame(specialization) || nrow(specialization) == 0) return(NULL)
  df <- specialization |>
    dplyr::arrange(dplyr::desc(.data$tp + .data$tc)) |>
    dplyr::slice(seq_len(min(30L, nrow(specialization))))
  ggplot2::ggplot(df, ggplot2::aes(x = specialization_score, y = cpp, fill = venue_scope)) +
    ggplot2::geom_vline(xintercept = c(0.33, 0.66), linetype = "dotted", color = "#666666", linewidth = 0.3) +
    ggplot2::geom_point(ggplot2::aes(size = tp), shape = 21, color = "#222222", stroke = 0.25, alpha = 0.9) +
    ggplot2::geom_text(ggplot2::aes(label = substr(source, 1, 22)), size = 2.15, vjust = -0.75, check_overlap = TRUE) +
    ggplot2::scale_x_continuous("Specialization score", limits = c(0, 1), labels = scales::label_number(accuracy = 0.1)) +
    ggplot2::scale_size_continuous(name = "TP", range = c(2.2, 8)) +
    ggplot2::scale_fill_manual(values = get_ieee_palette(length(unique(df$venue_scope)), "primary"), name = "Scope") +
    ggplot2::labs(title = "Specialist-Generalist Source Map", subtitle = "Higher specialization means source output is concentrated in fewer keyword domains.", y = "CPP") +
    ieee_theme_wide(base_size = 8.5) +
    ggplot2::theme(legend.position = "bottom")
}

m4_plot_lifecycle_trajectory <- function(lifecycle, horizon, config) {
  if (!is.data.frame(lifecycle) || nrow(lifecycle) == 0) return(NULL)
  df <- lifecycle |>
    dplyr::arrange(dplyr::desc(.data$tp + .data$tc)) |>
    dplyr::slice(seq_len(min(30L, nrow(lifecycle))))
  ggplot2::ggplot(df, ggplot2::aes(x = tp, y = tc, fill = lifecycle_stage)) +
    ggplot2::geom_segment(
      ggplot2::aes(xend = tp_forecast, yend = tc_forecast),
      arrow = grid::arrow(length = grid::unit(0.08, "inches"), type = "closed"),
      color = "#444444",
      linewidth = 0.35,
      alpha = 0.75
    ) +
    ggplot2::geom_point(ggplot2::aes(size = cpp), shape = 21, color = "#222222", stroke = 0.25, alpha = 0.9) +
    ggplot2::geom_text(ggplot2::aes(label = substr(source, 1, 22)), size = 2.15, vjust = -0.8, check_overlap = TRUE) +
    ggplot2::scale_fill_manual(values = get_ieee_palette(length(unique(df$lifecycle_stage)), "primary"), name = "Lifecycle") +
    ggplot2::scale_size_continuous(name = "CPP", range = c(2.2, 8)) +
    ggplot2::scale_x_continuous("TP", labels = scales::label_number(big.mark = ","), expand = ggplot2::expansion(mult = c(0.03, 0.12))) +
    ggplot2::scale_y_continuous("TC", labels = scales::label_number(big.mark = ","), expand = ggplot2::expansion(mult = c(0.03, 0.12))) +
    ggplot2::labs(title = "Source Lifecycle Trajectories", subtitle = sprintf("Arrows project TP/TC movement over %d years from observed slopes.", horizon)) +
    ieee_theme_wide(base_size = 8.5) +
    ggplot2::theme(legend.position = "bottom")
}

m4_plot_lifecycle_stage_bar <- function(lifecycle, config) {
  if (!is.data.frame(lifecycle) || nrow(lifecycle) == 0) return(NULL)
  df <- lifecycle |>
    dplyr::count(.data$lifecycle_stage, name = "n")
  ggplot2::ggplot(df, ggplot2::aes(x = lifecycle_stage, y = n, fill = lifecycle_stage)) +
    ggplot2::geom_col(color = "#222222", linewidth = 0.18, width = 0.68) +
    ggplot2::scale_fill_manual(values = get_ieee_palette(nrow(df), "primary")) +
    ggplot2::labs(title = "Venue Lifecycle Composition", x = NULL, y = "Number of sources") +
    ieee_theme_wide(base_size = 8.5) +
    ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 20, hjust = 1))
}
