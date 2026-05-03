# ============================================================================
# m3_render_positioning.R - Country bubble, trajectory and cluster plots
# ============================================================================

render_m3_positioning <- function(result, config = biblio_config()) {
  if (!is.list(result) || !identical(result$status %||% "", "success")) {
    return(list(status = result$status %||% "stub", plots = list()))
  }
  table <- result$table %||% tibble::tibble()
  list(
    status = "success",
    plots = list(
      scp_mcp_bubble_trajectory = m3_plot_scp_mcp_bubble_trajectory(table, result$horizon %||% 3L, config),
      tc_tp_bubble_trajectory = m3_plot_tc_tp_bubble_trajectory(table, result$horizon %||% 3L, config),
      kmeans_positioning = m3_plot_positioning_kmeans(result$clusters %||% tibble::tibble(), table, config)
    )
  )
}

m3_positioning_plot_data <- function(table, top_n) {
  if (!is.data.frame(table) || nrow(table) == 0) return(tibble::tibble())
  table |>
    dplyr::arrange(dplyr::desc(.data$tp + .data$tc)) |>
    dplyr::slice(seq_len(min(top_n, nrow(table)))) |>
    dplyr::mutate(label = substr(trimws(.data$country), 1, 22))
}

m3_plot_scp_mcp_bubble_trajectory <- function(table, horizon, config) {
  top_n <- as.integer(config$top_n_countries %||% 20L)
  df <- m3_positioning_plot_data(table, top_n)
  if (!is.data.frame(df) || nrow(df) == 0) return(NULL)

  x_mid <- stats::median(df$scp, na.rm = TRUE)
  y_mid <- stats::median(df$mcp, na.rm = TRUE)
  max_x <- max(df$scp_future, df$scp, na.rm = TRUE)
  max_y <- max(df$mcp_future, df$mcp, na.rm = TRUE)

  ggplot2::ggplot(df, ggplot2::aes(x = scp, y = mcp)) +
    ggplot2::annotate("rect", xmin = x_mid, xmax = Inf, ymin = y_mid, ymax = Inf, fill = "#DDEBF7", alpha = 0.35) +
    ggplot2::annotate("rect", xmin = 0, xmax = x_mid, ymin = y_mid, ymax = Inf, fill = "#E2F0D9", alpha = 0.30) +
    ggplot2::annotate("rect", xmin = x_mid, xmax = Inf, ymin = 0, ymax = y_mid, fill = "#FFF2CC", alpha = 0.30) +
    ggplot2::annotate("rect", xmin = 0, xmax = x_mid, ymin = 0, ymax = y_mid, fill = "#F2F2F2", alpha = 0.45) +
    ggplot2::geom_vline(xintercept = x_mid, linewidth = 0.35, linetype = "dashed", color = "#555555") +
    ggplot2::geom_hline(yintercept = y_mid, linewidth = 0.35, linetype = "dashed", color = "#555555") +
    ggplot2::geom_segment(
      ggplot2::aes(xend = scp_future, yend = mcp_future),
      arrow = grid::arrow(length = grid::unit(0.08, "inches"), type = "closed"),
      linewidth = 0.35,
      color = "#444444",
      alpha = 0.75
    ) +
    ggplot2::geom_point(ggplot2::aes(size = tp, fill = mcp_ratio), shape = 21, color = "#222222", stroke = 0.25, alpha = 0.92) +
    ggplot2::geom_text(ggplot2::aes(label = label), size = 2.25, vjust = -0.85, check_overlap = TRUE) +
    ggplot2::annotate("text", x = max_x * 0.82, y = max_y * 0.95, label = "Dual-base leaders", size = 2.5, fontface = "bold") +
    ggplot2::annotate("text", x = max_x * 0.18, y = max_y * 0.95, label = "Collaboration connectors", size = 2.5, fontface = "bold") +
    ggplot2::annotate("text", x = max_x * 0.82, y = max_y * 0.08, label = "Domestic anchors", size = 2.5, fontface = "bold") +
    ggplot2::annotate("text", x = max_x * 0.18, y = max_y * 0.08, label = "Emerging/peripheral", size = 2.5, fontface = "bold") +
    ggplot2::scale_x_continuous("SCP - single-country publications", labels = scales::label_number(big.mark = ","), expand = ggplot2::expansion(mult = c(0.03, 0.12))) +
    ggplot2::scale_y_continuous("MCP - multi-country publications", labels = scales::label_number(big.mark = ","), expand = ggplot2::expansion(mult = c(0.03, 0.12))) +
    ggplot2::scale_size_continuous(name = "TP", range = c(2.5, 9), labels = scales::label_number(big.mark = ",")) +
    ggplot2::scale_fill_gradientn(name = "MCP share", colors = c("#4C78A8", "#F2CF5B", "#B22222"), labels = scales::label_percent(accuracy = 1)) +
    ggplot2::labs(
      title = "Country Positioning: SCP vs MCP with Forward Trajectories",
      subtitle = sprintf("Bubble size = total publications (TP); arrows project country movement over %d years from recent slopes.", horizon),
      caption = "Quadrants are split by sample medians: domestic anchors, collaboration connectors, dual-base leaders, and emerging/peripheral contributors."
    ) +
    ieee_theme_wide(base_size = 8.5) +
    ggplot2::theme(legend.position = "bottom")
}

m3_plot_tc_tp_bubble_trajectory <- function(table, horizon, config) {
  top_n <- as.integer(config$top_n_countries %||% 20L)
  df <- m3_positioning_plot_data(table, top_n)
  if (!is.data.frame(df) || nrow(df) == 0) return(NULL)

  x_mid <- stats::median(df$tp, na.rm = TRUE)
  y_mid <- stats::median(df$tc, na.rm = TRUE)
  max_x <- max(df$tp_future, df$tp, na.rm = TRUE)
  max_y <- max(df$tc_future, df$tc, na.rm = TRUE)

  ggplot2::ggplot(df, ggplot2::aes(x = tp, y = tc)) +
    ggplot2::annotate("rect", xmin = x_mid, xmax = Inf, ymin = y_mid, ymax = Inf, fill = "#DDEBF7", alpha = 0.35) +
    ggplot2::annotate("rect", xmin = 0, xmax = x_mid, ymin = y_mid, ymax = Inf, fill = "#E2F0D9", alpha = 0.30) +
    ggplot2::annotate("rect", xmin = x_mid, xmax = Inf, ymin = 0, ymax = y_mid, fill = "#FFF2CC", alpha = 0.30) +
    ggplot2::annotate("rect", xmin = 0, xmax = x_mid, ymin = 0, ymax = y_mid, fill = "#F2F2F2", alpha = 0.45) +
    ggplot2::geom_vline(xintercept = x_mid, linewidth = 0.35, linetype = "dashed", color = "#555555") +
    ggplot2::geom_hline(yintercept = y_mid, linewidth = 0.35, linetype = "dashed", color = "#555555") +
    ggplot2::geom_segment(
      ggplot2::aes(xend = tp_future, yend = tc_future),
      arrow = grid::arrow(length = grid::unit(0.08, "inches"), type = "closed"),
      linewidth = 0.35,
      color = "#444444",
      alpha = 0.75
    ) +
    ggplot2::geom_point(ggplot2::aes(size = mcp_ratio, fill = citations_per_paper), shape = 21, color = "#222222", stroke = 0.25, alpha = 0.92) +
    ggplot2::geom_text(ggplot2::aes(label = label), size = 2.25, vjust = -0.85, check_overlap = TRUE) +
    ggplot2::annotate("text", x = max_x * 0.82, y = max_y * 0.95, label = "Volume-impact leaders", size = 2.5, fontface = "bold") +
    ggplot2::annotate("text", x = max_x * 0.18, y = max_y * 0.95, label = "Selective high impact", size = 2.5, fontface = "bold") +
    ggplot2::annotate("text", x = max_x * 0.82, y = max_y * 0.08, label = "Volume-heavy, lower impact", size = 2.5, fontface = "bold") +
    ggplot2::annotate("text", x = max_x * 0.18, y = max_y * 0.08, label = "Emerging/peripheral", size = 2.5, fontface = "bold") +
    ggplot2::scale_x_continuous("TP - total publications", labels = scales::label_number(big.mark = ","), expand = ggplot2::expansion(mult = c(0.03, 0.12))) +
    ggplot2::scale_y_continuous("TC - total citations", labels = scales::label_number(big.mark = ","), expand = ggplot2::expansion(mult = c(0.03, 0.12))) +
    ggplot2::scale_size_continuous(name = "MCP share", range = c(2.5, 9), labels = scales::label_percent(accuracy = 1)) +
    ggplot2::scale_fill_gradientn(name = "TC/TP", colors = c("#4C78A8", "#F2CF5B", "#B22222")) +
    ggplot2::labs(
      title = "Country Positioning: TC vs TP with Forward Trajectories",
      subtitle = sprintf("Bubble size = MCP share; arrows project impact-output movement over %d years.", horizon),
      caption = "Quadrants are split by sample medians: volume-impact leaders, selective high-impact countries, volume-heavy lower-impact countries, and emerging/peripheral contributors."
    ) +
    ieee_theme_wide(base_size = 8.5) +
    ggplot2::theme(legend.position = "bottom")
}

m3_plot_positioning_kmeans <- function(clusters, table, config) {
  if (!is.data.frame(clusters) || nrow(clusters) == 0) return(NULL)
  plot_df <- clusters |>
    dplyr::left_join(table |> dplyr::select("country", "tp", "tc", "mcp_ratio"), by = "country") |>
    dplyr::mutate(label = substr(trimws(.data$country), 1, 22))

  ggplot2::ggplot(plot_df, ggplot2::aes(x = pc1, y = pc2, color = cluster)) +
    ggplot2::geom_hline(yintercept = 0, color = "#D0D0D0", linewidth = 0.25) +
    ggplot2::geom_vline(xintercept = 0, color = "#D0D0D0", linewidth = 0.25) +
    ggplot2::geom_point(ggplot2::aes(size = tp), alpha = 0.88) +
    ggplot2::geom_text(ggplot2::aes(label = label), size = 2.25, vjust = -0.75, check_overlap = TRUE, color = "#222222") +
    ggplot2::stat_ellipse(linewidth = 0.35, linetype = "dashed", show.legend = FALSE) +
    ggplot2::scale_color_manual(values = get_ieee_palette(length(unique(plot_df$cluster)), "primary")) +
    ggplot2::scale_size_continuous(name = "TP", range = c(2.2, 8), labels = scales::label_number(big.mark = ",")) +
    ggplot2::labs(
      title = "K-means Country Clusters from Production, Impact, and Collaboration Features",
      subtitle = "PCA projection of standardized log(TP), log(TC), log(SCP), log(MCP), MCP share, and TC/TP.",
      x = "PC1 - dominant positioning contrast",
      y = "PC2 - secondary positioning contrast",
      color = "Cluster"
    ) +
    ieee_theme_wide(base_size = 8.5) +
    ggplot2::theme(legend.position = "bottom")
}
