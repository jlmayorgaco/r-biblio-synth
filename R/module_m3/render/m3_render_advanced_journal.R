# ============================================================================
# m3_render_advanced_journal.R - Advanced M3 journal plots
# ============================================================================

render_m3_advanced_journal <- function(result, config = biblio_config()) {
  if (!is.list(result) || !identical(result$status %||% "", "success")) {
    return(list(status = result$status %||% "stub", plots = list(), reason = result$reason %||% "Advanced M3 analytics unavailable."))
  }

  plots <- list()

  premium <- result$collaboration_premium$table %||% tibble::tibble()
  if (is.data.frame(premium) && nrow(premium) > 0) {
    plots$collaboration_premium_forest <- m3_plot_collaboration_premium(premium, config)
  }

  mobility <- result$mobility$rank_windows %||% tibble::tibble()
  if (is.data.frame(mobility) && nrow(mobility) > 0) {
    plots$rank_mobility_bump <- m3_plot_rank_mobility(mobility, config)
  }

  trajectories <- result$trajectories$table %||% tibble::tibble()
  if (is.data.frame(trajectories) && nrow(trajectories) > 0) {
    plots$emerging_declining_bubbles <- m3_plot_country_trajectories(trajectories, config)
    plots$country_trajectory_heatmap <- m3_plot_trajectory_heatmap(trajectories, config)
    plots$quadrant_productivity_impact <- m3_plot_quadrant_productivity_impact(trajectories, config)
    plots$quadrant_growth_impact <- m3_plot_quadrant_growth_impact(trajectories, config)
  }

  concentration <- result$geo_concentration
  if (is.list(concentration) && is.data.frame(concentration$lorenz) && nrow(concentration$lorenz) > 0) {
    plots$concentration_dashboard <- m3_plot_concentration_dashboard(concentration, config)
  }

  regional <- result$regional_decomposition
  if (is.list(regional) && is.data.frame(regional$region_year) && nrow(regional$region_year) > 0) {
    plots$regional_decomposition_area <- m3_plot_regional_decomposition(regional$region_year, config)
  }
  if (is.list(regional) && is.data.frame(regional$table) && nrow(regional$table) > 0) {
    plots$quadrant_collaboration_impact <- m3_plot_quadrant_collaboration_impact(regional$table, config)
  }

  country_metrics <- result$country_metrics
  if (is.list(country_metrics) && is.data.frame(country_metrics$contribution) && nrow(country_metrics$contribution) > 0) {
    plots$country_contribution_decomposition <- m3_plot_country_contribution_decomposition(country_metrics$contribution, config)
  }
  if (is.list(country_metrics) && is.data.frame(country_metrics$country_year) && nrow(country_metrics$country_year) > 0) {
    plots$temporal_country_heatmap <- m3_plot_temporal_country_heatmap(country_metrics$country_year, config)
  }
  if (is.list(country_metrics) && is.data.frame(country_metrics$country_table) && nrow(country_metrics$country_table) > 0) {
    plots$impact_collaboration_quadrant <- m3_plot_impact_collaboration_quadrant(country_metrics$country_table, config)
    plots$normalized_impact <- m3_plot_normalized_impact(country_metrics$country_table, config)
  }
  if (is.list(country_metrics) && is.data.frame(country_metrics$world_map_metrics) && nrow(country_metrics$world_map_metrics) > 0) {
    maps <- m3_plot_publication_world_maps(country_metrics$world_map_metrics, config)
    if (length(maps) > 0) plots$publication_world_maps <- maps
  }

  network <- result$collaboration_network
  if (is.list(network) && is.data.frame(network$edges) && nrow(network$edges) > 0) {
    plots$country_collaboration_network <- m3_plot_country_collaboration_network(network, config)
  }

  scp_mcp <- result$scp_mcp_trends
  if (is.list(scp_mcp) && is.data.frame(scp_mcp$annual) && nrow(scp_mcp$annual) > 0) {
    plots$scp_mcp_trend <- m3_plot_scp_mcp_trend(scp_mcp, config)
  }

  uncertainty <- result$uncertainty
  if (is.list(uncertainty) && is.data.frame(uncertainty$intervals) && nrow(uncertainty$intervals) > 0) {
    plots$uncertainty_intervals <- m3_plot_uncertainty_intervals(uncertainty$intervals, config)
  }

  robustness <- result$robustness
  if (is.list(robustness) && is.data.frame(robustness$quadrant_robustness) && nrow(robustness$quadrant_robustness) > 0) {
    plots$robustness_dashboard <- m3_plot_robustness_dashboard(robustness, config)
  }

  inequality <- result$inequality_decomposition
  if (is.list(inequality) && is.data.frame(inequality$window_metrics) && nrow(inequality$window_metrics) > 0) {
    plots$inequality_decomposition_dashboard <- m3_plot_inequality_decomposition(inequality$window_metrics, config)
  }

  roles <- result$country_roles$table %||% tibble::tibble()
  if (is.data.frame(roles) && nrow(roles) > 0) {
    plots$country_role_taxonomy <- m3_plot_country_roles(roles, config)
  }

  spatial <- result$spatial_autocorrelation
  if (is.list(spatial) && is.data.frame(spatial$lisa) && nrow(spatial$lisa) > 0) {
    plots$spatial_autocorrelation_lisa <- m3_plot_lisa_clusters(spatial, config)
  }

  backbone <- result$collaboration_backbone
  if (is.list(backbone) && is.data.frame(backbone$edges) && nrow(backbone$edges) > 0) {
    plots$collaboration_backbone <- m3_plot_collaboration_backbone(backbone, config)
  }

  list(status = if (length(plots) > 0) "success" else "stub", plots = plots)
}

m3_plot_collaboration_premium <- function(premium, config) {
  df <- premium |>
    dplyr::filter(is.finite(.data$mean_difference), is.finite(.data$ci_low), is.finite(.data$ci_high))
  if (nrow(df) == 0) {
    return(m3_placeholder_plot("Collaboration premium unavailable", "No finite SCP/MCP citation premium interval was available.", layout = "full"))
  }
  if ("comparison" %in% names(df)) {
    df$comparison <- gsub("_", " ", as.character(df$comparison), fixed = TRUE)
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(y = .data$comparison, x = .data$mean_difference)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "#666666", linewidth = 0.4) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$ci_low, xmax = .data$ci_high), height = 0.18, linewidth = 0.45, color = "#111111") +
    ggplot2::geom_point(size = 2.4, color = "#0072BD") +
    ggplot2::labs(
      title = "Collaboration Premium",
      subtitle = "MCP versus SCP citation difference with bootstrap interval.",
      x = "Citation premium (MCP - SCP)",
      y = NULL,
      caption = "Positive values indicate higher impact for internationally collaborative documents."
    ) +
    ieee_theme_wide(base_size = 7.8) +
    ggplot2::theme(plot.caption = ggplot2::element_text(size = 5.8, color = "#555555", hjust = 0))
  ieee_mark_plot_layout(p, "full")
}

m3_plot_country_roles <- function(roles, config) {
  df <- roles |>
    dplyr::count(.data$role_label, name = "countries") |>
    dplyr::arrange(dplyr::desc(.data$countries))
  if (nrow(df) == 0) {
    return(m3_placeholder_plot("Country roles unavailable", "No country role labels were generated.", layout = "single"))
  }
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$countries, y = stats::reorder(.data$role_label, .data$countries), fill = .data$role_label)) +
    ggplot2::geom_col(width = 0.65, show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = get_ieee_palette(length(unique(df$role_label)))) +
    ggplot2::labs(
      title = "Country Role Taxonomy",
      subtitle = "Automated role labels summarize output, impact, collaboration, and trend position.",
      x = "Countries",
      y = NULL
    ) +
    ieee_theme(base_size = 8.5)
  ieee_mark_plot_layout(p, "single")
}

m3_plot_lisa_clusters <- function(spatial, config) {
  lisa <- spatial$lisa
  global <- spatial$global %||% tibble::tibble()
  subtitle <- if (is.data.frame(global) && nrow(global) > 0) {
    sprintf("Moran's I %.3f, p=%.3f (%s).", global$morans_i[1], global$p_value[1], global$method[1])
  } else {
    "Local spatial/proxy clusters."
  }
  df <- lisa |>
    dplyr::arrange(dplyr::desc(abs(.data$local_i))) |>
    utils::head(25)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$local_i, y = stats::reorder(.data$country, .data$local_i), fill = .data$lisa_cluster)) +
    ggplot2::geom_vline(xintercept = 0, color = "#333333", linewidth = 0.35) +
    ggplot2::geom_col(width = 0.65) +
    ggplot2::scale_fill_manual(values = get_ieee_palette(length(unique(df$lisa_cluster)))) +
    ggplot2::labs(
      title = "Spatial Autocorrelation Screen",
      subtitle = subtitle,
      x = "Local I proxy",
      y = NULL,
      fill = "Cluster"
    ) +
    ieee_theme_wide(base_size = 8.5)
  ieee_mark_plot_layout(p, "full")
}

m3_plot_collaboration_backbone <- function(backbone, config) {
  edges <- backbone$edges
  if (!is.data.frame(edges) || nrow(edges) == 0) {
    return(m3_placeholder_plot("Collaboration backbone unavailable", "No backbone edges were selected.", layout = "full"))
  }
  df <- edges |>
    dplyr::arrange(dplyr::desc(.data$weight)) |>
    utils::head(25) |>
    dplyr::mutate(pair = paste(.data$from, .data$to, sep = " - "))
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$weight, y = stats::reorder(.data$pair, .data$weight))) +
    ggplot2::geom_col(fill = "#0072BD", width = 0.65) +
    ggplot2::labs(
      title = "Country Collaboration Backbone",
      subtitle = "Strongest international co-authorship country pairs retained after backbone filtering.",
      x = "Co-authored documents",
      y = NULL
    ) +
    ieee_theme_wide(base_size = 8.5)
  ieee_mark_plot_layout(p, "full")
}

m3_plot_rank_mobility <- function(mobility, config) {
  top_n <- min(15, nrow(mobility))
  df <- mobility |>
    dplyr::arrange(.data$rank_last) |>
    utils::head(top_n) |>
    dplyr::select("country", "rank_first", "rank_last") |>
    tidyr::pivot_longer(c("rank_first", "rank_last"), names_to = "window", values_to = "rank") |>
    dplyr::mutate(
      window = ifelse(.data$window == "rank_first", "Early window", "Late window"),
      country_label = m3_plot_label_country(.data$country)
    )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$window, y = .data$rank, group = .data$country)) +
    ggplot2::geom_line(color = "#777777", linewidth = 0.45) +
    ggplot2::geom_point(ggplot2::aes(color = .data$country), size = 1.8, show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = .data$country_label), size = 2.3, hjust = -0.05, check_overlap = TRUE) +
    ggplot2::scale_y_reverse() +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(
      title = "Country Rank Mobility",
      subtitle = "Early-versus-late production rank movement for the leading countries.",
      x = NULL,
      y = "Production rank",
      caption = "Upward movement indicates improved relative position in the late window."
    ) +
    ieee_theme_wide(base_size = 8.5) +
    ggplot2::theme(plot.margin = ggplot2::margin(8, 40, 8, 8))
  ieee_mark_plot_layout(p, "full")
}

m3_plot_country_trajectories <- function(trajectories, config) {
  df <- trajectories |>
    dplyr::filter(is.finite(.data$share_change), is.finite(.data$share_last)) |>
    dplyr::arrange(dplyr::desc(abs(.data$share_change))) |>
    utils::head(25) |>
    dplyr::mutate(country_label = m3_plot_label_country(.data$country))
  if (nrow(df) == 0) {
    return(m3_placeholder_plot("Country trajectories unavailable", "No finite country share changes were available.", layout = "full"))
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$share_change, y = .data$share_last, size = abs(.data$rank_change), fill = .data$trajectory_class)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "#666666", linewidth = 0.35) +
    ggplot2::geom_point(shape = 21, color = "#111111", alpha = 0.82) +
    ggplot2::geom_text(ggplot2::aes(label = .data$country_label), size = 2.3, vjust = -0.75, check_overlap = TRUE) +
    ggplot2::scale_fill_manual(values = c(emerging = "#77AC30", declining = "#A2142F", stable = "#BDBDBD", volatile = "#EDB120", late_entry = "#0072BD"), drop = FALSE) +
    ggplot2::scale_size_area(max_size = 8, name = "Rank shift") +
    ggplot2::labs(
      title = "Emerging and Declining Country Trajectories",
      subtitle = "Share movement, late-window share, and rank mobility classify country trajectories.",
      x = "Production-share change",
      y = "Late-window production share",
      fill = "Trajectory",
      caption = "Labels are pruned automatically to protect readability."
    ) +
    ieee_theme_wide(base_size = 8.5)
  ieee_mark_plot_layout(p, "full")
}

m3_plot_concentration_dashboard <- function(concentration, config) {
  lorenz <- concentration$lorenz
  metrics <- concentration$table
  prod <- metrics[metrics$metric == "production", , drop = FALSE]
  subtitle <- if (nrow(prod) > 0) {
    sprintf(
      "Gini %.3f | HHI %.3f | Theil %.3f | Top-5 %.1f%%",
      prod$gini[1],
      prod$hhi[1],
      prod$theil[1],
      100 * prod$top5_share[1]
    )
  } else {
    "Lorenz curve for country production."
  }

  p <- ggplot2::ggplot(lorenz, ggplot2::aes(x = .data$cumulative_countries, y = .data$cumulative_production)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#777777", linewidth = 0.4) +
    ggplot2::geom_line(color = "#0072BD", linewidth = 0.9) +
    ggplot2::geom_area(fill = "#0072BD", alpha = 0.12) +
    ggplot2::coord_equal(clip = "off") +
    ggplot2::labs(
      title = "Geographic Concentration",
      subtitle = subtitle,
      x = "Countries (cumulative share)",
      y = "Production (cumulative share)",
      caption = "The diagonal gap summarizes concentration; tables report Gini, Theil, HHI, Palma, and top shares."
    ) +
    ieee_theme_wide(base_size = 7.6) +
    ggplot2::theme(plot.caption = ggplot2::element_text(size = 5.8, color = "#555555", hjust = 0))
  ieee_mark_plot_layout(p, "full")
}

m3_plot_regional_decomposition <- function(region_year, config) {
  df <- region_year |>
    dplyr::filter(is.finite(.data$year), is.finite(.data$share), !is.na(.data$region))
  if (nrow(df) == 0) {
    return(m3_placeholder_plot("Regional decomposition unavailable", "No finite regional year-share data were available.", layout = "full"))
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$year, y = .data$share, fill = .data$region)) +
    ggplot2::geom_area(alpha = 0.88, linewidth = 0.15, color = "white") +
    ggplot2::scale_fill_manual(values = get_ieee_palette(length(unique(df$region)))) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(
      title = "Regional Decomposition of Production Share",
      subtitle = "Temporal redistribution of country output across macro-regions.",
      x = "Year",
      y = "Production share",
      fill = "Region",
      caption = "Shares sum to one within each year after country-to-region mapping."
    ) +
    ieee_theme_wide(base_size = 8.5)
  ieee_mark_plot_layout(p, "full")
}

m3_plot_trajectory_heatmap <- function(trajectories, config) {
  df <- trajectories |>
    dplyr::arrange(dplyr::desc(abs(.data$share_change))) |>
    utils::head(30) |>
    dplyr::select("country", "share_first", "share_last") |>
    tidyr::pivot_longer(c("share_first", "share_last"), names_to = "window", values_to = "share") |>
    dplyr::mutate(
      window = ifelse(.data$window == "share_first", "Early share", "Late share"),
      country_label = m3_plot_label_country(.data$country)
    )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$window, y = stats::reorder(.data$country_label, .data$share), fill = .data$share)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.25) +
    ggplot2::scale_fill_gradient(low = "#F7FBFF", high = "#084594", labels = scales::percent_format(accuracy = 0.1)) +
    ggplot2::labs(
      title = "Country Trajectory Heatmap",
      subtitle = "Early and late production-share profiles for the most dynamic countries.",
      x = NULL,
      y = NULL,
      fill = "Share",
      caption = "Countries are selected by absolute share movement."
    ) +
    ieee_theme_wide(base_size = 8.5)
  ieee_mark_plot_layout(p, "full")
}

m3_plot_quadrant_productivity_impact <- function(trajectories, config) {
  df <- trajectories |>
    dplyr::filter(is.finite(.data$share_last), is.finite(.data$share_change)) |>
    dplyr::mutate(country_label = m3_plot_label_country(.data$country))
  if (!"articles_last" %in% names(df)) df$articles_last <- df$share_last
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$articles_last, y = .data$share_last)) +
    ggplot2::geom_vline(xintercept = stats::median(df$articles_last, na.rm = TRUE), linetype = "dashed", color = "#777777") +
    ggplot2::geom_hline(yintercept = stats::median(df$share_last, na.rm = TRUE), linetype = "dashed", color = "#777777") +
    ggplot2::geom_point(ggplot2::aes(fill = .data$trajectory_class, size = abs(.data$share_change)), shape = 21, color = "#111111", alpha = 0.82) +
    ggplot2::geom_text(ggplot2::aes(label = .data$country_label), size = 2.2, vjust = -0.7, check_overlap = TRUE) +
    ggplot2::scale_fill_manual(values = c(emerging = "#77AC30", declining = "#A2142F", stable = "#BDBDBD", volatile = "#EDB120", late_entry = "#0072BD"), drop = FALSE) +
    ggplot2::labs(
      title = "Productivity-Impact Quadrant",
      subtitle = "Late-window output and production share locate country roles.",
      x = "Late-window articles",
      y = "Late-window share",
      fill = "Trajectory",
      size = "Share shift"
    ) +
    ieee_theme_wide(base_size = 8.5)
  ieee_mark_plot_layout(p, "full")
}

m3_plot_quadrant_growth_impact <- function(trajectories, config) {
  df <- trajectories |>
    dplyr::filter(is.finite(.data$share_change), is.finite(.data$share_last)) |>
    dplyr::mutate(country_label = m3_plot_label_country(.data$country))
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$share_change, y = .data$share_last)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "#777777") +
    ggplot2::geom_hline(yintercept = stats::median(df$share_last, na.rm = TRUE), linetype = "dashed", color = "#777777") +
    ggplot2::geom_point(ggplot2::aes(fill = .data$trajectory_class, size = abs(.data$rank_change)), shape = 21, color = "#111111", alpha = 0.82) +
    ggplot2::geom_text(ggplot2::aes(label = .data$country_label), size = 2.2, vjust = -0.7, check_overlap = TRUE) +
    ggplot2::scale_fill_manual(values = c(emerging = "#77AC30", declining = "#A2142F", stable = "#BDBDBD", volatile = "#EDB120", late_entry = "#0072BD"), drop = FALSE) +
    ggplot2::labs(
      title = "Growth-Impact Quadrant",
      subtitle = "Share growth against late-window country impact.",
      x = "Production-share change",
      y = "Late-window share",
      fill = "Trajectory",
      size = "Rank shift"
    ) +
    ieee_theme_wide(base_size = 8.5)
  ieee_mark_plot_layout(p, "full")
}

m3_plot_quadrant_collaboration_impact <- function(regional, config) {
  df <- regional |>
    dplyr::filter(is.finite(.data$mcp_ratio), is.finite(.data$citations_per_article))
  if (nrow(df) == 0) {
    return(m3_placeholder_plot("Collaboration-impact quadrant unavailable", "Regional MCP ratio or impact data were unavailable.", layout = "full"))
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$mcp_ratio, y = .data$citations_per_article)) +
    ggplot2::geom_vline(xintercept = stats::median(df$mcp_ratio, na.rm = TRUE), linetype = "dashed", color = "#777777") +
    ggplot2::geom_hline(yintercept = stats::median(df$citations_per_article, na.rm = TRUE), linetype = "dashed", color = "#777777") +
    ggplot2::geom_point(ggplot2::aes(size = .data$article_count, fill = .data$region), shape = 21, color = "#111111", alpha = 0.82) +
    ggplot2::geom_text(ggplot2::aes(label = .data$region), size = 2.5, vjust = -0.75, check_overlap = TRUE) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::scale_fill_manual(values = get_ieee_palette(length(unique(df$region)))) +
    ggplot2::labs(
      title = "Collaboration-Impact Quadrant",
      subtitle = "Regional international collaboration against citation impact.",
      x = "MCP ratio",
      y = "Citations per article",
      fill = "Region",
      size = "Articles"
    ) +
    ieee_theme_wide(base_size = 8.5)
  ieee_mark_plot_layout(p, "full")
}

m3_plot_country_contribution_decomposition <- function(contribution, config) {
  df <- contribution |>
    dplyr::filter(is.finite(.data$share), .data$share >= 0) |>
    dplyr::group_by(.data$country) |>
    dplyr::mutate(country_total = sum(.data$share, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(.data$country_total)) |>
    dplyr::group_by(.data$country) |>
    dplyr::mutate(component_weight = ifelse(.data$country_total > 0, .data$share / .data$country_total, 0)) |>
    dplyr::ungroup()
  top_countries <- df |>
    dplyr::distinct(.data$country, .data$country_total) |>
    dplyr::arrange(dplyr::desc(.data$country_total)) |>
    utils::head(15) |>
    dplyr::pull("country")
  df <- df |>
    dplyr::filter(.data$country %in% top_countries) |>
    dplyr::mutate(country_label = m3_plot_label_country(.data$country))
  if (nrow(df) == 0) {
    return(m3_placeholder_plot("Country contribution unavailable", "Contribution components were empty.", layout = "full"))
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = stats::reorder(.data$country_label, .data$country_total), y = .data$component_weight, fill = .data$component)) +
    ggplot2::geom_col(width = 0.72, color = "white", linewidth = 0.15) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::scale_fill_manual(values = get_ieee_palette(length(unique(df$component)))) +
    ggplot2::labs(
      title = "Country Contribution Decomposition",
      subtitle = "Leading countries decomposed by production, citation, SCP, MCP, and fractional contribution profiles.",
      x = NULL,
      y = "Within-country contribution profile",
      fill = "Component",
      caption = "The stacked profile is normalized within country; absolute component shares are exported in the table."
    ) +
    ieee_theme_wide(base_size = 8.2)
  ieee_mark_plot_layout(p, "full")
}

m3_plot_temporal_country_heatmap <- function(country_year, config) {
  df <- country_year |>
    dplyr::filter(is.finite(.data$year), is.finite(.data$share), !is.na(.data$country))
  if (nrow(df) == 0) {
    return(m3_placeholder_plot("Temporal country heatmap unavailable", "No country-year shares were available.", layout = "full"))
  }
  top_countries <- df |>
    dplyr::group_by(.data$country) |>
    dplyr::summarise(total_articles = sum(.data$article_count, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$total_articles)) |>
    utils::head(25) |>
    dplyr::pull("country")
  df <- df |>
    dplyr::filter(.data$country %in% top_countries) |>
    dplyr::mutate(country_label = m3_plot_label_country(.data$country))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$year, y = stats::reorder(.data$country_label, .data$article_count, FUN = sum), fill = .data$share)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.12) +
    ggplot2::scale_fill_gradient(low = "#F7FBFF", high = "#08306B", labels = scales::percent_format(accuracy = 0.1)) +
    ggplot2::labs(
      title = "Temporal Country Heatmap",
      subtitle = "Country-by-year production share shows emergence, continuity, and decline.",
      x = "Year",
      y = NULL,
      fill = "Annual share",
      caption = "Countries are selected by total production; yearly shares sum to one across all observed countries."
    ) +
    ieee_theme_wide(base_size = 7.8)
  ieee_mark_plot_layout(p, "full")
}

m3_plot_country_collaboration_network <- function(network, config) {
  layout <- network$layout %||% tibble::tibble()
  edges <- network$edges %||% tibble::tibble()
  if (!is.data.frame(layout) || nrow(layout) == 0 || !is.data.frame(edges) || nrow(edges) == 0) {
    return(m3_placeholder_plot("Country collaboration network unavailable", "No international country-pair edges were available.", layout = "full"))
  }
  nodes <- layout |>
    dplyr::arrange(dplyr::desc(.data$degree_weight), dplyr::desc(.data$full_articles)) |>
    utils::head(28)
  edges <- edges |>
    dplyr::filter(.data$from %in% nodes$country, .data$to %in% nodes$country) |>
    dplyr::arrange(dplyr::desc(.data$weight)) |>
    utils::head(60) |>
    dplyr::left_join(nodes |> dplyr::select(country, x, y), by = c("from" = "country")) |>
    dplyr::rename(x_from = "x", y_from = "y") |>
    dplyr::left_join(nodes |> dplyr::select(country, x, y), by = c("to" = "country")) |>
    dplyr::rename(x_to = "x", y_to = "y")
  nodes <- nodes |> dplyr::mutate(country_label = m3_plot_label_country(.data$country))

  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = edges,
      ggplot2::aes(x = .data$x_from, y = .data$y_from, xend = .data$x_to, yend = .data$y_to, linewidth = .data$weight),
      color = "#6E7F91",
      alpha = 0.38,
      lineend = "round"
    ) +
    ggplot2::geom_point(
      data = nodes,
      ggplot2::aes(x = .data$x, y = .data$y, size = .data$degree_weight, fill = .data$mcp_ratio),
      shape = 21,
      color = "#111111",
      alpha = 0.90
    ) +
    ggplot2::geom_text(data = nodes, ggplot2::aes(x = .data$x, y = .data$y, label = .data$country_label), size = 2.1, vjust = -1.05, check_overlap = TRUE) +
    ggplot2::scale_linewidth(range = c(0.15, 1.4), guide = "none") +
    ggplot2::scale_size_area(max_size = 8, name = "Edge strength") +
    ggplot2::scale_fill_gradient(low = "#FEE8C8", high = "#D7301F", labels = scales::percent_format(accuracy = 1), name = "MCP ratio") +
    ggplot2::coord_equal(clip = "off") +
    ggplot2::labs(
      title = "Country Collaboration Network",
      subtitle = "International co-authorship edges weighted by shared MCP documents.",
      x = NULL,
      y = NULL,
      caption = "The network uses a deterministic circular layout to avoid runtime graph-layout dependencies."
    ) +
    ieee_theme_wide(base_size = 8.0) +
    ggplot2::theme(axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(), panel.grid = ggplot2::element_blank(), plot.margin = ggplot2::margin(8, 18, 8, 18))
  ieee_mark_plot_layout(p, "full")
}

m3_plot_scp_mcp_trend <- function(scp_mcp, config) {
  annual <- scp_mcp$annual %||% tibble::tibble()
  if (!is.data.frame(annual) || nrow(annual) == 0 || !"mcp_share" %in% names(annual)) {
    return(m3_placeholder_plot("SCP/MCP trend unavailable", "Annual SCP/MCP counts were unavailable.", layout = "full"))
  }
  mix <- annual |>
    dplyr::select("year", "SCP", "MCP") |>
    tidyr::pivot_longer(c("SCP", "MCP"), names_to = "collaboration_type", values_to = "documents") |>
    dplyr::group_by(.data$year) |>
    dplyr::mutate(share = .data$documents / max(sum(.data$documents, na.rm = TRUE), .Machine$double.eps)) |>
    dplyr::ungroup()
  trend <- scp_mcp$trend %||% tibble::tibble()
  subtitle <- if (is.data.frame(trend) && nrow(trend) > 0 && is.finite(trend$slope[1])) {
    sprintf("MCP-share slope %.4f per year; p = %.3f.", trend$slope[1], trend$p_value[1])
  } else {
    "Annual domestic versus international collaboration mix."
  }

  p <- ggplot2::ggplot(mix, ggplot2::aes(x = .data$year, y = .data$share, fill = .data$collaboration_type)) +
    ggplot2::geom_area(alpha = 0.88, color = "white", linewidth = 0.15) +
    ggplot2::geom_line(data = annual, ggplot2::aes(x = .data$year, y = .data$mcp_share), inherit.aes = FALSE, color = "#111111", linewidth = 0.65) +
    ggplot2::geom_point(data = annual, ggplot2::aes(x = .data$year, y = .data$mcp_share), inherit.aes = FALSE, color = "#111111", size = 1.2) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::scale_fill_manual(values = c(SCP = "#BDBDBD", MCP = "#0072BD")) +
    ggplot2::labs(
      title = "Domestic versus International Collaboration Trend",
      subtitle = subtitle,
      x = "Year",
      y = "Document share",
      fill = "Type",
      caption = "Black line tracks annual MCP share over the stacked SCP/MCP composition."
    ) +
    ieee_theme_wide(base_size = 8.3)
  ieee_mark_plot_layout(p, "full")
}

m3_plot_impact_collaboration_quadrant <- function(country_table, config) {
  df <- country_table |>
    dplyr::filter(is.finite(.data$mcp_ratio), is.finite(.data$age_normalized_impact), is.finite(.data$full_articles)) |>
    dplyr::arrange(dplyr::desc(.data$full_articles)) |>
    utils::head(30) |>
    dplyr::mutate(country_label = m3_plot_label_country(.data$country))
  if (nrow(df) == 0) {
    return(m3_placeholder_plot("Impact-collaboration quadrant unavailable", "Country MCP ratio or normalized impact was unavailable.", layout = "full"))
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$mcp_ratio, y = .data$age_normalized_impact)) +
    ggplot2::geom_vline(xintercept = stats::median(df$mcp_ratio, na.rm = TRUE), linetype = "dashed", color = "#777777") +
    ggplot2::geom_hline(yintercept = stats::median(df$age_normalized_impact, na.rm = TRUE), linetype = "dashed", color = "#777777") +
    ggplot2::geom_point(ggplot2::aes(size = .data$full_articles, fill = .data$production_share), shape = 21, color = "#111111", alpha = 0.86) +
    ggplot2::geom_text(ggplot2::aes(label = .data$country_label), size = 2.2, vjust = -0.75, check_overlap = TRUE) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::scale_fill_gradient(low = "#E5F5E0", high = "#238B45", labels = scales::percent_format(accuracy = 1), name = "Global share") +
    ggplot2::scale_size_area(max_size = 8, name = "Articles") +
    ggplot2::labs(
      title = "Impact-Collaboration Country Quadrant",
      subtitle = "Age-normalized impact against international collaboration intensity.",
      x = "MCP ratio",
      y = "Age-normalized citations per article",
      caption = "Median guides separate high/low collaboration and high/low normalized impact regimes."
    ) +
    ieee_theme_wide(base_size = 8.3)
  ieee_mark_plot_layout(p, "full")
}

m3_plot_normalized_impact <- function(country_table, config) {
  df <- country_table |>
    dplyr::filter(is.finite(.data$age_normalized_impact), .data$full_articles > 0) |>
    dplyr::arrange(dplyr::desc(.data$age_normalized_impact)) |>
    utils::head(20) |>
    dplyr::mutate(country_label = m3_plot_label_country(.data$country))
  if (nrow(df) == 0) {
    return(m3_placeholder_plot("Normalized impact unavailable", "Age-normalized citation impact could not be computed.", layout = "full"))
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = stats::reorder(.data$country_label, .data$age_normalized_impact), y = .data$age_normalized_impact, fill = .data$mcp_ratio)) +
    ggplot2::geom_col(width = 0.72, color = "#111111", linewidth = 0.12) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_gradient(low = "#FEE8C8", high = "#D7301F", labels = scales::percent_format(accuracy = 1), na.value = "#BDBDBD") +
    ggplot2::labs(
      title = "Age-Normalized Country Impact",
      subtitle = "Citation impact adjusted by publication age highlights durable influence rather than raw citation volume.",
      x = NULL,
      y = "Age-normalized citations per article",
      fill = "MCP ratio"
    ) +
    ieee_theme_wide(base_size = 8.3)
  ieee_mark_plot_layout(p, "full")
}

m3_plot_uncertainty_intervals <- function(intervals, config) {
  df <- intervals |>
    dplyr::filter(is.finite(.data$estimate), is.finite(.data$ci_low), is.finite(.data$ci_high)) |>
    dplyr::arrange(.data$metric, dplyr::desc(abs(.data$estimate))) |>
    dplyr::group_by(.data$metric) |>
    dplyr::slice_head(n = 8) |>
    dplyr::ungroup() |>
    dplyr::mutate(entity_label = m3_plot_label_country(.data$entity))
  if (nrow(df) == 0) {
    return(m3_placeholder_plot("Uncertainty intervals unavailable", "Bootstrap intervals were not finite.", layout = "full"))
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(y = stats::reorder(.data$entity_label, .data$estimate), x = .data$estimate, color = .data$metric)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "#777777", linewidth = 0.35) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$ci_low, xmax = .data$ci_high), height = 0.18, linewidth = 0.45) +
    ggplot2::geom_point(size = 2.0) +
    ggplot2::scale_color_manual(values = get_ieee_palette(length(unique(df$metric)))) +
    ggplot2::labs(
      title = "Country-Level Uncertainty Intervals",
      subtitle = "Bootstrap intervals for citation premium, share change, and top-country dominance.",
      x = "Estimate with 95% interval",
      y = NULL,
      color = "Metric",
      caption = "Intervals crossing zero are treated as inconclusive in the automatic hypothesis layer."
    ) +
    ieee_theme_wide(base_size = 8.0)
  ieee_mark_plot_layout(p, "full")
}

m3_plot_robustness_dashboard <- function(robustness, config) {
  quad <- robustness$quadrant_robustness %||% tibble::tibble()
  rank <- robustness$rank_sensitivity %||% tibble::tibble()
  if (!is.data.frame(quad) || nrow(quad) == 0) {
    return(m3_placeholder_plot("Robustness dashboard unavailable", "Full versus fractional rank comparison was unavailable.", layout = "full"))
  }
  df <- quad |>
    dplyr::filter(is.finite(.data$full_rank), is.finite(.data$fractional_rank)) |>
    dplyr::arrange(abs(.data$rank_sensitivity)) |>
    utils::head(30) |>
    dplyr::mutate(country_label = m3_plot_label_country(.data$country))
  rho <- rank[rank$method == "spearman", , drop = FALSE]
  subtitle <- if (nrow(rho) > 0 && is.finite(rho$estimate[1])) {
    sprintf("Full/fractional rank correlation rho = %.3f; mean absolute rank shift = %.2f.", rho$estimate[1], rho$mean_abs_rank_change[1])
  } else {
    "Sensitivity of country ranking and quadrant assignment to counting mode."
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$full_rank, y = .data$fractional_rank)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#777777", linewidth = 0.35) +
    ggplot2::geom_point(ggplot2::aes(fill = .data$quadrant_changed), shape = 21, color = "#111111", size = 2.4, alpha = 0.86) +
    ggplot2::geom_text(ggplot2::aes(label = .data$country_label), size = 2.1, vjust = -0.75, check_overlap = TRUE) +
    ggplot2::scale_x_reverse() +
    ggplot2::scale_y_reverse() +
    ggplot2::scale_fill_manual(values = c(`FALSE` = "#0072BD", `TRUE` = "#A2142F"), labels = c(`FALSE` = "Stable", `TRUE` = "Changed")) +
    ggplot2::labs(
      title = "Counting-Mode Robustness Dashboard",
      subtitle = subtitle,
      x = "Full-count rank",
      y = "Fractional-count rank",
      fill = "Quadrant",
      caption = "Points away from the diagonal indicate rank sensitivity; red points changed quadrant under normalization."
    ) +
    ieee_theme_wide(base_size = 8.1)
  ieee_mark_plot_layout(p, "full")
}

m3_plot_inequality_decomposition <- function(window_metrics, config) {
  df <- window_metrics |>
    dplyr::filter(.data$period %in% c("early", "mid", "late"), .data$metric %in% c("production", "citations")) |>
    tidyr::pivot_longer(c("gini", "hhi", "top5_share"), names_to = "index", values_to = "value") |>
    dplyr::filter(is.finite(.data$value))
  if (nrow(df) == 0) {
    return(m3_placeholder_plot("Inequality decomposition unavailable", "Windowed concentration metrics were not finite.", layout = "full"))
  }
  df$period <- factor(df$period, levels = c("early", "mid", "late"))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$period, y = .data$value, color = .data$index, group = .data$index)) +
    ggplot2::geom_line(linewidth = 0.65) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::facet_wrap(~metric, nrow = 1) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::scale_color_manual(values = get_ieee_palette(length(unique(df$index)))) +
    ggplot2::labs(
      title = "Windowed Inequality Decomposition",
      subtitle = "Gini, HHI, and top-5 share track geographic concentration over early, mid, and late windows.",
      x = "Temporal window",
      y = "Index value",
      color = "Index"
    ) +
    ieee_theme_wide(base_size = 8.1)
  ieee_mark_plot_layout(p, "full")
}

m3_plot_publication_world_maps <- function(world_map_metrics, config) {
  if (!requireNamespace("rnaturalearth", quietly = TRUE) ||
      !requireNamespace("rnaturalearthdata", quietly = TRUE) ||
      !requireNamespace("sf", quietly = TRUE)) {
    return(list())
  }

  specs <- list(
    production = list(column = "article_count", title = "World Map: Publication Output", fill = "Articles"),
    normalized_impact = list(column = "normalized_impact", title = "World Map: Age-Normalized Impact", fill = "Norm. impact"),
    mcp_ratio = list(column = "mcp_ratio", title = "World Map: International Collaboration", fill = "MCP ratio"),
    growth = list(column = "growth", title = "World Map: Country Growth", fill = "Growth")
  )

  plots <- list()
  for (nm in names(specs)) {
    spec <- specs[[nm]]
    p <- m3_plot_publication_world_map_metric(world_map_metrics, spec$column, spec$title, spec$fill, config)
    if (inherits(p, "ggplot")) plots[[nm]] <- p
  }
  plots
}

m3_plot_publication_world_map_metric <- function(world_map_metrics, metric_col, title, fill_label, config) {
  if (!metric_col %in% names(world_map_metrics)) return(NULL)
  world <- tryCatch(rnaturalearth::ne_countries(scale = "medium", returnclass = "sf"), error = function(e) NULL)
  if (is.null(world)) return(NULL)

  metrics <- world_map_metrics |>
    dplyr::mutate(country_norm = m3_plot_map_normalize(.data$country), fill_value = suppressWarnings(as.numeric(.data[[metric_col]]))) |>
    dplyr::filter(is.finite(.data$fill_value))
  if (nrow(metrics) == 0) return(NULL)

  world$name_norm <- m3_plot_map_normalize(world$name)
  world_data <- dplyr::left_join(world, metrics, by = c("name_norm" = "country_norm"))
  covered <- sum(is.finite(world_data$fill_value), na.rm = TRUE)
  if (covered == 0) return(NULL)

  p <- ggplot2::ggplot(world_data) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$fill_value), color = "white", linewidth = 0.08) +
    ggplot2::scale_fill_gradient(low = "#F7FBFF", high = "#08306B", na.value = "#F2F2F2") +
    ggplot2::labs(
      title = title,
      subtitle = sprintf("Countries matched: %d. Optional map rendered only when geospatial dependencies are available.", covered),
      fill = fill_label,
      caption = "Country names are matched after normalization; unmatched countries remain light gray."
    ) +
    ieee_theme_wide(base_size = 7.6) +
    ggplot2::theme(axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(), panel.grid = ggplot2::element_blank())
  ieee_mark_plot_layout(p, "full")
}

m3_plot_label_country <- function(country) {
  x <- as.character(country)
  x <- gsub("_", " ", x, fixed = TRUE)
  out <- tools::toTitleCase(tolower(x))
  out <- gsub("\\bMcp\\b", "MCP", out)
  out <- gsub("\\bScp\\b", "SCP", out)
  out <- gsub("\\bVs\\b", "vs", out)
  out
}

m3_plot_map_normalize <- function(country) {
  x <- toupper(trimws(as.character(country)))
  x <- gsub("[^A-Z ]", " ", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  x <- gsub("^UNITED STATES OF AMERICA$", "UNITED STATES", x)
  x <- gsub("^RUSSIAN FEDERATION$", "RUSSIA", x)
  x <- gsub("^KOREA REPUBLIC OF$", "SOUTH KOREA", x)
  x <- gsub("^REPUBLIC OF KOREA$", "SOUTH KOREA", x)
  x <- gsub("^IRAN ISLAMIC REPUBLIC OF$", "IRAN", x)
  x <- gsub("^VIET NAM$", "VIETNAM", x)
  x
}
