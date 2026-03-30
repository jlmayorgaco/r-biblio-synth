# ============================================================================
# m3_render_scp_mcp.R - Enhanced SCP/MCP Plots (IEEE Q1 Legacy Compatible)
# ============================================================================
# Creates legacy-style plots:
# - M3_p1_mcp_share_vs_year.png - MCP share over time
# - Stacked bar chart for SCP/MCP by country
# - MCP ratio ranking
# ============================================================================

#' Render all M3 SCP/MCP collaboration plots (Enhanced IEEE Q1 Quality)
#'
#' @param scp_mcp_data Output from m3_compute_scp_mcp
#' @param config Configuration list
#' @return List with plots
#' @export
m3_render_scp_mcp <- function(scp_mcp_data, config = biblio_config()) {
  if (!is.list(scp_mcp_data) || scp_mcp_data$status == "stub") {
    return(list(status = "stub", plots = list()))
  }
  
  plots <- list()
  
  top_n <- config$top_n_countries %||% 10
  df <- scp_mcp_data$scp_mcp
  
  if (is.null(df) || nrow(df) == 0) {
    return(list(status = "no data", plots = list()))
  }
  
  # 1. Stacked bar: SCP + MCP by country
  plots$stacked_bar <- create_scp_mcp_stacked_plot(df, top_n, config)
  
  # 2. MCP ratio ranking
  plots$mcp_ratio <- create_mcp_ratio_plot(df, top_n, config)
  
  # 3. M3_p1_mcp_share_vs_year - MCP share over time (if annual data available)
  if ("annual_scp_mcp" %in% names(scp_mcp_data) && !is.null(scp_mcp_data$annual_scp_mcp)) {
    plots$mcp_share_vs_year <- create_mcp_share_vs_year_plot(scp_mcp_data$annual_scp_mcp, config)
  }
  
  # 4. SCP/MCP proportion pie chart
  if ("total" %in% names(scp_mcp_data)) {
    plots$scp_mcp_pie <- create_scp_mcp_pie_plot(scp_mcp_data$total, config)
  }
  
  # 5. Top collaborating countries (highest MCP)
  if ("top_collaborators" %in% names(scp_mcp_data)) {
    plots$top_collaborators <- create_top_collaborators_plot(scp_mcp_data$top_collaborators, config)
  }
  
  list(status = "success", plots = plots)
}

#' Create SCP/MCP stacked bar plot
#' @keywords internal
create_scp_mcp_stacked_plot <- function(df, top_n, config) {
  df_top <- df %>%
    dplyr::arrange(dplyr::desc(article_count)) %>%
    dplyr::slice(seq_len(min(top_n, nrow(.)))) %>%
    dplyr::mutate(label_clean = substr(trimws(country), 1, 25))
  
  df_long <- df_top %>%
    dplyr::select(label_clean, scp, mcp) %>%
    tidyr::pivot_longer(
      cols = c(scp, mcp),
      names_to = "type",
      values_to = "count"
    ) %>%
    dplyr::mutate(type = toupper(type))
  
  df_long$label_clean <- factor(df_long$label_clean, 
                                  levels = rev(unique(df_long$label_clean)))
  
  p <- ggplot2::ggplot(df_long, ggplot2::aes(x = label_clean, y = count, fill = type)) +
    ggplot2::geom_col(color = "black", linewidth = 0.2, width = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(
      values = c(SCP = "#0072BD", MCP = "#D95319"),
      name = "Publication Type",
      labels = c("Single Country", "Multi-Country")
    ) +
    ggplot2::scale_y_continuous(
      name = "Number of Publications",
      labels = scales::label_number(big.mark = ","),
      expand = ggplot2::expansion(mult = c(0, 0.1))
    ) +
    ggplot2::labs(
      title = "SCP vs MCP by Country",
      subtitle = sprintf("Top %d countries by publication volume", top_n),
      x = NULL
    ) +
    ieee_theme_bar() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      plot.title = ggplot2::element_text(size = 10, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5, face = "italic")
    )
  
  p
}

#' Create MCP ratio plot
#' @keywords internal
create_mcp_ratio_plot <- function(df, top_n, config) {
  df_ratio <- df %>%
    dplyr::filter(!is.na(mcp_ratio)) %>%
    dplyr::arrange(dplyr::desc(mcp_ratio)) %>%
    dplyr::slice(seq_len(min(top_n, nrow(.)))) %>%
    dplyr::mutate(
      label_clean = substr(trimws(country), 1, 25),
      mcp_pct = mcp_ratio * 100
    )
  
  if (nrow(df_ratio) == 0) return(NULL)
  
  df_ratio$label_clean <- factor(df_ratio$label_clean, 
                                   levels = rev(df_ratio$label_clean))
  
  p <- ggplot2::ggplot(df_ratio, ggplot2::aes(x = label_clean, y = mcp_pct)) +
    ggplot2::geom_col(fill = "#7E2F8E", color = "black", linewidth = 0.3, width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.1f%%", mcp_pct)),
      hjust = -0.1,
      size = 2.5,
      family = "mono"
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      name = "MCP Ratio (%)",
      limits = c(0, 115),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::labs(
      title = "International Collaboration Rate",
      subtitle = sprintf("Top %d countries by MCP ratio", top_n),
      x = NULL
    ) +
    ieee_theme_bar() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 7)
    )
  
  p
}

#' Create MCP share vs year plot (M3_p1_mcp_share_vs_year)
#' @keywords internal
create_mcp_share_vs_year_plot <- function(annual_data, config) {
  if (is.null(annual_data) || nrow(annual_data) == 0) return(NULL)
  
  year_col <- if ("year" %in% names(annual_data)) "year" else "Year"
  
  # Calculate MCP share if not present
  if (!"mcp_share" %in% names(annual_data) && "mcp" %in% names(annual_data) && "scp" %in% names(annual_data)) {
    annual_data$mcp_share <- annual_data$mcp / (annual_data$mcp + annual_data$scp) * 100
  }
  
  if (!"mcp_share" %in% names(annual_data) && "mcp_count" %in% names(annual_data)) {
    total_col <- if ("total" %in% names(annual_data)) "total" else "article_count"
    annual_data$mcp_share <- annual_data$mcp_count / annual_data[[total_col]] * 100
  }
  
  mcp_col <- if ("mcp_share" %in% names(annual_data)) "mcp_share" else "mcp_ratio"
  
  p <- ggplot2::ggplot(annual_data, ggplot2::aes_string(x = year_col, y = mcp_col)) +
    ggplot2::geom_line(color = "#7E2F8E", linewidth = 0.8) +
    ggplot2::geom_point(color = "#7E2F8E", size = 2, alpha = 0.8) +
    ggplot2::geom_smooth(
      method = "loess",
      se = TRUE,
      color = "#D95319",
      fill = "#D95319",
      alpha = 0.2,
      linewidth = 0.6
    ) +
    ggplot2::geom_hline(
      yintercept = 50,
      linetype = "dashed",
      color = "#666666",
      linewidth = 0.4
    ) +
    ggplot2::annotate(
      "text",
      x = min(annual_data[[year_col]], na.rm = TRUE),
      y = 52,
      label = "50% threshold",
      hjust = 0,
      size = 2.5,
      color = "#666666"
    ) +
    ggplot2::scale_x_continuous(
      name = "Year",
      breaks = scales::breaks_pretty(n = 8)
    ) +
    ggplot2::scale_y_continuous(
      name = "MCP Share (%)",
      limits = c(0, 100),
      labels = scales::label_number(accuracy = 1)
    ) +
    ieee_theme_timeseries() +
    ggplot2::labs(
      title = "International Collaboration Trend Over Time",
      subtitle = "Multi-Country Publication (MCP) share evolution"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 10, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5, face = "italic")
    )
  
  p
}

#' Create SCP/MCP pie chart
#' @keywords internal
create_scp_mcp_pie_plot <- function(total_data, config) {
  if (is.null(total_data)) return(NULL)
  
  scp <- total_data$scp %||% total_data$scp_count %||% 0
  mcp <- total_data$mcp %||% total_data$mcp_count %||% 0
  total <- scp + mcp
  
  if (total == 0) return(NULL)
  
  pie_df <- data.frame(
    type = c("SCP", "MCP"),
    count = c(scp, mcp),
    pct = c(scp, mcp) / total * 100,
    label = c(
      sprintf("SCP\n%d (%.1f%%)", scp, scp / total * 100),
      sprintf("MCP\n%d (%.1f%%)", mcp, mcp / total * 100)
    )
  )
  
  p <- ggplot2::ggplot(pie_df, ggplot2::aes(x = "", y = count, fill = type)) +
    ggplot2::geom_col(color = "black", linewidth = 0.3, width = 1) +
    ggplot2::coord_polar("y", start = 0) +
    ggplot2::geom_text(
      ggplot2::aes(label = label),
      position = ggplot2::position_stack(vjust = 0.5),
      size = 3,
      family = "mono",
      fontface = "bold"
    ) +
    ggplot2::scale_fill_manual(
      values = c(SCP = "#0072BD", MCP = "#D95319"),
      guide = "none"
    ) +
    ggplot2::labs(title = "Overall SCP vs MCP Distribution") +
    ieee_theme() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 10, face = "bold", hjust = 0.5)
    )
  
  p
}

#' Create top collaborators plot
#' @keywords internal
create_top_collaborators_plot <- function(top_collab, config) {
  if (is.null(top_collab) || nrow(top_collab) == 0) return(NULL)
  
  top_n <- config$top_n_countries %||% 10
  top <- head(top_collab[order(-top_collab$mcp_ratio), ], top_n)
  top$country <- substr(trimws(top$country), 1, 25)
  top$country <- factor(top$country, levels = rev(top$country))
  
  p <- ggplot2::ggplot(top, ggplot2::aes(x = country, y = mcp_ratio * 100)) +
    ggplot2::geom_col(fill = "#4DBEEE", color = "black", linewidth = 0.3, width = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      name = "MCP Ratio (%)",
      limits = c(0, 100)
    ) +
    ggplot2::labs(
      title = "Top Collaborating Countries",
      subtitle = "Countries with highest international collaboration rate",
      x = NULL
    ) +
    ieee_theme_bar()
  
  p
}

`%||%` <- function(a, b) if (!is.null(a)) a else b