# ============================================================================
# m1_render_countries.R - Countries Plots (IEEE REFACTORED)
# ============================================================================

#' @export
render_m1_countries <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"top_countries_by_articles" %in% names(result)) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  articles <- result$top_countries_by_articles
  if (nrow(articles) == 0 || !"label" %in% names(articles) || !"value" %in% names(articles)) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  plots <- list()
  articles_top <- articles[seq_len(min(10, nrow(articles))), ]

  # 1. Articles bar
  plots$articles_bar <- ggplot2::ggplot(articles_top, ggplot2::aes(x = reorder(label, value), y = value)) +
    ggplot2::geom_col(fill = ieee_colors$blue, color = "black", linewidth = 0.2) +
    ggplot2::geom_text(ggplot2::aes(label = value), hjust = -0.1, size = 2.0) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.10))) +
    ggplot2::labs(x = NULL, y = "Publications") +
    ieee_theme()

  # 2. Citations bar
  if ("top_countries_by_citations" %in% names(result) && nrow(result$top_countries_by_citations) > 0) {
    cit <- result$top_countries_by_citations[1:min(10, nrow(result$top_countries_by_citations)), ]
    plots$citations_bar <- ggplot2::ggplot(cit, ggplot2::aes(x = reorder(label, value), y = value)) +
      ggplot2::geom_col(fill = ieee_colors$orange, color = "black", linewidth = 0.2) +
      ggplot2::geom_text(ggplot2::aes(label = format(value, big.mark = ",")), hjust = -0.1, size = 2.0) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.10))) +
      ggplot2::labs(x = NULL, y = "Citations") +
      ieee_theme()
  }

  # 3. SCP/MCP stacked bar (double-column, clear legend)
  if ("scp_mcp_summary" %in% names(result) && nrow(result$scp_mcp_summary) > 0) {
    scp_mcp <- result$scp_mcp_summary[1:min(10, nrow(result$scp_mcp_summary)), ]
    scp_long <- tidyr::pivot_longer(scp_mcp, cols = c("scp", "mcp"), names_to = "type", values_to = "count")
    scp_long$type <- toupper(scp_long$type)

    plots$scp_mcp_stacked <- ggplot2::ggplot(scp_long, ggplot2::aes(x = reorder(country, count), y = count, fill = type)) +
      ggplot2::geom_col(position = "stack", color = "black", linewidth = 0.2) +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(values = c("SCP" = ieee_colors$blue, "MCP" = ieee_colors$orange), name = NULL) +
      ggplot2::labs(x = NULL, y = "Documents") +
      ggplot2::ggtitle(NULL) +
      ieee_theme(base_size = 6) +
      ggplot2::theme(legend.position = "right")
  }

  # 4. Lorenz (smooth)
  if ("country_gini_articles" %in% names(result) && !is.na(result$country_gini_articles)) {
    lorenz <- m1_compute_lorenz(articles$value, smooth = TRUE)
    plots$lorenz_articles <- ggplot2::ggplot(lorenz, ggplot2::aes(x = cumulative_entities, y = cumulative_values)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = cumulative_values, ymax = cumulative_entities),
                           fill = ieee_colors$blue, alpha = 0.08) +
      ggplot2::geom_line(color = ieee_colors$blue, linewidth = 0.5) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = ieee_colors$gray, linewidth = 0.3) +
      ggplot2::annotate("text", x = 0.3, y = 0.7, label = sprintf("Gini = %.3f", result$country_gini_articles),
                        size = 2.2, fontface = "italic") +
      ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.25), labels = scales::percent_format(accuracy = 25)) +
      ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.25), labels = scales::percent_format(accuracy = 25)) +
      ggplot2::coord_fixed(ratio = 1, xlim = c(0, 1), ylim = c(0, 1)) +
      ggplot2::labs(x = "Cumulative % of Countries", y = "Cumulative % of Publications") +
      ggplot2::ggtitle(NULL) +
      ieee_theme()
  }

  # 5. Avg citations bar
  if ("avg_citations_by_country" %in% names(result) && nrow(result$avg_citations_by_country) > 0) {
    avg <- result$avg_citations_by_country[1:min(10, nrow(result$avg_citations_by_country)), ]
    plots$avg_citations_bar <- ggplot2::ggplot(avg, ggplot2::aes(x = reorder(label, value), y = value)) +
      ggplot2::geom_col(fill = ieee_colors$green, color = "black", linewidth = 0.2) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f", value)), hjust = -0.1, size = 2.0) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.10))) +
      ggplot2::labs(x = NULL, y = "Avg Citations") +
      ieee_theme()
  }

  list(status = "success", plots = plots, tables = list())
}
