# ============================================================================
# m1_render_countries.R - Countries Plots (FIXED)
# ============================================================================

#' @export
render_m1_countries <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"top_countries_by_articles" %in% names(result)) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  articles <- result$top_countries_by_articles
  if (nrow(articles) == 0) return(list(status = "stub", plots = list(), tables = list()))

  plots <- list()
  ec <- unlist(ieee_colors[1:5])

  # Articles bar
  plots$articles_bar <- ggplot2::ggplot(articles, ggplot2::aes(x = reorder(label, value), y = value)) +
    ggplot2::geom_bar(stat = "identity", fill = ieee_colors$blue, color = "black", linewidth = 0.3) +
    ggplot2::geom_text(ggplot2::aes(label = value), hjust = -0.1, size = 2.5) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(title = "Most Productive Countries", x = "Country", y = "Publications") +
    ieee_theme(base_size = 8) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 6))

  # Citations bar
  if ("top_countries_by_citations" %in% names(result) && nrow(result$top_countries_by_citations) > 0) {
    citations <- result$top_countries_by_citations
    plots$citations_bar <- ggplot2::ggplot(citations, ggplot2::aes(x = reorder(label, value), y = value)) +
      ggplot2::geom_bar(stat = "identity", fill = ieee_colors$orange, color = "black", linewidth = 0.3) +
      ggplot2::geom_text(ggplot2::aes(label = format(value, big.mark = ",")), hjust = -0.1, size = 2.5) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
      ggplot2::labs(title = "Top Countries by Citations", x = "Country", y = "Total Citations") +
      ieee_theme(base_size = 8) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 6))
  }

  # SCP/MCP stacked
  if ("scp_mcp_summary" %in% names(result) && nrow(result$scp_mcp_summary) > 0) {
    scp_mcp <- result$scp_mcp_summary
    scp_mcp_long <- tidyr::pivot_longer(scp_mcp, cols = c("scp", "mcp"), names_to = "type", values_to = "count")
    scp_mcp_long$type <- toupper(scp_mcp_long$type)

    plots$scp_mcp_stacked <- ggplot2::ggplot(scp_mcp_long, ggplot2::aes(x = reorder(country, count), y = count, fill = type)) +
      ggplot2::geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.3) +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(values = c("SCP" = ieee_colors$blue, "MCP" = ieee_colors$orange), name = "Type") +
      ggplot2::labs(title = "Single vs Multi-Country Publications", x = "Country", y = "Documents") +
      ieee_theme(base_size = 8)
  }

  # Lorenz curve (FIXED: correct axes)
  if ("country_gini_articles" %in% names(result) && !is.na(result$country_gini_articles)) {
    lorenz <- m1_compute_lorenz(articles$value)

    plots$lorenz_articles <- ggplot2::ggplot(lorenz, ggplot2::aes(x = cumulative_entities, y = cumulative_values)) +
      ggplot2::geom_area(fill = ieee_colors$blue, alpha = 0.15) +
      ggplot2::geom_line(color = ieee_colors$blue, linewidth = 0.8) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = ieee_colors$gray, linewidth = 0.4) +
      ggplot2::annotate("text", x = 0.25, y = 0.75, label = sprintf("Gini = %.3f", result$country_gini_articles), size = 3, fontface = "italic") +
      ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.25), labels = scales::percent_format()) +
      ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.25), labels = scales::percent_format()) +
      ggplot2::coord_fixed(ratio = 1, xlim = c(0, 1), ylim = c(0, 1)) +
      ggplot2::labs(title = "Lorenz Curve: Country Productivity", x = "Cumulative % of Countries", y = "Cumulative % of Publications") +
      ieee_theme(base_size = 8)
  }

  # Average citations
  if ("avg_citations_by_country" %in% names(result) && nrow(result$avg_citations_by_country) > 0) {
    avg_cit <- result$avg_citations_by_country
    plots$avg_citations_bar <- ggplot2::ggplot(avg_cit, ggplot2::aes(x = reorder(label, value), y = value)) +
      ggplot2::geom_bar(stat = "identity", fill = ieee_colors$yellow, color = "black", linewidth = 0.3) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f", value)), hjust = -0.1, size = 2.5) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
      ggplot2::labs(title = "Average Citations per Article", x = "Country", y = "Avg Citations") +
      ieee_theme(base_size = 8) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 6))
  }

  list(status = "success", plots = plots, tables = list())
}
