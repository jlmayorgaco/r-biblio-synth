# ============================================================================
# m1_render_countries.R - Render countries plots
# ============================================================================

#' Render M1 countries plots
#'
#' @param result The compute result for countries.
#' @param config A configuration list.
#' @return A list with \code{status}, \code{plots}, \code{tables}.
#' @export
render_m1_countries <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"top_countries_by_articles" %in% names(result)) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  articles <- result$top_countries_by_articles
  if (nrow(articles) == 0) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  palette <- get_biblio_palette("main")
  plots <- list()

  # Bar plot - top countries by articles
  plots$articles_bar <- ggplot2::ggplot(articles, ggplot2::aes(x = reorder(label, value), y = value)) +
    ggplot2::geom_bar(stat = "identity", fill = palette[1], color = "black", linewidth = 0.3) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Top Most Productive Countries",
      x = "Country",
      y = "Number of Articles"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5)
    )

  # Bar plot - top countries by citations
  if ("top_countries_by_citations" %in% names(result) && nrow(result$top_countries_by_citations) > 0) {
    citations <- result$top_countries_by_citations
    plots$citations_bar <- ggplot2::ggplot(citations, ggplot2::aes(x = reorder(label, value), y = value)) +
      ggplot2::geom_bar(stat = "identity", fill = palette[2], color = "black", linewidth = 0.3) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Top Countries by Total Citations",
        x = "Country",
        y = "Total Citations"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5)
      )
  }

  # SCP/MCP stacked bar
  if ("scp_mcp_summary" %in% names(result) && nrow(result$scp_mcp_summary) > 0) {
    scp_mcp <- result$scp_mcp_summary
    scp_mcp_long <- tidyr::pivot_longer(
      scp_mcp, cols = c("scp", "mcp"), names_to = "type", values_to = "count"
    )
    scp_mcp_long$type <- toupper(scp_mcp_long$type)

    plots$scp_mcp_stacked <- ggplot2::ggplot(
      scp_mcp_long,
      ggplot2::aes(x = reorder(country, count), y = count, fill = type)
    ) +
      ggplot2::geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.3) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "SCP vs MCP by Country",
        x = "Country",
        y = "Number of Documents",
        fill = "Type"
      ) +
      ggplot2::scale_fill_manual(values = c("SCP" = palette[1], "MCP" = palette[2])) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5)
      )
  }

  # Lorenz curve for articles
  if ("country_gini_articles" %in% names(result) && !is.na(result$country_gini_articles)) {
    lorenz <- m1_compute_lorenz(articles$value)
    plots$lorenz_articles <- ggplot2::ggplot(lorenz, ggplot2::aes(x = cumulative_x, y = cumulative_y)) +
      ggplot2::geom_line(color = palette[1], linewidth = 1.2) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
      ggplot2::labs(
        title = paste0("Lorenz Curve: Country Articles (Gini = ", round(result$country_gini_articles, 3), ")"),
        x = "Cumulative % of Articles",
        y = "Cumulative % of Countries"
      ) +
      ggplot2::coord_fixed(ratio = 1) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5)
      )
  }

  list(
    status = "success",
    plots  = plots,
    tables = list()
  )
}
