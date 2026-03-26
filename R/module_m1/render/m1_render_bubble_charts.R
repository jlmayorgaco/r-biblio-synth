# ============================================================================
# module_m1/render/m1_render_bubble_charts.R - Bubble chart visualizations
# ============================================================================

#' @export
render_m1_bubble_charts <- function(result, config = biblio_config()) {
  if (!inherits(result, "list")) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  plots <- list()

  # Countries: TP vs TC bubble
  if ("top_countries_by_articles" %in% names(result) && "top_countries_by_citations" %in% names(result)) {
    tp <- result$top_countries_by_articles
    tc <- result$top_countries_by_citations

    if (nrow(tp) > 0 && nrow(tc) > 0) {
      merged <- merge(tp, tc, by = "label", suffixes = c("_tp", "_tc"))
      if (nrow(merged) > 0) {
        # Compute avg citations per article
        merged$avg_tc <- merged$value_tc / merged$value_tp

        # Quadrant bounds (medians)
        median_tp <- median(merged$value_tp)
        median_tc <- median(merged$value_tc)

        merged$quadrant <- ifelse(merged$value_tp >= median_tp & merged$value_tc >= median_tc, "High TP, High TC",
                           ifelse(merged$value_tp >= median_tp & merged$value_tc < median_tc, "High TP, Low TC",
                           ifelse(merged$value_tp < median_tp & merged$value_tc >= median_tc, "Low TP, High TC",
                                  "Low TP, Low TC")))

        plots$countries_tp_tc <- ggplot2::ggplot(merged, ggplot2::aes(x = value_tp, y = value_tc, size = avg_tc, color = quadrant)) +
          ggplot2::geom_point(alpha = 0.7) +
          ggplot2::geom_hline(yintercept = median_tc, linetype = "dashed", color = "gray50", linewidth = 0.3) +
          ggplot2::geom_vline(xintercept = median_tp, linetype = "dashed", color = "gray50", linewidth = 0.3) +
          ggrepel::geom_text_repel(ggplot2::aes(label = label), size = 2.5, max.overlaps = 15) +
          ggplot2::scale_color_manual(values = c(
            "High TP, High TC" = ieee_colors$green,
            "High TP, Low TC" = ieee_colors$orange,
            "Low TP, High TC" = ieee_colors$blue,
            "Low TP, Low TC" = ieee_colors$gray
          )) +
          ggplot2::scale_size_continuous(range = c(2, 10), name = "Avg TC/Article") +
          ggplot2::labs(
            title = "Country Productivity vs Impact",
            x = "Total Publications",
            y = "Total Citations"
          ) +
          ieee_theme(base_size = 8) +
          ggplot2::theme(legend.position = "right")
      }
    }
  }

  # Papers: TC vs TCperYear bubble
  if ("top_cited_documents" %in% names(result) && "citations_per_year" %in% names(result)) {
    tc <- result$top_cited_documents
    cpy <- result$citations_per_year

    if (nrow(tc) > 0 && nrow(cpy) > 0) {
      merged_papers <- merge(tc, cpy, by = "label", suffixes = c("_tc", "_cpy"))
      if (nrow(merged_papers) > 0) {
        plots$papers_tc_cpy <- ggplot2::ggplot(merged_papers, ggplot2::aes(x = value_tc, y = value_cpy)) +
          ggplot2::geom_point(color = ieee_colors$blue, size = 3, alpha = 0.7) +
          ggrepel::geom_text_repel(ggplot2::aes(label = label), size = 2, max.overlaps = 10) +
          ggplot2::labs(
            title = "Papers: Total Citations vs Citations/Year",
            x = "Total Citations",
            y = "Citations per Year"
          ) +
          ieee_theme(base_size = 8)
      }
    }
  }

  list(status = "success", plots = plots, tables = list())
}
