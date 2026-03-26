# ============================================================================
# m1_render_sources.R - Sources Plots (FIXED)
# ============================================================================

#' @export
render_m1_sources <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"top_sources" %in% names(result)) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  sources <- result$top_sources
  if (nrow(sources) == 0) return(list(status = "stub", plots = list(), tables = list()))

  plots <- list()

  # Bar chart
  plots$bar <- ggplot2::ggplot(sources, ggplot2::aes(x = reorder(label, value), y = value)) +
    ggplot2::geom_bar(stat = "identity", fill = ieee_colors$purple, color = "black", linewidth = 0.3) +
    ggplot2::geom_text(ggplot2::aes(label = value), hjust = -0.1, size = 2.5) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(title = "Most Productive Sources", x = "Source", y = "Publications") +
    ieee_theme(base_size = 8) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 5.5))

  # Lorenz curve (FIXED: correct axes)
  if ("source_gini" %in% names(result) && !is.na(result$source_gini)) {
    lorenz <- m1_compute_lorenz(sources$value)

    plots$lorenz <- ggplot2::ggplot(lorenz, ggplot2::aes(x = cumulative_entities, y = cumulative_values)) +
      ggplot2::geom_area(fill = ieee_colors$purple, alpha = 0.15) +
      ggplot2::geom_line(color = ieee_colors$purple, linewidth = 0.8) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = ieee_colors$gray, linewidth = 0.4) +
      ggplot2::annotate("text", x = 0.25, y = 0.75, label = sprintf("Gini = %.3f", result$source_gini), size = 3, fontface = "italic") +
      ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.25), labels = scales::percent_format()) +
      ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.25), labels = scales::percent_format()) +
      ggplot2::coord_fixed(ratio = 1, xlim = c(0, 1), ylim = c(0, 1)) +
      ggplot2::labs(title = "Lorenz Curve: Source Distribution", x = "Cumulative % of Sources", y = "Cumulative % of Publications") +
      ieee_theme(base_size = 8)
  }

  list(status = "success", plots = plots, tables = list())
}
