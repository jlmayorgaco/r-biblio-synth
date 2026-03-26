# ============================================================================
# m1_render_sources.R - Sources Plots (IEEE REFACTORED)
# ============================================================================

#' @export
render_m1_sources <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"top_sources" %in% names(result)) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  sources <- result$top_sources
  if (nrow(sources) == 0) return(list(status = "stub", plots = list(), tables = list()))

  plots <- list()
  sources_top <- sources[1:min(10, nrow(sources)), ]

  # Truncate long source names intelligently
  sources_top$label_clean <- sapply(sources_top$label, function(x) {
    if (nchar(x) > 30) paste0(substr(x, 1, 27), "...") else x
  })

  # 1. Bar chart
  plots$bar <- ggplot2::ggplot(sources_top, ggplot2::aes(x = reorder(label_clean, value), y = value)) +
    ggplot2::geom_col(fill = ieee_colors$purple, color = "black", linewidth = 0.2) +
    ggplot2::geom_text(ggplot2::aes(label = value), hjust = -0.1, size = 2.0) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.10))) +
    ggplot2::labs(x = NULL, y = "Publications") +
    ieee_theme()

  # 2. Lorenz (smooth)
  if ("source_gini" %in% names(result) && !is.na(result$source_gini)) {
    lorenz <- m1_compute_lorenz(sources$value, smooth = TRUE)
    plots$lorenz <- ggplot2::ggplot(lorenz, ggplot2::aes(x = cumulative_entities, y = cumulative_values)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = cumulative_values, ymax = cumulative_entities),
                           fill = ieee_colors$purple, alpha = 0.08) +
      ggplot2::geom_line(color = ieee_colors$purple, linewidth = 0.5) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = ieee_colors$gray, linewidth = 0.3) +
      ggplot2::annotate("text", x = 0.3, y = 0.7, label = sprintf("Gini = %.3f", result$source_gini),
                        size = 2.2, fontface = "italic") +
      ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.25), labels = scales::percent_format(accuracy = 25)) +
      ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.25), labels = scales::percent_format(accuracy = 25)) +
      ggplot2::coord_fixed(ratio = 1, xlim = c(0, 1), ylim = c(0, 1)) +
      ggplot2::labs(x = "Cumulative % of Sources", y = "Cumulative % of Publications") +
      ggplot2::ggtitle(NULL) +
      ieee_theme()
  }

  list(status = "success", plots = plots, tables = list())
}
