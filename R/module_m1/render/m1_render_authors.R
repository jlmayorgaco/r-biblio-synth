# ============================================================================
# m1_render_authors.R - Authors Plots (IEEE REFACTORED)
# ============================================================================

#' @export
render_m1_authors <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"top_authors" %in% names(result)) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  authors <- result$top_authors
  if (nrow(authors) == 0) return(list(status = "stub", plots = list(), tables = list()))

  plots <- list()

  # 1. Bar chart (top 10, clean)
  authors_top <- authors[1:min(10, nrow(authors)), ]
  authors_top$label_clean <- substr(trimws(authors_top$label), 1, 25)

  plots$bar <- ggplot2::ggplot(authors_top, ggplot2::aes(x = reorder(label_clean, value), y = value)) +
    ggplot2::geom_col(fill = ieee_colors$blue, color = "black", linewidth = 0.2) +
    ggplot2::geom_text(ggplot2::aes(label = value), hjust = -0.1, size = 2.0, color = "black") +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.10))) +
    ggplot2::labs(x = NULL, y = "Publications") +
    ieee_theme()

  # 2. Lorenz curve (smooth interpolation)
  if (!is.null(result$author_gini) && !is.na(result$author_gini)) {
    lorenz <- m1_compute_lorenz(authors$value, smooth = TRUE)

    plots$lorenz <- ggplot2::ggplot(lorenz, ggplot2::aes(x = cumulative_entities, y = cumulative_values)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = cumulative_values, ymax = cumulative_entities),
                           fill = ieee_colors$blue, alpha = 0.08) +
      ggplot2::geom_line(color = ieee_colors$blue, linewidth = 0.5) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = ieee_colors$gray, linewidth = 0.3) +
      ggplot2::annotate("text", x = 0.3, y = 0.7,
                        label = sprintf("Gini = %.3f", result$author_gini),
                        size = 2.2, fontface = "italic", color = "black") +
      ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.25), labels = scales::percent_format(accuracy = 25)) +
      ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.25), labels = scales::percent_format(accuracy = 25)) +
      ggplot2::coord_fixed(ratio = 1, xlim = c(0, 1), ylim = c(0, 1)) +
      ggplot2::labs(x = "Cumulative % of Authors", y = "Cumulative % of Publications") +
      ggplot2::ggtitle(NULL) +
      ieee_theme()
  }

  list(status = "success", plots = plots, tables = list())
}
