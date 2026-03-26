# ============================================================================
# m1_render_authors.R - Authors Plots (FIXED)
# ============================================================================

#' @export
render_m1_authors <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"top_authors" %in% names(result)) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  authors <- result$top_authors
  if (nrow(authors) == 0) return(list(status = "stub", plots = list(), tables = list()))

  plots <- list()

  # Bar chart
  plots$bar <- ggplot2::ggplot(authors, ggplot2::aes(x = reorder(label, value), y = value)) +
    ggplot2::geom_bar(stat = "identity", fill = ieee_colors$blue, color = "black", linewidth = 0.3) +
    ggplot2::geom_text(ggplot2::aes(label = value), hjust = -0.15, size = 2.5) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(title = "Top Most Productive Authors", x = "Author", y = "Publications") +
    ieee_theme(base_size = 8) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 6))

  # Lorenz curve (FIXED: correct axes)
  if (!is.null(result$author_gini) && !is.na(result$author_gini)) {
    lorenz <- m1_compute_lorenz(authors$value)

    plots$lorenz <- ggplot2::ggplot(lorenz, ggplot2::aes(x = cumulative_entities, y = cumulative_values)) +
      ggplot2::geom_area(fill = ieee_colors$blue, alpha = 0.15) +
      ggplot2::geom_line(color = ieee_colors$blue, linewidth = 0.8) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = ieee_colors$gray, linewidth = 0.4) +
      ggplot2::annotate("text", x = 0.25, y = 0.75, label = sprintf("Gini = %.3f", result$author_gini), size = 3, fontface = "italic") +
      ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.25), labels = scales::percent_format()) +
      ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.25), labels = scales::percent_format()) +
      ggplot2::coord_fixed(ratio = 1, xlim = c(0, 1), ylim = c(0, 1)) +
      ggplot2::labs(title = "Lorenz Curve: Author Productivity", x = "Cumulative % of Authors", y = "Cumulative % of Publications") +
      ieee_theme(base_size = 8)
  }

  list(status = "success", plots = plots, tables = list())
}
