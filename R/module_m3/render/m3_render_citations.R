# ============================================================================
# m3_render_citations.R - Citation plots for M3
# ============================================================================

#' Render country citation plots
#'
#' @param citation_data Output from \code{m3_compute_citations}
#' @param config A configuration list (see \code{biblio_config})
#' @return A list with \code{status} and \code{plots}
#' @export
m3_render_citations <- function(citation_data, config = biblio_config()) {
  stub <- list(status = "stub", plots = list())

  if (!is.list(citation_data) ||
      nrow(citation_data$top_countries_by_citations) == 0) {
    return(stub)
  }

  plots <- list()

  # 1. Total citations bar chart
  top_cit <- citation_data$top_countries_by_citations
  top_cit$label_clean <- substr(trimws(top_cit$label), 1, 30)

  plots$bar_total_citations <- ggplot2::ggplot(
    top_cit,
    ggplot2::aes(x = reorder(label_clean, value), y = value)
  ) +
    ggplot2::geom_col(fill = ieee_colors$orange, color = "black", linewidth = 0.2) +
    ggplot2::geom_text(ggplot2::aes(label = round(value, 0)), hjust = -0.15, size = 2.0) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::labs(
      title = "Top Countries by Total Citations",
      x     = NULL,
      y     = "Total Citations"
    ) +
    ieee_theme()

  # 2. Average citations bar chart
  top_avg <- citation_data$top_countries_by_avg_citations
  if (nrow(top_avg) > 0) {
    top_avg$label_clean <- substr(trimws(top_avg$label), 1, 30)

    plots$bar_avg_citations <- ggplot2::ggplot(
      top_avg,
      ggplot2::aes(x = reorder(label_clean, value), y = value)
    ) +
      ggplot2::geom_col(fill = ieee_colors$yellow, color = "black", linewidth = 0.2) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f", value)),
                         hjust = -0.15, size = 2.0) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
      ggplot2::labs(
        title = "Top Countries by Average Citations per Article",
        x     = NULL,
        y     = "Avg Citations / Article"
      ) +
      ieee_theme()
  }

  list(status = "success", plots = plots)
}
