# ============================================================================
# m1_render_authors.R - Render authors plots
# ============================================================================

#' Render M1 authors plots
#'
#' @param result The compute result for authors.
#' @param config A configuration list.
#' @return A list with \code{status}, \code{plots}, \code{tables}.
#' @export
render_m1_authors <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"top_authors" %in% names(result)) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  authors <- result$top_authors
  if (nrow(authors) == 0) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  palette <- get_biblio_palette("main")

  # Bar plot
  bar_plot <- ggplot2::ggplot(authors, ggplot2::aes(x = reorder(label, value), y = value)) +
    ggplot2::geom_bar(stat = "identity", fill = palette[1], color = "black", linewidth = 0.3) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Top Most Productive Authors",
      x = "Author",
      y = "Number of Articles"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
      axis.text = ggplot2::element_text(size = 10)
    )

  # Lorenz curve (if gini data available)
  plots <- list(bar = bar_plot)

  if (!is.null(result$author_gini) && !is.na(result$author_gini)) {
    lorenz <- m1_compute_lorenz(authors$value)
    lorenz_plot <- ggplot2::ggplot(lorenz, ggplot2::aes(x = cumulative_x, y = cumulative_y)) +
      ggplot2::geom_line(color = palette[1], linewidth = 1.2) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
      ggplot2::labs(
        title = paste0("Lorenz Curve: Author Productivity (Gini = ", round(result$author_gini, 3), ")"),
        x = "Cumulative % of Articles",
        y = "Cumulative % of Authors"
      ) +
      ggplot2::coord_fixed(ratio = 1) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5)
      )
    plots$lorenz <- lorenz_plot
  }

  list(
    status = "success",
    plots  = plots,
    tables = list()
  )
}
