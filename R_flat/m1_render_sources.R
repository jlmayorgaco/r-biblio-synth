# ============================================================================
# m1_render_sources.R - Render sources plots
# ============================================================================

#' Render M1 sources plots
#'
#' @param result The compute result for sources.
#' @param config A configuration list.
#' @return A list with \code{status}, \code{plots}, \code{tables}.
#' @export
render_m1_sources <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"top_sources" %in% names(result)) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  sources <- result$top_sources
  if (nrow(sources) == 0) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  palette <- get_biblio_palette("main")

  # Bar plot
  bar_plot <- ggplot2::ggplot(sources, ggplot2::aes(x = reorder(label, value), y = value)) +
    ggplot2::geom_bar(stat = "identity", fill = palette[1], color = "black", linewidth = 0.3) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Top Most Relevant Sources",
      x = "Source",
      y = "Number of Articles"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5)
    )

  plots <- list(bar = bar_plot)

  # Lorenz curve
  if ("source_gini" %in% names(result) && !is.na(result$source_gini)) {
    lorenz <- m1_compute_lorenz(sources$value)
    plots$lorenz <- ggplot2::ggplot(lorenz, ggplot2::aes(x = cumulative_x, y = cumulative_y)) +
      ggplot2::geom_line(color = palette[1], linewidth = 1.2) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
      ggplot2::labs(
        title = paste0("Lorenz Curve: Source Distribution (Gini = ", round(result$source_gini, 3), ")"),
        x = "Cumulative % of Articles",
        y = "Cumulative % of Sources"
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
