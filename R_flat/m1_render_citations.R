# ============================================================================
# m1_render_citations.R - Render citations plots
# ============================================================================

#' Render M1 citations plots
#'
#' @param result The compute result for citations.
#' @param config A configuration list.
#' @return A list with \code{status}, \code{plots}, \code{tables}.
#' @export
render_m1_citations <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"top_cited_documents" %in% names(result)) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  top_cited <- result$top_cited_documents
  if (nrow(top_cited) == 0) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  palette <- get_biblio_palette("main")

  # Bar plot
  bar_plot <- ggplot2::ggplot(top_cited, ggplot2::aes(x = reorder(label, value), y = value)) +
    ggplot2::geom_bar(stat = "identity", fill = palette[1], color = "black", linewidth = 0.3) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Top Most Cited Papers",
      x = "Paper",
      y = "Total Citations"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
      axis.text = ggplot2::element_text(size = 10)
    )

  plots <- list(bar = bar_plot)

  # Citations per year plot
  if ("citations_per_year" %in% names(result) && nrow(result$citations_per_year) > 0) {
    cpy <- result$citations_per_year
    cpy_plot <- ggplot2::ggplot(cpy, ggplot2::aes(x = reorder(label, value), y = value)) +
      ggplot2::geom_bar(stat = "identity", fill = palette[2], color = "black", linewidth = 0.3) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Top Papers by Citations per Year",
        x = "Paper",
        y = "Citations per Year"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5)
      )
    plots$citations_per_year <- cpy_plot
  }

  list(
    status = "success",
    plots  = plots,
    tables = list()
  )
}
