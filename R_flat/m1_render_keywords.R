# ============================================================================
# m1_render_keywords.R - Render keywords plots
# ============================================================================

#' Render M1 keywords plots
#'
#' @param result The compute result for keywords.
#' @param config A configuration list.
#' @return A list with \code{status}, \code{plots}, \code{tables}.
#' @export
render_m1_keywords <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"top_keywords" %in% names(result)) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  kw <- result$top_keywords
  if (nrow(kw) == 0) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  palette <- get_biblio_palette("main")

  # Bar plot (safer than wordcloud for package stability)
  bar_plot <- ggplot2::ggplot(kw, ggplot2::aes(x = reorder(label, value), y = value)) +
    ggplot2::geom_bar(stat = "identity", fill = palette[1], color = "black", linewidth = 0.3) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Top Keywords by Frequency",
      x = "Keyword",
      y = "Frequency"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5)
    )

  list(
    status = "success",
    plots  = list(bar = bar_plot),
    tables = list()
  )
}
