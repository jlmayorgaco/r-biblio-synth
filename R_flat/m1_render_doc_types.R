# ============================================================================
# m1_render_doc_types.R - Render doc types plots
# ============================================================================

#' Render M1 doc types plots
#'
#' @param result The compute result for doc_types.
#' @param config A configuration list.
#' @return A list with \code{status}, \code{plots}, \code{tables}.
#' @export
render_m1_doc_types <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"doc_type_table" %in% names(result)) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  dt <- result$doc_type_table
  if (nrow(dt) == 0) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  palette <- get_biblio_palette("main")

  # Pie chart
  pie_plot <- ggplot2::ggplot(dt, ggplot2::aes(x = "", y = value, fill = label)) +
    ggplot2::geom_bar(width = 1, stat = "identity", color = "black", linewidth = 0.25) +
    ggplot2::coord_polar("y", start = 0) +
    ggplot2::labs(title = "Document Types Distribution") +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
      legend.title = ggplot2::element_blank(),
      legend.position = "right"
    ) +
    ggplot2::scale_fill_manual(values = palette)

  # Bar chart
  bar_plot <- ggplot2::ggplot(dt, ggplot2::aes(x = reorder(label, value), y = value)) +
    ggplot2::geom_bar(stat = "identity", fill = palette[1], color = "black", linewidth = 0.3) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Document Types Distribution",
      x = "Document Type",
      y = "Count"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
      axis.text = ggplot2::element_text(size = 10)
    )

  list(
    status = "success",
    plots  = list(pie = pie_plot, bar = bar_plot),
    tables = list()
  )
}
