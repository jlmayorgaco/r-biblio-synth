# ============================================================================
# m1_render_bradford.R - Render bradford plots
# ============================================================================

#' Render M1 bradford plots
#'
#' @param result The compute result for bradford.
#' @param config A configuration list.
#' @return A list with \code{status}, \code{plots}, \code{tables}.
#' @export
render_m1_bradford <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"bradford_table" %in% names(result)) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  bt <- result$bradford_table
  if (nrow(bt) == 0) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  palette <- get_biblio_palette("main")

  # Bradford zone bar plot
  zone_colors <- stats::setNames(
    palette[1:length(unique(bt$zone))],
    unique(bt$zone)
  )

  bar_plot <- ggplot2::ggplot(bt, ggplot2::aes(x = reorder(source, freq), y = freq, fill = zone)) +
    ggplot2::geom_bar(stat = "identity", color = "black", linewidth = 0.3) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Bradford's Law Distribution",
      x = "Source",
      y = "Frequency",
      fill = "Zone"
    ) +
    ggplot2::scale_fill_manual(values = zone_colors) +
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
