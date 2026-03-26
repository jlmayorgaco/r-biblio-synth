# ============================================================================
# m1_render_overview.R - Overview (IEEE REFACTORED)
# ============================================================================

#' @export
render_m1_overview <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"summary_table" %in% names(result)) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  plots <- list()
  tables <- list(summary = result$summary_table)

  tbl <- result$summary_table
  if (nrow(tbl) == 0) return(list(status = "success", plots = plots, tables = tables))

  # Filter numeric values only
  numeric_rows <- suppressWarnings(as.numeric(tbl$value))
  tbl_plot <- tbl[!is.na(numeric_rows), ]

  if (nrow(tbl_plot) > 0) {
    tbl_plot$value_num <- as.numeric(tbl_plot$value)
    tbl_plot$label <- format(tbl_plot$value_num, big.mark = ",", digits = 3)

    plots$summary_bar <- ggplot2::ggplot(tbl_plot, ggplot2::aes(x = reorder(metric, value_num), y = value_num)) +
      ggplot2::geom_col(fill = ieee_colors$blue, color = "black", linewidth = 0.2) +
      ggplot2::geom_text(ggplot2::aes(label = label), hjust = -0.1, size = 2.0) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.10))) +
      ggplot2::labs(x = NULL, y = NULL) +
      ieee_theme()
  }

  list(status = "success", plots = plots, tables = tables)
}
