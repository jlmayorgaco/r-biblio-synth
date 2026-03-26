# ============================================================================
# m1_render_overview.R - IEEE Quality Overview
# ============================================================================

#' @export
render_m1_overview <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"summary_table" %in% names(result)) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  plots <- list()
  tables <- list(summary = result$summary_table)

  # Overview as horizontal bar chart
  tbl <- result$summary_table
  if (nrow(tbl) > 0) {
    # Filter to numeric-looking values only
    numeric_rows <- suppressWarnings(as.numeric(tbl$value))
    tbl_plot <- tbl[!is.na(numeric_rows), ]

    if (nrow(tbl_plot) > 0) {
      tbl_plot$value_num <- as.numeric(tbl_plot$value)

      plots$summary_bar <- ggplot2::ggplot(
        tbl_plot,
        ggplot2::aes(x = reorder(metric, value_num), y = value_num)
      ) +
        ggplot2::geom_bar(stat = "identity", fill = ieee_colors$blue, color = "black", linewidth = 0.3) +
        ggplot2::geom_text(
          ggplot2::aes(label = format(value_num, big.mark = ",", digits = 3)),
          hjust = -0.1, size = 2.5, color = "black"
        ) +
        ggplot2::coord_flip() +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
        ggplot2::labs(
          title = "Bibliometric Overview",
          x = "",
          y = "Value"
        ) +
        ieee_theme(base_size = 8) +
        ggplot2::theme(axis.text.y = ggplot2::element_text(size = 7))
    }
  }

  list(status = "success", plots = plots, tables = tables)
}
