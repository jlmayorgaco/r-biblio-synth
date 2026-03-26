# ============================================================================
# m1_render_doc_types.R - Doc Types Plots (IEEE REFACTORED)
# ============================================================================

#' @export
render_m1_doc_types <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"doc_type_table" %in% names(result)) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  dt <- result$doc_type_table
  if (nrow(dt) == 0) return(list(status = "stub", plots = list(), tables = list()))

  plots <- list()
  n_types <- nrow(dt)
  pal <- get_ieee_palette(n_types)

  # 1. Bar chart (IEEE preferred - cleaner than pie)
  dt$pct_label <- sprintf("%.1f%%", dt$percentage)

  plots$bar <- ggplot2::ggplot(dt, ggplot2::aes(x = reorder(label, value), y = value)) +
    ggplot2::geom_col(fill = ieee_colors$blue, color = "black", linewidth = 0.2) +
    ggplot2::geom_text(ggplot2::aes(label = pct_label), hjust = -0.1, size = 2.2, color = "black") +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(x = NULL, y = "Documents") +
    ieee_theme()

  # 2. Pie chart (use scale_fill_manual with correct mapping)
  dt$label <- factor(dt$label, levels = rev(dt$label))
  pie_colors <- setNames(pal, levels(dt$label))

  plots$pie <- ggplot2::ggplot(dt, ggplot2::aes(x = "", y = value, fill = label)) +
    ggplot2::geom_bar(width = 0.8, stat = "identity", color = "white", linewidth = 0.3) +
    ggplot2::coord_polar("y", start = 0) +
    ggplot2::scale_fill_manual(values = pie_colors) +
    ggplot2::theme_void(base_size = 7) +
    ggplot2::theme(
      plot.title = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 6),
      legend.key.size = ggplot2::unit(3, "mm")
    )

  list(status = "success", plots = plots, tables = list())
}
