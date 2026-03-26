# ============================================================================
# m1_render_doc_types.R - Doc Types Plots (FIXED)
# ============================================================================

#' @export
render_m1_doc_types <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"doc_type_table" %in% names(result)) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  dt <- result$doc_type_table
  if (nrow(dt) == 0) return(list(status = "stub", plots = list(), tables = list()))

  plots <- list()
  ec <- unlist(ieee_colors[1:8])

  dt$label <- factor(dt$label, levels = rev(dt$label))

  # Pie chart
  plots$pie <- ggplot2::ggplot(dt, ggplot2::aes(x = "", y = value, fill = label)) +
    ggplot2::geom_bar(width = 0.8, stat = "identity", color = "black", linewidth = 0.3) +
    ggplot2::coord_polar("y", start = 0) +
    ggplot2::labs(title = "Document Type Distribution") +
    ggplot2::scale_fill_manual(values = ec) +
    ggplot2::theme_void(base_size = 8) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 9, face = "bold", hjust = 0.5),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 7)
    )

  # Bar chart
  dt_bar <- dt
  dt_bar$pct_label <- sprintf("%.1f%%", dt_bar$percentage)

  plots$bar <- ggplot2::ggplot(dt_bar, ggplot2::aes(x = reorder(label, value), y = value)) +
    ggplot2::geom_bar(stat = "identity", fill = ieee_colors$blue, color = "black", linewidth = 0.3) +
    ggplot2::geom_text(ggplot2::aes(label = pct_label), hjust = -0.15, size = 2.5) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::labs(title = "Document Types", x = NULL, y = "Documents") +
    ieee_theme(base_size = 8)

  list(status = "success", plots = plots, tables = list())
}
