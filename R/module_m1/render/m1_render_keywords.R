# ============================================================================
# m1_render_keywords.R - Keywords Plots (IEEE REFACTORED)
# ============================================================================

#' @export
render_m1_keywords <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"top_keywords" %in% names(result)) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  kw <- result$top_keywords
  if (nrow(kw) == 0) return(list(status = "stub", plots = list(), tables = list()))

  plots <- list()
  kw_top <- kw[1:min(15, nrow(kw)), ]

  # 1. Bar chart
  plots$bar <- ggplot2::ggplot(kw_top, ggplot2::aes(x = reorder(label, value), y = value)) +
    ggplot2::geom_col(fill = ieee_colors$green, color = "black", linewidth = 0.2) +
    ggplot2::geom_text(ggplot2::aes(label = value), hjust = -0.1, size = 2.0) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.10))) +
    ggplot2::labs(x = NULL, y = "Frequency") +
    ieee_theme()

  list(status = "success", plots = plots, tables = list())
}
