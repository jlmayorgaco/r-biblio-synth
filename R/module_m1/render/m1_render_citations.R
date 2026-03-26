# ============================================================================
# m1_render_citations.R - Citations Plots (IEEE REFACTORED)
# ============================================================================

#' @export
render_m1_citations <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"top_cited_documents" %in% names(result)) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  tc <- result$top_cited_documents
  if (nrow(tc) == 0) return(list(status = "stub", plots = list(), tables = list()))

  plots <- list()

  # Clean labels: use short PaperID or truncate
  tc$label_clean <- ifelse(is.na(tc$label) | tc$label == "NA", paste0("[", tc$rank, "]"), tc$label)
  tc$label_clean <- substr(tc$label_clean, 1, 20)

  # 1. Top Cited Papers (horizontal bar)
  plots$bar <- ggplot2::ggplot(tc[1:min(10, nrow(tc)), ], ggplot2::aes(x = reorder(label_clean, value), y = value)) +
    ggplot2::geom_col(fill = ieee_colors$orange, color = "black", linewidth = 0.2) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::labs(x = NULL, y = "Citations") +
    ggplot2::ggtitle(NULL) +
    ieee_theme() +
    ggplot2::theme(plot.title = ggplot2::element_blank())

  # 2. Citations per year
  if ("citations_per_year" %in% names(result) && nrow(result$citations_per_year) > 0) {
    cpy <- result$citations_per_year
    cpy$label_clean <- ifelse(is.na(cpy$label) | cpy$label == "NA", paste0("[", cpy$rank, "]"), cpy$label)
    cpy$label_clean <- substr(cpy$label_clean, 1, 20)

    plots$citations_per_year <- ggplot2::ggplot(cpy[1:min(10, nrow(cpy)), ], ggplot2::aes(x = reorder(label_clean, value), y = value)) +
      ggplot2::geom_col(fill = ieee_colors$green, color = "black", linewidth = 0.2) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.08))) +
      ggplot2::labs(x = NULL, y = "Citations/Year") +
      ggplot2::ggtitle(NULL) +
      ieee_theme()

    # 3. Dual plot (2-column, simplified)
    merged <- merge(tc, cpy, by = "rank", suffixes = c("_tc", "_cpy"))
    if (nrow(merged) > 0) {
      merged$label_clean <- ifelse(is.na(merged$label_tc) | merged$label_tc == "NA",
                                   paste0("[", merged$rank, "]"), merged$label_tc)
      merged$label_clean <- substr(merged$label_clean, 1, 20)

      plots$dual_bar_line <- ggplot2::ggplot(merged[1:min(8, nrow(merged)), ]) +
        ggplot2::geom_col(ggplot2::aes(x = reorder(label_clean, value_tc), y = value_tc),
                          fill = ieee_colors$blue, color = "black", linewidth = 0.2) +
        ggplot2::geom_point(ggplot2::aes(x = reorder(label_clean, value_tc), y = value_cpy * max(value_tc) / max(value_cpy)),
                            color = ieee_colors$orange, size = 2) +
        ggplot2::coord_flip() +
        ggplot2::scale_y_continuous(
          name = "Total Citations",
          sec.axis = ggplot2::sec_axis(~ . * max(merged$value_cpy) / max(merged$value_tc), name = "Citations/Year")
        ) +
        ggplot2::labs(x = NULL) +
        ggplot2::ggtitle(NULL) +
        ieee_theme(base_size = 6) +
        ggplot2::theme(axis.title.y.right = ggplot2::element_text(color = ieee_colors$orange))
    }
  }

  list(status = "success", plots = plots, tables = list())
}
