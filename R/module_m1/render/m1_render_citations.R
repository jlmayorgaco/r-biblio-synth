# ============================================================================
# m1_render_citations.R - Citations Plots (FIXED)
# ============================================================================

#' @export
render_m1_citations <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"top_cited_documents" %in% names(result)) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  top_cited <- result$top_cited_documents
  if (nrow(top_cited) == 0) return(list(status = "stub", plots = list(), tables = list()))

  plots <- list()

  # Bar chart
  plots$bar <- ggplot2::ggplot(top_cited, ggplot2::aes(x = reorder(label, value), y = value)) +
    ggplot2::geom_bar(stat = "identity", fill = ieee_colors$orange, color = "black", linewidth = 0.3) +
    ggplot2::geom_text(ggplot2::aes(label = format(value, big.mark = ",")), hjust = -0.1, size = 2.5) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(title = "Most Cited Papers", x = "Paper", y = "Total Citations") +
    ieee_theme(base_size = 8) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 6))

  # Citations per year
  if ("citations_per_year" %in% names(result) && nrow(result$citations_per_year) > 0) {
    cpy <- result$citations_per_year
    plots$citations_per_year <- ggplot2::ggplot(cpy, ggplot2::aes(x = reorder(label, value), y = value)) +
      ggplot2::geom_bar(stat = "identity", fill = ieee_colors$green, color = "black", linewidth = 0.3) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f", value)), hjust = -0.1, size = 2.5) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
      ggplot2::labs(title = "Citations per Year", x = "Paper", y = "Citations/Year") +
      ieee_theme(base_size = 8)

    # Dual bar+line
    merged <- merge(top_cited, cpy, by = "label", suffixes = c("_tc", "_cpy"))
    if (nrow(merged) > 0) {
      max_tc <- max(merged$value_tc, na.rm = TRUE)
      max_cpy <- max(merged$value_cpy, na.rm = TRUE)
      scale_factor <- if (max_cpy > 0) max_tc / max_cpy else 1
      merged$label_short <- substr(merged$label, 1, 15)

      plots$dual_bar_line <- ggplot2::ggplot(merged, ggplot2::aes(x = reorder(label_short, value_tc))) +
        ggplot2::geom_bar(ggplot2::aes(y = value_tc), stat = "identity", fill = ieee_colors$blue, color = "black", linewidth = 0.3) +
        ggplot2::geom_line(ggplot2::aes(y = value_cpy * scale_factor, group = 1), color = ieee_colors$orange, linewidth = 0.8) +
        ggplot2::geom_point(ggplot2::aes(y = value_cpy * scale_factor), color = ieee_colors$orange, size = 2) +
        ggplot2::coord_flip() +
        ggplot2::scale_y_continuous(name = "Total Citations", sec.axis = ggplot2::sec_axis(~ . / scale_factor, name = "Citations/Year")) +
        ggplot2::labs(title = "Citations: Total vs Per Year", x = "Paper") +
        ieee_theme(base_size = 8)
    }
  }

  list(status = "success", plots = plots, tables = list())
}
