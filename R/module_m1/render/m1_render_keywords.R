# ============================================================================
# m1_render_keywords.R - Keywords Plots (FIXED)
# ============================================================================

#' @export
render_m1_keywords <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"top_keywords" %in% names(result)) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  kw <- result$top_keywords
  if (nrow(kw) == 0) return(list(status = "stub", plots = list(), tables = list()))

  plots <- list()

  # Bar chart
  plots$bar <- ggplot2::ggplot(kw, ggplot2::aes(x = reorder(label, value), y = value)) +
    ggplot2::geom_bar(stat = "identity", fill = ieee_colors$green, color = "black", linewidth = 0.3) +
    ggplot2::geom_text(ggplot2::aes(label = value), hjust = -0.1, size = 2.5) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(title = "Most Frequent Keywords", x = "Keyword", y = "Frequency") +
    ieee_theme(base_size = 8) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 5.5))

  # Wordcloud (optional)
  plots$wordcloud <- tryCatch({
    if (requireNamespace("ggwordcloud", quietly = TRUE)) {
      ggwordcloud::ggwordcloud(data = kw, word = "label", size = "value",
                               color = ieee_colors$blue, scale = c(3, 0.5), max_words = 50) +
        ggplot2::labs(title = "Keyword Cloud") +
        ggplot2::theme_void(base_size = 8) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 9, face = "bold", hjust = 0.5))
    } else { NULL }
  }, error = function(e) NULL)

  list(status = "success", plots = plots, tables = list())
}
