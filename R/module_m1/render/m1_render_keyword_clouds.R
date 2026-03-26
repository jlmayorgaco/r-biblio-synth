# ============================================================================
# module_m1/render/m1_render_keyword_clouds.R - Keyword cloud visualizations
# ============================================================================

#' @export
render_m1_keyword_clouds <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"chunks" %in% names(result)) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  plots <- list()
  chunks <- result$chunks

  if (length(chunks) == 0) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  # Bar chart of top keywords overall
  grid_data <- result$grid_data
  if (!is.null(grid_data) && nrow(grid_data) > 0) {
    top_kw <- head(grid_data[order(-grid_data$freq), ], 30)
    plots$top_keywords_bar <- ggplot2::ggplot(top_kw, ggplot2::aes(x = reorder(keyword, freq), y = freq)) +
      ggplot2::geom_bar(stat = "identity", fill = ieee_colors$blue, color = "black", linewidth = 0.2) +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "Top 30 Keywords", x = "Keyword", y = "Frequency") +
      ieee_theme(base_size = 8) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 5))
  }

  # Keyword evolution over chunks
  if (length(chunks) > 1) {
    chunk_summary <- do.call(rbind, lapply(names(chunks), function(nm) {
      chunk <- chunks[[nm]]
      data.frame(
        chunk = chunk$chunk[1],
        label = paste0(chunk$year_start[1], "-", chunk$year_end[1]),
        n_keywords = length(unique(chunk$keyword)),
        total_freq = sum(chunk$freq),
        stringsAsFactors = FALSE
      )
    }))

    plots$keyword_evolution <- ggplot2::ggplot(chunk_summary, ggplot2::aes(x = chunk, y = n_keywords)) +
      ggplot2::geom_line(color = ieee_colors$blue, linewidth = 0.8) +
      ggplot2::geom_point(color = ieee_colors$blue, size = 3) +
      ggplot2::scale_x_continuous(breaks = chunk_summary$chunk, labels = chunk_summary$label) +
      ggplot2::labs(title = "Keyword Diversity Over Time", x = "Time Period", y = "Unique Keywords") +
      ieee_theme(base_size = 8) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 6))
  }

  # Individual wordcloud for each chunk (if ggwordcloud available)
  if (requireNamespace("ggwordcloud", quietly = TRUE) && length(chunks) > 0) {
    chunk_wordclouds <- list()
    for (nm in names(chunks)) {
      chunk <- head(chunks[[nm]], 30)
      if (nrow(chunk) > 0) {
        chunk$year_label <- paste0(chunk$year_start, "-", chunk$year_end)
        wc <- ggwordcloud::ggwordcloud(
          data = chunk, word = "keyword", size = "freq",
          color = sample(c(ieee_colors$blue, ieee_colors$orange, ieee_colors$green), nrow(chunk), replace = TRUE),
          scale = c(3, 0.5), max_words = 30
        ) +
          ggplot2::labs(title = unique(chunk$year_label)) +
          ggplot2::theme_void(base_size = 6) +
          ggplot2::theme(plot.title = ggplot2::element_text(size = 7, face = "bold", hjust = 0.5))
        chunk_wordclouds[[nm]] <- wc
      }
    }
    plots$wordclouds <- chunk_wordclouds
  }

  list(status = "success", plots = plots, tables = list())
}
