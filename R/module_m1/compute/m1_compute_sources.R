# ============================================================================
# m1_compute_sources.R - Sources metric for M1
# ============================================================================

#' @export
compute_m1_sources <- function(input, config = biblio_config()) {
  if (!is.data.frame(input) || nrow(input) == 0) {
    return(list(top_sources = m1_empty_rank_table(), source_summary = list(), source_gini = NA_real_, status = "error"))
  }

  top_n <- config$top_n_sources

  # Try bibliometrix first
  source_data <- tryCatch({
    res <- bibliometrix::biblioAnalysis(input, sep = ";")
    s <- summary(res, pause = FALSE, verbose = FALSE)
    mr <- s$MostRelSources
    colnames(mr) <- make.unique(colnames(mr))
    art_col <- if ("Articles" %in% colnames(mr)) "Articles" else colnames(mr)[2]
    mr[[art_col]] <- suppressWarnings(as.numeric(mr[[art_col]]))
    mr <- mr[!is.na(mr[[art_col]]), ]
    mr <- mr[order(-mr[[art_col]]), ]
    src_col <- if ("Sources" %in% colnames(mr)) "Sources" else colnames(mr)[1]
    list(sources = mr[[src_col]], articles = mr[[art_col]])
  }, error = function(e) {
    # Fallback: count SO column
    if ("SO" %in% names(input)) {
      so_counts <- table(input$SO, useNA = "no")
      so_counts <- sort(so_counts, decreasing = TRUE)
      list(sources = names(so_counts), articles = as.integer(so_counts))
    } else {
      list(sources = character(0), articles = integer(0))
    }
  })

  if (length(source_data$sources) > 0) {
    top_sources <- tibble::tibble(
      rank  = seq_len(min(top_n, length(source_data$sources))),
      label = source_data$sources[1:min(top_n, length(source_data$sources))],
      value = source_data$articles[1:min(top_n, length(source_data$articles))]
    )
    lorenz <- m1_compute_lorenz(top_sources$value)
    source_gini <- m1_compute_gini(lorenz$cumulative_x, lorenz$cumulative_y)
  } else {
    top_sources <- m1_empty_rank_table()
    source_gini <- NA_real_
  }

  list(
    top_sources = top_sources,
    source_summary = list(total_sources = if ("SO" %in% names(input)) length(unique(input$SO)) else 0, top_n = top_n),
    source_gini = source_gini,
    status = "success"
  )
}
