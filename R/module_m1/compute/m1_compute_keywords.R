# ============================================================================
# m1_compute_keywords.R - Keywords metric for M1
# ============================================================================

#' @export
compute_m1_keywords <- function(input, config = biblio_config()) {
  if (!is.data.frame(input) || nrow(input) == 0) {
    return(list(keywords_summary = list(), top_keywords = m1_empty_rank_table(), status = "error"))
  }

  top_n <- config$top_n_keywords

  # Try bibliometrix first
  kw_data <- tryCatch({
    cached <- get_cached_biblio_analysis(input)
    res <- cached$res
    s <- summary(res, pause = FALSE, verbose = FALSE)
    mr <- s$MostRelKeywords
    colnames(mr) <- make.unique(colnames(mr))
    freq_col <- if ("Occurrences" %in% colnames(mr)) "Occurrences" else if ("Freq" %in% colnames(mr)) "Freq" else colnames(mr)[2]
    mr[[freq_col]] <- suppressWarnings(as.numeric(mr[[freq_col]]))
    mr <- mr[!is.na(mr[[freq_col]]), ]
    mr <- mr[order(-mr[[freq_col]]), ]
    kw_col <- if ("Keywords" %in% colnames(mr)) "Keywords" else colnames(mr)[1]
    list(keywords = mr[[kw_col]], freq = mr[[freq_col]])
  }, error = function(e) {
    # Fallback: count DE column
    if ("DE" %in% names(input)) {
      kw_list <- lapply(input$DE[!is.na(input$DE)], function(x) trimws(unlist(strsplit(x, ";"))))
      kw_counts <- sort(table(unlist(kw_list)), decreasing = TRUE)
      list(keywords = names(kw_counts), freq = as.integer(kw_counts))
    } else {
      list(keywords = character(0), freq = integer(0))
    }
  })

  if (length(kw_data$keywords) > 0) {
    top_keywords <- tibble::tibble(
      rank  = seq_len(min(top_n, length(kw_data$keywords))),
      label = kw_data$keywords[1:min(top_n, length(kw_data$keywords))],
      value = kw_data$freq[1:min(top_n, length(kw_data$freq))]
    )
  } else {
    top_keywords <- m1_empty_rank_table()
  }

  list(
    keywords_summary = list(
      total_keywords_de = sum(!is.na(input$DE) & input$DE != ""),
      total_keywords_id = sum(!is.na(input$ID) & input$ID != "")
    ),
    top_keywords = top_keywords,
    status = "success"
  )
}
