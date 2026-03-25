# ============================================================================
# m1_compute_keywords.R - Keywords metric for M1
# ============================================================================

#' Compute M1 keywords
#'
#' @param input A data frame of bibliographic data.
#' @param config A configuration list.
#' @return A list with \code{keywords_summary}, \code{top_keywords}, \code{status}.
#' @export
compute_m1_keywords <- function(input, config = biblio_config()) {
  if (!is.data.frame(input) || nrow(input) == 0) {
    return(list(
      keywords_summary = list(),
      top_keywords     = m1_empty_rank_table(),
      status           = "error"
    ))
  }

  # Use bibliometrix summary
  res <- bibliometrix::biblioAnalysis(input, sep = ";")
  s <- summary(res, pause = FALSE, verbose = FALSE)

  most_rel_kw <- s$MostRelKeywords

  top_n <- config$top_n_keywords

  if (!is.null(most_rel_kw) && nrow(most_rel_kw) > 0) {
    colnames(most_rel_kw) <- make.unique(colnames(most_rel_kw))
    freq_col <- if ("Occurrences" %in% colnames(most_rel_kw)) {
      "Occurrences"
    } else if ("Freq" %in% colnames(most_rel_kw)) {
      "Freq"
    } else {
      colnames(most_rel_kw)[2]
    }
    most_rel_kw[[freq_col]] <- suppressWarnings(as.numeric(most_rel_kw[[freq_col]]))
    most_rel_kw <- most_rel_kw[!is.na(most_rel_kw[[freq_col]]), ]
    most_rel_kw <- most_rel_kw[order(-most_rel_kw[[freq_col]]), ]

    kw_col <- if ("Keywords" %in% colnames(most_rel_kw)) "Keywords" else colnames(most_rel_kw)[1]

    top_keywords <- tibble::tibble(
      rank  = seq_len(min(top_n, nrow(most_rel_kw))),
      label = most_rel_kw[[kw_col]][1:min(top_n, nrow(most_rel_kw))],
      value = most_rel_kw[[freq_col]][1:min(top_n, nrow(most_rel_kw))]
    )
  } else {
    top_keywords <- m1_empty_rank_table()
  }

  keywords_summary <- list(
    total_keywords_de = sum(!is.na(input$DE) & input$DE != ""),
    total_keywords_id = sum(!is.na(input$ID) & input$ID != "")
  )

  list(
    keywords_summary = keywords_summary,
    top_keywords     = top_keywords,
    status           = "success"
  )
}
