# ============================================================================
# m1_compute_citations.R - Citations metric for M1
# ============================================================================

#' @export
compute_m1_citations <- function(input, config = biblio_config()) {
  if (!is.data.frame(input) || nrow(input) == 0) {
    return(list(top_cited_documents = m1_empty_rank_table(), citations_per_year = m1_empty_rank_table(),
                citation_summary = list(), status = "error"))
  }

  top_n <- config$top_n_default

  # Try bibliometrix first
  cit_data <- tryCatch({
    res <- bibliometrix::biblioAnalysis(input, sep = ";")
    s <- summary(res, pause = FALSE, verbose = FALSE)
    mc <- s$MostCitedPapers
    colnames(mc) <- make.unique(colnames(mc))
    mc
  }, error = function(e) {
    # Fallback: use TC column directly
    if ("TC" %in% names(input) && "TI" %in% names(input)) {
      mc <- data.frame(
        Paper = input$TI,
        `Total Citations` = as.numeric(input$TC),
        DOI = if ("DI" %in% names(input)) input$DI else NA_character_,
        stringsAsFactors = FALSE
      )
      mc <- mc[order(-mc[[2]]), ]
      mc
    } else {
      NULL
    }
  })

  if (!is.null(cit_data) && nrow(cit_data) > 0) {
    cit_col <- if ("Total.Citations" %in% colnames(cit_data)) "Total.Citations"
               else if ("Total Citations" %in% colnames(cit_data)) "Total Citations"
               else if ("TC" %in% colnames(cit_data)) "TC"
               else colnames(cit_data)[2]

    cit_data[[cit_col]] <- suppressWarnings(as.numeric(cit_data[[cit_col]]))
    cit_data <- cit_data[!is.na(cit_data[[cit_col]]), ]
    cit_data <- cit_data[order(-cit_data[[cit_col]]), ]

    # PaperID
    if ("DOI" %in% colnames(cit_data) && "DI" %in% names(input)) {
      cit_data$PaperID <- vapply(cit_data$DOI, function(doi) {
        idx <- which(input$DI == doi)
        if (length(idx) > 0) paste0("[", idx[1], "]") else NA_character_
      }, character(1))
    } else {
      cit_data$PaperID <- paste0("[", seq_len(nrow(cit_data)), "]")
    }

    top_cited <- tibble::tibble(
      rank  = seq_len(min(top_n, nrow(cit_data))),
      label = cit_data$PaperID[1:min(top_n, nrow(cit_data))],
      value = cit_data[[cit_col]][1:min(top_n, nrow(cit_data))]
    )

    # TCperYear
    if ("TCperYear" %in% colnames(cit_data)) {
      cit_data$TCperYear <- suppressWarnings(as.numeric(cit_data$TCperYear))
      citations_per_year <- tibble::tibble(
        rank  = seq_len(min(top_n, nrow(cit_data))),
        label = cit_data$PaperID[1:min(top_n, nrow(cit_data))],
        value = cit_data$TCperYear[1:min(top_n, nrow(cit_data))]
      )
    } else {
      citations_per_year <- m1_empty_rank_table()
    }
  } else {
    top_cited <- m1_empty_rank_table()
    citations_per_year <- m1_empty_rank_table()
  }

  total_cit <- if ("TC" %in% names(input)) sum(as.numeric(input$TC), na.rm = TRUE) else 0
  mean_cit <- if ("TC" %in% names(input)) mean(as.numeric(input$TC), na.rm = TRUE) else 0

  list(
    top_cited_documents = top_cited,
    citations_per_year  = citations_per_year,
    citation_summary    = list(total_citations = total_cit, mean_citations = round(mean_cit, 2)),
    status              = "success"
  )
}
