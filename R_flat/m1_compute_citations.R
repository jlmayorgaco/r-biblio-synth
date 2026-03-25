# ============================================================================
# m1_compute_citations.R - Citations metric for M1
# ============================================================================

#' Compute M1 citations
#'
#' @param input A data frame of bibliographic data.
#' @param config A configuration list.
#' @return A list with \code{top_cited_documents}, \code{citation_summary}, \code{status}.
#' @export
compute_m1_citations <- function(input, config = biblio_config()) {
  if (!is.data.frame(input) || nrow(input) == 0) {
    return(list(
      top_cited_documents = m1_empty_rank_table(),
      citations_per_year  = m1_empty_rank_table(),
      citation_summary    = list(),
      status              = "error"
    ))
  }

  # Use bibliometrix summary
  res <- bibliometrix::biblioAnalysis(input, sep = ";")
  s <- summary(res, pause = FALSE, verbose = FALSE)

  most_cited <- s$MostCitedPapers

  # Assign PaperID from DOI
  if (!is.null(most_cited) && nrow(most_cited) > 0) {
    colnames(most_cited) <- make.unique(colnames(most_cited))

    # Get citation column
    cit_col <- if ("Total Citations" %in% colnames(most_cited)) {
      "Total Citations"
    } else if ("TC" %in% colnames(most_cited)) {
      "TC"
    } else {
      colnames(most_cited)[2]
    }

    most_cited[[cit_col]] <- suppressWarnings(as.numeric(most_cited[[cit_col]]))
    most_cited <- most_cited[!is.na(most_cited[[cit_col]]), ]
    most_cited <- most_cited[order(-most_cited[[cit_col]]), ]

    # Assign PaperID from DOI
    if ("DOI" %in% colnames(most_cited) && "DI" %in% names(input)) {
      most_cited$PaperID <- vapply(most_cited$DOI, function(doi) {
        idx <- which(input$DI == doi)
        if (length(idx) > 0) paste0("[", idx[1], "]") else NA_character_
      }, character(1))
    } else {
      most_cited$PaperID <- paste0("[", seq_len(nrow(most_cited)), "]")
    }

    # Build top cited table
    top_n <- config$top_n_default
    top_cited <- tibble::tibble(
      rank  = seq_len(min(top_n, nrow(most_cited))),
      label = most_cited$PaperID[1:min(top_n, nrow(most_cited))],
      value = most_cited[[cit_col]][1:min(top_n, nrow(most_cited))]
    )

    # Citations per year if available
    if ("TCperYear" %in% colnames(most_cited)) {
      most_cited$TCperYear <- suppressWarnings(as.numeric(most_cited$TCperYear))
      citations_per_year <- tibble::tibble(
        rank  = seq_len(min(top_n, nrow(most_cited))),
        label = most_cited$PaperID[1:min(top_n, nrow(most_cited))],
        value = most_cited$TCperYear[1:min(top_n, nrow(most_cited))]
      )
    } else {
      citations_per_year <- m1_empty_rank_table()
    }
  } else {
    top_cited <- m1_empty_rank_table()
    citations_per_year <- m1_empty_rank_table()
  }

  # Citation summary
  if ("TC" %in% names(input)) {
    total_cit <- sum(as.numeric(input$TC), na.rm = TRUE)
    mean_cit <- mean(as.numeric(input$TC), na.rm = TRUE)
  } else {
    total_cit <- 0
    mean_cit <- 0
  }

  citation_summary <- list(
    total_citations = total_cit,
    mean_citations  = round(mean_cit, 2)
  )

  list(
    top_cited_documents = top_cited,
    citations_per_year  = citations_per_year,
    citation_summary    = citation_summary,
    status              = "success"
  )
}
