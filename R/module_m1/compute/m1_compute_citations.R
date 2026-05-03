# ============================================================================
# m1_compute_citations.R - Citations metric for M1
# ============================================================================

#' @export
compute_m1_citations <- function(input, config = biblio_config()) {
  if (!is.data.frame(input) || nrow(input) == 0) {
    return(list(
      top_cited_documents = m1_empty_rank_table(),
      citations_per_year = m1_empty_rank_table(),
      citation_summary = list(),
      status = "error"
    ))
  }

  top_n <- if (!is.null(config$top_n_documents)) {
    as.integer(config$top_n_documents)
  } else {
    10L
  }

  citation_df <- data.frame(
    title = as.character(input$TI %||% NA_character_),
    total_citations = suppressWarnings(as.numeric(input$TC %||% NA_real_)),
    doi = as.character(input$DI %||% NA_character_),
    source = as.character(input$SO %||% NA_character_),
    year = suppressWarnings(as.integer(input$PY %||% NA_integer_)),
    stringsAsFactors = FALSE
  )
  citation_df <- citation_df[!is.na(citation_df$total_citations), , drop = FALSE]
  if (nrow(citation_df) == 0) {
    return(list(
      top_cited_documents = m1_empty_rank_table(),
      citations_per_year = m1_empty_rank_table(),
      citation_summary = list(total_citations = 0, mean_citations = 0),
      status = "success"
    ))
  }

  citation_df$label <- m1_build_citation_display_label(
    title = citation_df$title,
    source = citation_df$source,
    year = citation_df$year,
    doi = citation_df$doi
  )
  citation_df$label_short <- ifelse(
    nchar(citation_df$label) > 48,
    paste0(substr(citation_df$label, 1, 45), "..."),
    citation_df$label
  )

  by_total <- citation_df[order(-citation_df$total_citations, citation_df$year, citation_df$title), , drop = FALSE]
  top_cited <- tibble::tibble(
    rank = seq_len(min(top_n, nrow(by_total))),
    label = by_total$label[1:min(top_n, nrow(by_total))],
    label_short = by_total$label_short[1:min(top_n, nrow(by_total))],
    value = by_total$total_citations[1:min(top_n, nrow(by_total))],
    title = by_total$title[1:min(top_n, nrow(by_total))],
    source = by_total$source[1:min(top_n, nrow(by_total))],
    year = by_total$year[1:min(top_n, nrow(by_total))],
    doi = by_total$doi[1:min(top_n, nrow(by_total))]
  )

  current_year <- suppressWarnings(max(as.integer(input$PY), na.rm = TRUE))
  if (!is.finite(current_year)) {
    current_year <- as.integer(format(Sys.Date(), "%Y"))
  }
  citation_df$age_years <- pmax(current_year - citation_df$year + 1L, 1L)
  citation_df$citations_per_year_value <- citation_df$total_citations / citation_df$age_years

  by_rate <- citation_df[order(-citation_df$citations_per_year_value, -citation_df$total_citations, citation_df$title), , drop = FALSE]
  citations_per_year <- tibble::tibble(
    rank = seq_len(min(top_n, nrow(by_rate))),
    label = by_rate$label[1:min(top_n, nrow(by_rate))],
    label_short = by_rate$label_short[1:min(top_n, nrow(by_rate))],
    value = round(by_rate$citations_per_year_value[1:min(top_n, nrow(by_rate))], 3),
    title = by_rate$title[1:min(top_n, nrow(by_rate))],
    source = by_rate$source[1:min(top_n, nrow(by_rate))],
    year = by_rate$year[1:min(top_n, nrow(by_rate))],
    doi = by_rate$doi[1:min(top_n, nrow(by_rate))]
  )

  total_cit <- sum(citation_df$total_citations, na.rm = TRUE)
  mean_cit <- mean(citation_df$total_citations, na.rm = TRUE)

  list(
    top_cited_documents = top_cited,
    citations_per_year = citations_per_year,
    citation_summary = list(
      total_citations = total_cit,
      mean_citations = round(mean_cit, 2)
    ),
    status = "success"
  )
}
