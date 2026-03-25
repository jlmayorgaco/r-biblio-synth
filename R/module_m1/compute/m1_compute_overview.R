# ============================================================================
# m1_compute_overview.R - Overview metric for M1
# ============================================================================

#' Compute M1 overview
#'
#' @param input A data frame of bibliographic data.
#' @param config A configuration list.
#' @return A list with summary_table, main_indicators, status.
#' @export
compute_m1_overview <- function(input, config = biblio_config()) {
  if (!is.data.frame(input) || nrow(input) == 0) {
    return(list(
      summary_table   = m1_empty_summary_table(),
      main_indicators = list(),
      status          = "error"
    ))
  }

  # Try bibliometrix first, fallback to manual computation
  main_indicators <- tryCatch({
    res <- bibliometrix::biblioAnalysis(input, sep = ";")
    s <- summary(res, pause = FALSE, verbose = FALSE)
    m1_extract_main_information(s$MainInformationDF)
  }, error = function(e) {
    # Fallback: manual computation
    m1_compute_overview_manual(input)
  })

  # Build summary table
  summary_table <- tibble::tibble(
    metric = c("Documents", "Sources", "Authors", "Timespan"),
    value = c(
      main_indicators$documents,
      main_indicators$sources,
      main_indicators$authors,
      main_indicators$timespan
    )
  )

  list(
    summary_table   = summary_table,
    main_indicators = main_indicators,
    status          = "success"
  )
}

#' Manual overview computation fallback
#' @param input A data frame.
#' @return A list of indicators.
m1_compute_overview_manual <- function(input) {
  n_docs <- nrow(input)
  n_sources <- if ("SO" %in% names(input)) length(unique(input$SO)) else NA
  n_authors <- if ("AU" %in% names(input)) length(m1_extract_authors(input)) else NA
  years <- if ("PY" %in% names(input)) as.integer(input$PY) else NA
  timespan <- if (all(!is.na(years))) paste(min(years, na.rm = TRUE), max(years, na.rm = TRUE), sep = "-") else NA

  list(
    timespan                      = as.character(timespan),
    sources                       = as.integer(n_sources),
    documents                     = as.integer(n_docs),
    annual_growth_rate             = NA_character_,
    document_average_age           = NA_character_,
    avg_citations_per_doc          = if ("TC" %in% names(input)) as.character(round(mean(as.numeric(input$TC), na.rm = TRUE), 2)) else NA_character_,
    avg_citations_per_year_per_doc = NA_character_,
    references                    = NA_character_,
    authors                       = as.integer(n_authors),
    authors_per_doc               = NA_character_,
    co_authors_per_doc            = NA_character_,
    international_collaborations  = NA_character_,
    single_author_documents       = NA_character_,
    multi_author_documents        = NA_character_
  )
}
