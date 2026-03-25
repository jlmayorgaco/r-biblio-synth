# ============================================================================
# m1_compute_overview.R - Overview metric for M1
# ============================================================================

#' Compute M1 overview
#'
#' Uses bibliometrix to extract main information from bibliographic data.
#'
#' @param input A data frame of bibliographic data.
#' @param config A configuration list.
#' @return A list with \code{summary_table}, \code{main_indicators}, \code{status}.
#' @export
compute_m1_overview <- function(input, config = biblio_config()) {
  if (!is.data.frame(input) || nrow(input) == 0) {
    return(list(
      summary_table   = m1_empty_summary_table(),
      main_indicators = list(),
      status          = "error"
    ))
  }

  # Run bibliometrix analysis
  res <- bibliometrix::biblioAnalysis(input, sep = ";")
  s <- summary(res, pause = FALSE, verbose = FALSE)

  # Extract indicators
  summary_df <- s$MainInformationDF
  main_indicators <- m1_extract_main_information(summary_df)

  # Build summary table
  summary_table <- tibble::tibble(
    metric = c(
      "Timespan", "Documents", "Sources", "Annual Growth Rate %",
      "Avg Citations per Doc", "Avg Citations per Year per Doc",
      "Authors", "Authors per Doc", "Co-Authors per Doc",
      "International Collaborations %"
    ),
    value = c(
      main_indicators$timespan,
      main_indicators$documents,
      main_indicators$sources,
      main_indicators$annual_growth_rate,
      main_indicators$avg_citations_per_doc,
      main_indicators$avg_citations_per_year_per_doc,
      main_indicators$authors,
      main_indicators$authors_per_doc,
      main_indicators$co_authors_per_doc,
      main_indicators$international_collaborations
    )
  )

  list(
    summary_table   = summary_table,
    main_indicators = main_indicators,
    status          = "success"
  )
}
