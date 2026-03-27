# ============================================================================
# m3_table_inequality.R - Inequality summary tables for M3
# ============================================================================

#' Build inequality summary table
#'
#' @param inequality_data Output from \code{m3_compute_inequality}
#' @param config A configuration list (see \code{biblio_config})
#' @return A tibble summarizing inequality metrics for production and citations
#' @export
m3_table_inequality <- function(inequality_data, config = biblio_config()) {
  if (!is.list(inequality_data) || !is.list(inequality_data$inequality_summary)) {
    return(tibble::tibble())
  }

  s <- inequality_data$inequality_summary

  .fmt <- function(x) if (is.null(x) || is.na(x)) NA_real_ else round(as.numeric(x), 4)

  tibble::tibble(
    Metric               = c("Gini Coefficient", "HHI",
                              "Normalized Entropy", "Top-5 Share (%)", "Top-10 Share (%)"),
    Production           = c(.fmt(s$production$gini),
                              .fmt(s$production$hhi),
                              .fmt(s$production$entropy),
                              .fmt(s$production$top5_share) * 100,
                              .fmt(s$production$top10_share) * 100),
    Citations            = c(.fmt(s$citations$gini),
                              .fmt(s$citations$hhi),
                              .fmt(s$citations$entropy),
                              .fmt(s$citations$top5_share) * 100,
                              .fmt(s$citations$top10_share) * 100)
  )
}
