# ============================================================================
# m3_table_citations.R - Citation tables for M3
# ============================================================================

#' Build country citation tables
#'
#' @param citation_data Output from \code{m3_compute_citations}
#' @param config A configuration list (see \code{biblio_config})
#' @return A list of tibbles: \code{full}, \code{top_total}, \code{top_avg}
#' @export
m3_table_citations <- function(citation_data, config = biblio_config()) {
  empty <- list(full = tibble::tibble(), top_total = tibble::tibble(), top_avg = tibble::tibble())
  if (!is.list(citation_data) || nrow(citation_data$country_citations) == 0) {
    return(empty)
  }

  full <- citation_data$country_citations %>%
    dplyr::mutate(average_citations = round(average_citations, 2)) %>%
    dplyr::rename(
      Country           = country,
      Articles          = article_count,
      `Total Citations` = total_citations,
      `Avg Citations`   = average_citations
    )

  top_total <- citation_data$top_countries_by_citations %>%
    dplyr::rename(Rank = rank, Country = label, `Total Citations` = value)

  top_avg <- citation_data$top_countries_by_avg_citations %>%
    dplyr::mutate(value = round(value, 2)) %>%
    dplyr::rename(Rank = rank, Country = label, `Avg Citations` = value)

  list(full = full, top_total = top_total, top_avg = top_avg)
}
