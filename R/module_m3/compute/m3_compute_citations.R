# ============================================================================
# m3_compute_citations.R - Citation metrics for M3
# ============================================================================

#' Compute country citation metrics
#'
#' @param prepared_data Output from \code{prepare_m3_country_data}
#' @param config A configuration list (see \code{biblio_config})
#' @return A list containing citation metrics
#' @export
m3_compute_citations <- function(prepared_data, config = biblio_config()) {
  # Extract the country summary (which includes article_count and total_citations)
  country_summary <- prepared_data$country_summary
  
  if (nrow(country_summary) == 0) {
    return(list(
      country_citations = tibble::tibble(),
      top_countries_by_citations = tibble::tibble(),
      top_countries_by_avg_citations = tibble::tibble(),
      citation_summary = list(
        total_citations = 0,
        total_countries = 0,
        gini_citations = NA_real_
      ),
      status = "error: no country data"
    ))
  }
  
  # Ensure we have total_citations column (should be present from preparation)
  if (!"total_citations" %in% names(country_summary)) {
    # If for some reason it's missing, set to 0
    country_summary <- country_summary %>%
      dplyr::mutate(total_citations = 0)
  }
  
  # Compute average citations per country
  country_summary <- country_summary %>%
    dplyr::mutate(
      average_citations = ifelse(article_count > 0, total_citations / article_count, 0)
    )
  
  # Total citations and number of countries
  total_citations <- sum(country_summary$total_citations)
  total_countries <- nrow(country_summary)
  
  # Compute Gini coefficient for citations
  # Sort by total_citations ascending for Lorenz
  sorted_citations <- sort(country_summary$total_citations)
  n <- length(sorted_citations)
  if (n == 0) {
    gini_citations <- NA_real_
  } else {
    # Cumulative proportion of countries
    cum_prop_countries <- (1:n) / n
    # Cumulative proportion of citations
    cum_prop_citations <- cumsum(sorted_citations) / sum(sorted_citations)
    # Area under Lorenz curve (trapezoidal rule)
    auc <- 0
    for (i in 2:n) {
      auc <- auc + (cum_prop_countries[i] - cum_prop_countries[i-1]) * (cum_prop_citations[i] + cum_prop_citations[i-1]) / 2
    }
    gini_citations <- 1 - 2 * auc
  }
  
  # Prepare the country citations table (full table with averages)
  country_citations <- country_summary %>%
    dplyr::arrange(desc(total_citations)) %>%
    dplyr::transmute(
      country = country,
      article_count = article_count,
      total_citations = total_citations,
      average_citations = average_citations
    )
  
  # Top N countries by total citations
  top_n <- config$top_n_countries
  top_countries_by_citations <- country_summary %>%
    dplyr::arrange(desc(total_citations)) %>%
    dplyr::slice(1:min(top_n, nrow(.))) %>%
    dplyr::transmute(
      rank = dplyr::row_number(),
      label = country,
      value = total_citations
    )
  
  # Top N countries by average citations (only those with at least 1 article, which is all)
  top_countries_by_avg_citations <- country_summary %>%
    dplyr::arrange(desc(average_citations)) %>%
    dplyr::slice(1:min(top_n, nrow(.))) %>%
    dplyr::transmute(
      rank = dplyr::row_number(),
      label = country,
      value = average_citations
    )
  
  # Citation summary
  citation_summary <- list(
    total_citations = total_citations,
    total_countries = total_countries,
    gini_citations = gini_citations
  )
  
  return(list(
    country_citations = country_citations,
    top_countries_by_citations = top_countries_by_citations,
    top_countries_by_avg_citations = top_countries_by_avg_citations,
    citation_summary = citation_summary,
    status = "success"
  ))
}