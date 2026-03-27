# ============================================================================
# m3_compute_production.R - Production metrics for M3
# ============================================================================

#' Compute country production metrics
#'
#' @param prepared_data Output from \code{prepare_m3_country_data}
#' @param config A configuration list (see \code{biblio_config})
#' @return A list containing production metrics
#' @export
m3_compute_production <- function(prepared_data, config = biblio_config()) {
  # Extract the country summary (total articles per country)
  country_summary <- prepared_data$country_summary
  
  if (nrow(country_summary) == 0) {
    return(list(
      country_production = tibble::tibble(),
      top_countries_by_production = tibble::tibble(),
      production_summary = list(
        total_articles = 0,
        total_countries = 0,
        gini_articles = NA_real_
      ),
      status = "error: no country data"
    ))
  }
  
  # Total articles and number of countries
  total_articles <- sum(country_summary$article_count)
  total_countries <- nrow(country_summary)
  
  # Calculate share of total articles
  country_summary <- country_summary %>%
    dplyr::mutate(
      share = article_count / total_articles,
      cumulative_share = cumsum(share)
    )
  
  # Top N countries by production
  top_n <- config$top_n_countries
  top_countries_by_production <- country_summary %>%
    dplyr::arrange(desc(article_count)) %>%
    dplyr::slice(1:min(top_n, nrow(.))) %>%
    dplyr::transmute(
      rank = dplyr::row_number(),
      label = country,
      value = article_count,
      share = share,
      cumulative_share = cumulative_share
    )
  
  # Compute Gini coefficient for production (using the Lorenz curve)
  # We'll use the same helper as in M1 if available, or we can define our own.
  # We'll create a helper function for Lorenz and Gini in the utils, but for now we can use the M1 one or define locally.
  # Since we are in a different module, we'll define a helper here or use the one from core if available.
  # Let's check if there is a shared function for Lorenz and Gini. We'll create one in utils if not.
  # For now, we'll copy the logic from M1 or create a simple one.
  
  # We'll create a function to compute Lorenz and Gini in the utils, but we haven't created it yet.
  # We'll do it in the utils file. However, to avoid dependency, we can compute it here and then move it to utils later.
  # Let's compute the Lorenz curve points and Gini.
  
  # Sort by article count ascending for Lorenz
  sorted_counts <- sort(country_summary$article_count)
  n <- length(sorted_counts)
  if (n == 0) {
    gini_articles <- NA_real_
  } else {
    # Cumulative proportion of countries
    cum_prop_countries <- (1:n) / n
    # Cumulative proportion of articles
    cum_prop_articles <- cumsum(sorted_counts) / sum(sorted_counts)
    # Lorenz curve: (cum_prop_countries, cum_prop_articles)
    # Gini: 1 - 2 * integral of Lorenz curve (approximated by trapezoidal rule)
    # We'll compute the area under the Lorenz curve using the trapezoidal rule
    auc <- 0
    for (i in 2:n) {
      auc <- auc + (cum_prop_countries[i] - cum_prop_countries[i-1]) * (cum_prop_articles[i] + cum_prop_articles[i-1]) / 2
    }
    gini_articles <- 1 - 2 * auc
  }
  
  # Prepare the country production table (full table with shares)
  country_production <- country_summary %>%
    dplyr::arrange(desc(article_count)) %>%
    dplyr::transmute(
      country = country,
      article_count = article_count,
      share = share,
      cumulative_share = cumulative_share
    )
  
  # Production summary
  production_summary <- list(
    total_articles = total_articles,
    total_countries = total_countries,
    gini_articles = gini_articles
  )
  
  return(list(
    country_production = country_production,
    top_countries_by_production = top_countries_by_production,
    production_summary = production_summary,
    status = "success"
  ))
}