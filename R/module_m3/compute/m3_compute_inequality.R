# ============================================================================
# m3_compute_inequality.R - Inequality/concentration metrics for M3
# ============================================================================

#' Compute inequality and concentration metrics for country production and citations
#'
#' @param prepared_data Output from \code{prepare_m3_country_data}
#' @param config A configuration list (see \code{biblio_config})
#' @return A list containing inequality metrics for production and citations
#' @export
m3_compute_inequality <- function(prepared_data, config = biblio_config()) {
  # We'll compute inequality for production (article_count) and citations (total_citations)
  # from the country summary.
  
  country_summary <- prepared_data$country_summary
  
  if (nrow(country_summary) == 0) {
    return(list(
      production_inequality = tibble::tibble(),
      citations_inequality = tibble::tibble(),
      inequality_summary = list(
        production = list(
          gini = NA_real_,
          hhi = NA_real_,
          entropy = NA_real_,
          top5_share = NA_real_,
          top10_share = NA_real_
        ),
        citations = list(
          gini = NA_real_,
          hhi = NA_real_,
          entropy = NA_real_,
          top5_share = NA_real_,
          top10_share = NA_real_
        )
      ),
      status = "error: no country data"
    ))
  }
  
  # Ensure we have the necessary columns
  if (!"article_count" %in% names(country_summary)) {
    country_summary <- country_summary %>%
      dplyr::mutate(article_count = 0)
  }
  if (!"total_citations" %in% names(country_summary)) {
    country_summary <- country_summary %>%
      dplyr::mutate(total_citations = 0)
  }
  
  # Helper function to compute inequality metrics for a numeric vector
  compute_inequality_metrics <- function(values) {
    # Remove zeros? We'll keep zeros as they represent countries with zero output.
    # However, for Gini, zeros are fine.
    # We'll sort ascending for Lorenz.
    sorted_values <- sort(values)
    n <- length(sorted_values)
    if (n == 0) {
      return(list(
        gini = NA_real_,
        hhi = NA_real_,
        entropy = NA_real_,
        top5_share = NA_real_,
        top10_share = NA_real_,
        lorenz_cumulative_entities = numeric(0),
        lorenz_cumulative_values = numeric(0)
      ))
    }
    
    # Total sum
    total <- sum(sorted_values)
    if (total == 0) {
      # All zeros
      return(list(
        gini = 0, # Perfect equality? Actually, if all zeros, inequality is undefined but we can set to 0.
        hhi = 0,
        entropy = 0,
        top5_share = 0,
        top10_share = 0,
        lorenz_cumulative_entities = (1:n)/n,
        lorenz_cumulative_values = rep(0, n)
      ))
    }
    
    # Cumulative proportions
    cum_prop_entities <- (1:n) / n
    cum_prop_values <- cumsum(sorted_values) / total
    
    # Gini: 1 - 2 * area under Lorenz curve (trapezoidal rule)
    auc <- 0
    for (i in 2:n) {
      auc <- auc + (cum_prop_entities[i] - cum_prop_entities[i-1]) * (cum_prop_values[i] + cum_prop_values[i-1]) / 2
    }
    gini <- 1 - 2 * auc
    
    # Herfindahl-Hirschman Index (HHI): sum of squared shares
    shares <- sorted_values / total
    hhi <- sum(shares^2)
    
    # Entropy: -sum(shares * log(shares)), with 0*log(0) = 0
    entropy <- -sum(shares * log(shares), na.rm = TRUE)
    # Normalized entropy (by log(n)) if n>1
    entropy_normalized <- if (n > 1) entropy / log(n) else NA_real_
    
    # Top-k concentration shares
    top5_share <- if (n >= 5) sum(sorted_values[(n-4):n]) / total else sum(sorted_values) / total
    top10_share <- if (n >= 10) sum(sorted_values[(n-9):n]) / total else sum(sorted_values) / total
    
    return(list(
      gini = gini,
      hhi = hhi,
      entropy = entropy_normalized, # returning normalized entropy
      top5_share = top5_share,
      top10_share = top10_share,
      lorenz_cumulative_entities = cum_prop_entities,
      lorenz_cumulative_values = cum_prop_values
    ))
  }
  
  # Compute for production
  prod_metrics <- compute_inequality_metrics(country_summary$article_count)
  
  # Compute for citations
  cit_metrics <- compute_inequality_metrics(country_summary$total_citations)
  
  # Prepare Lorenz curve data for production (for plotting)
  production_inequality <- tibble::tibble(
    cumulative_entities = prod_metrics$lorenz_cumulative_entities,
    cumulative_values = prod_metrics$lorenz_cumulative_values,
    type = "production"
  )
  
  # Prepare Lorenz curve data for citations
  citations_inequality <- tibble::tibble(
    cumulative_entities = cit_metrics$lorenz_cumulative_entities,
    cumulative_values = cit_metrics$lorenz_cumulative_values,
    type = "citations"
  )
  
  # Inequality summary
  inequality_summary <- list(
    production = list(
      gini = prod_metrics$gini,
      hhi = prod_metrics$hhi,
      entropy = prod_metrics$entropy,
      top5_share = prod_metrics$top5_share,
      top10_share = prod_metrics$top10_share
    ),
    citations = list(
      gini = cit_metrics$gini,
      hhi = cit_metrics$hhi,
      entropy = cit_metrics$entropy,
      top5_share = cit_metrics$top5_share,
      top10_share = cit_metrics$top10_share
    )
  )
  
  return(list(
    production_inequality = production_inequality,
    citations_inequality = citations_inequality,
    inequality_summary = inequality_summary,
    status = "success"
  ))
}