# ============================================================================
# m3_compute_rankings.R - Rank structure analysis for M3
# ============================================================================

#' Compute rank structure metrics for countries
#'
#' @param prepared_data Output from \code{prepare_m3_country_data}
#' @param config A configuration list (see \code{biblio_config})
#' @return A list containing rank structure metrics
#' @export
m3_compute_rankings <- function(prepared_data, config = biblio_config()) {
  # We'll compute rank structure for production (article_count) and citations (total_citations)
  
  country_summary <- prepared_data$country_summary
  
  if (nrow(country_summary) == 0) {
    return(list(
      production_rankings = tibble::tibble(),
      citations_rankings = tibble::tibble(),
      rank_summary = list(
        production = list(
          top_heavy = NA_real_,
          gini = NA_real_
        ),
        citations = list(
          top_heavy = NA_real_,
          gini = NA_real_
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
  
  # Helper function to compute rank structure for a numeric vector
  compute_rank_structure <- function(values) {
    # Sort descending for ranking (rank 1 is highest)
    sorted_values <- sort(values, decreasing = TRUE)
    n <- length(sorted_values)
    if (n == 0) {
      return(list(
        rankings = tibble::tibble(),
        top_heavy = NA_real_, # proportion of top 10% of countries that have 50% of the total
        gini = NA_real_
      ))
    }
    
    # Compute rankings table
    rankings <- tibble::tibble(
      rank = 1:n,
      value = sorted_values,
      cumulative_value = cumsum(sorted_values)
    ) %>%
      dplyr::mutate(
        cumulative_share = cumulative_value / sum(sorted_values)
      )
    
    # Compute top-heavy: proportion of top 10% of countries that have 50% of the total
    top_10_count <- ceiling(0.1 * n)
    if (top_10_count > 0) {
      top_10_sum <- sum(sorted_values[1:top_10_count])
      total_sum <- sum(sorted_values)
      top_heavy <- top_10_sum / total_sum
    } else {
      top_heavy <- NA_real_
    }
    
    # Compute Gini (we can reuse the inequality function, but we'll compute again for simplicity)
    # We'll use the same Lorenz and Gini as in inequality.
    # Sort ascending for Lorenz
    sorted_asc <- sort(values)
    n_asc <- length(sorted_asc)
    if (n_asc == 0) {
      gini <- NA_real_
    } else {
      total <- sum(sorted_asc)
      if (total == 0) {
        gini <- 0
      } else {
        cum_prop_entities <- (1:n_asc) / n_asc
        cum_prop_values <- cumsum(sorted_asc) / total
        auc <- 0
        for (i in 2:n_asc) {
          auc <- auc + (cum_prop_entities[i] - cum_prop_entities[i-1]) * (cum_prop_values[i] + cum_prop_values[i-1]) / 2
        }
        gini <- 1 - 2 * auc
      }
    }
    
    return(list(
      rankings = rankings,
      top_heavy = top_heavy,
      gini = gini
    ))
  }
  
  # Compute for production
  prod_rank <- compute_rank_structure(country_summary$article_count)
  
  # Compute for citations
  cit_rank <- compute_rank_structure(country_summary$total_citations)
  
  # Prepare the rankings tables (we want to return the rankings with country labels)
  # We need to join the sorted values with the country names.
  # We'll do: for production, arrange by article_count descending and assign rank.
  production_rankings <- country_summary %>%
    dplyr::arrange(desc(article_count)) %>%
    dplyr::mutate(rank = dplyr::row_number()) %>%
    dplyr::select(rank, country, article_count) %>%
    dplyr::rename(label = country, value = article_count)
  
  citations_rankings <- country_summary %>%
    dplyr::arrange(desc(total_citations)) %>%
    dplyr::mutate(rank = dplyr::row_number()) %>%
    dplyr::select(rank, country, total_citations) %>%
    dplyr::rename(label = country, value = total_citations)
  
  # Rank summary
  rank_summary <- list(
    production = list(
      top_heavy = prod_rank$top_heavy,
      gini = prod_rank$gini
    ),
    citations = list(
      top_heavy = cit_rank$top_heavy,
      gini = cit_rank$gini
    )
  )
  
  return(list(
    production_rankings = production_rankings,
    citations_rankings = citations_rankings,
    rank_summary = rank_summary,
    status = "success"
  ))
}