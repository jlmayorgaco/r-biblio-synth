# ============================================================================
# m3_compute_distribution_tests.R - Distribution tests for M3
# ============================================================================

#' Compute distribution tests and statistical diagnostics for country-level data
#'
#' @param prepared_data Output from \code{prepare_m3_country_data}
#' @param config A configuration list (see \code{biblio_config})
#' @return A list containing distribution diagnostics
#' @export
m3_compute_distribution_tests <- function(prepared_data, config = biblio_config()) {
  country_summary <- prepared_data$country_summary
  
  if (nrow(country_summary) == 0) {
    return(list(
      production_distribution = tibble::tibble(),
      citations_distribution = tibble::tibble(),
      distribution_summary = list(
        production = list(
          n = 0,
          mean = NA_real_,
          median = NA_real_,
          sd = NA_real_,
          skewness = NA_real_,
          kurtosis = NA_real_,
          shapiro_p = NA_real_,
          cv = NA_real_
        ),
        citations = list(
          n = 0,
          mean = NA_real_,
          median = NA_real_,
          sd = NA_real_,
          skewness = NA_real_,
          kurtosis = NA_real_,
          shapiro_p = NA_real_,
          cv = NA_real_
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
  
  # Helper function to compute distribution statistics for a numeric vector
  compute_distribution_stats <- function(values, variable_name) {
    # Remove NA
    values <- values[!is.na(values)]
    n <- length(values)
    if (n == 0) {
      return(list(
        n = 0,
        mean = NA_real_,
        median = NA_real_,
        sd = NA_real_,
        skewness = NA_real_,
        kurtosis = NA_real_,
        shapiro_p = NA_real_,
        cv = NA_real_
      ))
    }
    
    mean_val <- mean(values)
    median_val <- median(values)
    sd_val <- sd(values)
    # Skewness and kurtosis using moments (if package available, but we'll implement manually)
    # We'll use the definitions: skewness = E[(x - mu)^3] / sigma^3, kurtosis = E[(x - mu)^4] / sigma^4 - 3
    if (!is.finite(sd_val) || sd_val == 0) {
      skewness <- 0
      kurtosis <- -3
    } else {
      skewness <- mean((values - mean_val)^3) / (sd_val^3)
      kurtosis <- mean((values - mean_val)^4) / (sd_val^4) - 3
    }
    
    # Shapiro-Wilk test (only if n between 3 and 5000)
    shapiro_p <- NA_real_
    if (n >= 3 && n <= 5000) {
      shapiro_test <- tryCatch({
        shapiro.test(values)
      }, error = function(e) {
        list(p.value = NA_real_)
      })
      if (!is.null(shapiro_test$p.value)) {
        shapiro_p <- shapiro_test$p.value
      }
    }
    
    cv <- if (is.finite(mean_val) && is.finite(sd_val) && mean_val != 0) sd_val / mean_val else NA_real_
    
    return(list(
      n = n,
      mean = mean_val,
      median = median_val,
      sd = sd_val,
      skewness = skewness,
      kurtosis = kurtosis,
      shapiro_p = shapiro_p,
      cv = cv
    ))
  }
  
  # Compute for production
  prod_stats <- compute_distribution_stats(country_summary$article_count, "production")
  
  # Compute for citations
  cit_stats <- compute_distribution_stats(country_summary$total_citations, "citations")
  
  # Prepare distribution tables (for each country, we can return the statistics? 
  # But the requirement is to return a table. We'll return a table with the statistics for each variable.
  # We'll create a tibble with one row per variable and the statistics.
  production_distribution <- tibble::tibble(
    variable = "article_count",
    n = prod_stats$n,
    mean = prod_stats$mean,
    median = prod_stats$median,
    sd = prod_stats$sd,
    skewness = prod_stats$skewness,
    kurtosis = prod_stats$kurtosis,
    shapiro_p = prod_stats$shapiro_p,
    cv = prod_stats$cv
  )
  
  citations_distribution <- tibble::tibble(
    variable = "total_citations",
    n = cit_stats$n,
    mean = cit_stats$mean,
    median = cit_stats$median,
    sd = cit_stats$sd,
    skewness = cit_stats$skewness,
    kurtosis = cit_stats$kurtosis,
    shapiro_p = cit_stats$shapiro_p,
    cv = cit_stats$cv
  )
  
  # Distribution summary
  distribution_summary <- list(
    production = prod_stats,
    citations = cit_stats
  )
  
  return(list(
    production_distribution = production_distribution,
    citations_distribution = citations_distribution,
    distribution_summary = distribution_summary,
    status = "success"
  ))
}
