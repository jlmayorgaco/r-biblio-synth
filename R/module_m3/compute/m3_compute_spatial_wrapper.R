# ============================================================================
# m3_compute_spatial_wrapper.R - Wrapper for Spatial Analysis
# ============================================================================
# Wraps spatial analysis functions to work with prepared_data

#' Compute spatial statistics from prepared country data
#'
#' @param prepared_data Output from prepare_m3_country_data
#' @param config Configuration list
#' @return List with spatial autocorrelation results
#' @export
m3_compute_spatial <- function(prepared_data, config = biblio_config()) {
  if (is.null(prepared_data) || prepared_data$status != "success") {
    return(list(status = "error: invalid prepared_data"))
  }
  
  country_summary <- prepared_data$country_summary
  if (is.null(country_summary) || nrow(country_summary) < 4) {
    return(list(status = "error: insufficient countries for spatial analysis"))
  }
  
  results <- list()
  
  # Prepare country data for spatial analysis
  spatial_data <- data.frame(
    country = country_summary$country,
    value = country_summary$total_docs,
    stringsAsFactors = FALSE
  )
  
  # Moran's I
  results$morans_i <- m3_compute_morans_i(spatial_data)
  
  # Geary's C
  results$gearys_c <- m3_compute_gearys_c(spatial_data)
  
  # Getis-Ord Gi*
  results$getis_ord <- m3_compute_getis_ord(spatial_data)
  
  # LISA
  results$lisa <- m3_compute_lisa(spatial_data)
  
  # Regional inequality (Gini)
  results$inequality <- list(
    gini = safe_gini(spatial_data$value),
    theil = compute_theil_index(spatial_data$value),
    interpretation = paste("Spatial concentration:", 
                          ifelse(results$morans_i$statistic > 0, 
                                 "positive spatial autocorrelation",
                                 "negative spatial autocorrelation"))
  )
  
  results$status <- "success"
  results
}

#' Compute Theil index
#' @keywords internal
compute_theil_index <- function(x) {
  x <- x[!is.na(x) & x > 0]
  n <- length(x)
  if (n < 2) return(0)
  
  mean_x <- mean(x)
  theil <- sum((x / mean_x) * log(x / mean_x)) / n
  theil
}

#' Compute regional analysis from prepared country data
#'
#' @param prepared_data Output from prepare_m3_country_data
#' @param config Configuration list
#' @return List with regional analysis
#' @export
m3_compute_regional <- function(prepared_data, config = biblio_config()) {
  if (is.null(prepared_data) || prepared_data$status != "success") {
    return(list(status = "error: invalid prepared_data"))
  }
  
  country_summary <- prepared_data$country_summary
  
  # Aggregate by continent
  by_continent <- m3_aggregate_by_region(country_summary, group_by = "continent")
  
  # Aggregate by OECD status
  by_oecd <- m3_aggregate_by_region(country_summary, group_by = "oecd")
  
  # Aggregate by BRICS status
  by_brics <- m3_aggregate_by_region(country_summary, group_by = "brics")
  
  # Aggregate by EU status
  by_eu <- m3_aggregate_by_region(country_summary, group_by = "eu")
  
  # Regional comparisons
  continent_comparison <- m3_regional_comparison(by_continent)
  
  list(
    by_continent = by_continent,
    by_oecd = by_oecd,
    by_brics = by_brics,
    by_eu = by_eu,
    continent_comparison = continent_comparison,
    status = "success"
  )
}