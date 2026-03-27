# ============================================================================
# m3_compute_change_points.R - Change-point analysis for M3
# ============================================================================

#' Compute change-point analysis on country-level time series
#'
#' @param prepared_data Output from \code{prepare_m3_country_data}
#' @param config A configuration list (see \code{biblio_config})
#' @return A list containing change-point metrics
#' @export
m3_compute_change_points <- function(prepared_data, config = biblio_config()) {
  # We need annual data for change-point analysis.
  country_annual <- prepared_data$country_annual
  country_annual_citations <- prepared_data$country_annual_citations
  
  # If we don't have annual data, we return an empty result.
  if (nrow(country_annual) == 0) {
    return(list(
      productivity_change_points = tibble::tibble(),
      citations_change_points = tibble::tibble(),
      change_point_summary = list(
        productivity = list(
          n_countries_analyzed = 0,
          n_change_points = 0
        ),
        citations = list(
          n_countries_analyzed = 0,
          n_change_points = 0
        )
      ),
      status = "success: no annual data available"
    ))
  }
  
  # We'll analyze the top N countries by total productivity over the entire period.
  total_prod_per_country <- country_annual %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(
      total_articles = sum(article_count),
      .groups = "drop"
    ) %>%
    dplyr::arrange(desc(total_articles))
  
  top_n <- config$top_n_countries
  top_countries <- total_prod_per_country %>%
    dplyr::slice(1:min(top_n, nrow(.))) %>%
    dplyr::pull(country)
  
  # Filter annual data for top countries
  annual_productivity_top <- country_annual %>%
    dplyr::filter(country %in% top_countries) %>%
    dplyr::arrange(country, PY)
  
  # For citations, if we have the annual citations data
  if (nrow(country_annual_citations) > 0) {
    annual_citations_top <- country_annual_citations %>%
      dplyr::filter(country %in% top_countries) %>%
      dplyr::arrange(country, PY)
  } else {
    annual_citations_top <- tibble::tibble()
  }
  
  # Helper to detect change points in a time series for a country
  # We'll use a simple method: compare the mean of the first half and second half.
  # If the absolute difference exceeds a threshold (e.g., 1 standard deviation of the series), we flag a change point at the midpoint.
  # This is very simplistic and exploratory.
  # We'll only do this if the time series has at least 4 points.
  detect_change_point <- function(df, value_col) {
    # df should have columns: country, PY, and the value column
    # We'll return a tibble with one row per country that has a change point, or empty.
    
    results <- list()
    
    for (country in unique(df$country)) {
      country_data <- df %>%
        dplyr::filter(country == !!country) %>%
        dplyr::arrange(PY)
      
      n <- nrow(country_data)
      if (n < 4) {
        next
      }
      
      values <- country_data[[value_col]]
      split <- ceiling(n/2)
      first_half <- values[1:split]
      second_half <- values[(split+1):n]
      
      mean_first <- mean(first_half)
      mean_second <- mean(second_half)
      sd_total <- sd(values)
      
      if (is.na(sd_total) || sd_total == 0) {
        next
      }
      
      diff <- abs(mean_second - mean_first)
      if (diff > sd_total) {
        # Change point at the split
        cp_year <- if (split < n) {
          # We'll take the year at the split (the last year of the first half)
          country_data$PY[split]
        } else {
          NA_integer_
        }
        results[[country]] <- tibble::tibble(
          country = country,
          change_point_year = cp_year,
          mean_before = mean_first,
          mean_after = mean_second,
          absolute_difference = diff
        )
      }
    }
    
    if (length(results) == 0) {
      return(tibble::tibble())
    } else {
      return(dplyr::bind_rows(results))
    }
  }
  
  # Detect change points for productivity
  if (nrow(annual_productivity_top) > 0) {
    productivity_cp <- detect_change_point(annual_productivity_top, "article_count")
  } else {
    productivity_cp <- tibble::tibble()
  }
  
  # Detect change points for citations if data available
  if (nrow(annual_citations_top) > 0) {
    citations_cp <- detect_change_point(annual_citations_top, "total_citations")
  } else {
    citations_cp <- tibble::tibble()
  }
  
  # Change point summary
  change_point_summary <- list(
    productivity = list(
      n_countries_analyzed = length(top_countries),
      n_change_points = nrow(productivity_cp)
    ),
    citations = list(
      n_countries_analyzed = length(top_countries),
      n_change_points = nrow(citations_cp)
    )
  )
  
  return(list(
    productivity_change_points = productivity_cp,
    citations_change_points = citations_cp,
    change_point_summary = change_point_summary,
    status = "success"
  ))
}