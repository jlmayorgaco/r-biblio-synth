# ============================================================================
# m3_compute_growth_dynamics.R - Growth dynamics for M3
# ============================================================================

#' Compute growth dynamics (temporal trends) for country-level data
#'
#' @param prepared_data Output from \code{prepare_m3_country_data}
#' @param config A configuration list (see \code{biblio_config})
#' @return A list containing growth dynamics metrics
#' @export
m3_compute_growth_dynamics <- function(prepared_data, config = biblio_config()) {
  # We need annual data for growth dynamics.
  country_annual <- prepared_data$country_annual
  country_annual_citations <- prepared_data$country_annual_citations
  
  # If we don't have annual data, we return an empty result with a note.
  if (nrow(country_annual) == 0) {
    return(list(
      annual_productivity = tibble::tibble(),
      annual_citations = tibble::tibble(),
      growth_summary = list(
        productivity = list(
          available_years = 0,
          cagr = NA_real_,
          slope = NA_real_,
          growth_rate_last_year = NA_real_
        ),
        citations = list(
          available_years = 0,
          cagr = NA_real_,
          slope = NA_real_,
          growth_rate_last_year = NA_real_
        )
      ),
      status = "success: no annual data available"
    ))
  }
  
  # We'll compute growth dynamics for productivity (article count) and, if available, citations.
  # We'll focus on the top N countries by total productivity over the entire period.
  
  # First, compute total productivity per country over all years to get the top countries.
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
  
  # Helper to compute growth metrics for a time series of a country
  compute_country_growth <- function(df, value_col) {
    # df should have columns: country, PY, and the value column
    # We'll compute for each country:
    #   - CAGR: (end_value/start_value)^(1/(n_years-1)) - 1, if start_value > 0
    #   - Slope of a linear regression (log(value) ~ year) if we want to model exponential growth, but we'll do simple linear slope of value over time.
    #   - Growth rate last year: (value_t - value_{t-1}) / value_{t-1}
    # We'll return a tibble with one row per country.
    
    # We'll nest by country and compute.
    df %>%
      dplyr::group_by(country) %>%
      dplyr::arrange(PY) %>%
      dplyr::summarise(
        n_years = dplyr::n(),
        start_value = first(!!sym(value_col)),
        end_value = last(!!sym(value_col)),
        # CAGR
        cagr = ifelse(n_years > 1 && start_value > 0, (end_value/start_value)^(1/(n_years-1)) - 1, NA_real_),
        # Slope of value over year (linear regression)
        slope = ifelse(n_years > 1, {
          model <- lm(!!sym(value_col) ~ PY, data = cur_data())
          coef(model)[2]
        }, NA_real_),
        # Growth rate last year
        growth_rate_last_year = ifelse(n_years > 1 && lag(!!sym(value_col))[n_years] != 0,
                                       (last(!!sym(value_col)) - lag(!!sym(value_col))[n_years]) / lag(!!sym(value_col))[n_years],
                                       NA_real_)
      ) %>%
      dplyr::ungroup()
  }
  
  # Compute for productivity
  if (nrow(annual_productivity_top) > 0) {
    productivity_growth <- compute_country_growth(annual_productivity_top, "article_count")
  } else {
    productivity_growth <- tibble::tibble(
      country = character(),
      n_years = integer(),
      start_value = double(),
      end_value = double(),
      cagr = double(),
      slope = double(),
      growth_rate_last_year = double()
    )
  }
  
  # Compute for citations if data available
  if (nrow(annual_citations_top) > 0) {
    citations_growth <- compute_country_growth(annual_citations_top, "total_citations")
  } else {
    citations_growth <- tibble::tibble(
      country = character(),
      n_years = integer(),
      start_value = double(),
      end_value = double(),
      cagr = double(),
      slope = double(),
      growth_rate_last_year = double()
    )
  }
  
  # Prepare the annual data for return (we'll return the top countries annual data)
  annual_productivity <- annual_productivity_top %>%
    dplyr::select(country, PY, article_count)
  
  annual_citations <- if (nrow(annual_citations_top) > 0) {
    annual_citations_top %>%
      dplyr::select(country, PY, total_citations)
  } else {
    tibble::tibble(
      country = character(),
      PY = integer(),
      total_citations = double()
    )
  }
  
  # Growth summary (we can summarize across countries, e.g., median CAGR)
  growth_summary <- list(
    productivity = list(
      available_years = if (nrow(annual_productivity) > 0) max(annual_productivity$PY) - min(annual_productivity$PY) + 1 else 0,
      median_cagr = if (nrow(productivity_growth) > 0) median(productivity_growth$cagr, na.rm = TRUE) else NA_real_,
      median_slope = if (nrow(productivity_growth) > 0) median(productivity_growth$slope, na.rm = TRUE) else NA_real_
    ),
    citations = list(
      available_years = if (nrow(annual_citations) > 0) max(annual_citations$PY) - min(annual_citations$PY) + 1 else 0,
      median_cagr = if (nrow(citations_growth) > 0) median(citations_growth$cagr, na.rm = TRUE) else NA_real_,
      median_slope = if (nrow(citations_growth) > 0) median(citations_growth$slope, na.rm = TRUE) else NA_real_
    )
  )
  
  return(list(
    annual_productivity = annual_productivity,
    annual_citations = annual_citations,
    productivity_growth = productivity_growth,
    citations_growth = citations_growth,
    growth_summary = growth_summary,
    status = "success"
  ))
}