# ============================================================================
# m3_table_growth_dynamics.R - Growth dynamics tables for M3
# ============================================================================

#' Build growth dynamics tables
#'
#' @param growth_data Output from \code{m3_compute_growth_dynamics}
#' @param config A configuration list (see \code{biblio_config})
#' @return A list of tibbles: \code{productivity_growth}, \code{citations_growth}
#' @export
m3_table_growth_dynamics <- function(growth_data, config = biblio_config()) {
  empty <- list(productivity_growth = tibble::tibble(), citations_growth = tibble::tibble())
  if (!is.list(growth_data)) return(empty)

  .clean_growth <- function(df) {
    if (!is.data.frame(df) || nrow(df) == 0) return(tibble::tibble())
    df %>%
      dplyr::mutate(
        cagr                    = round(cagr * 100, 2),
        slope                   = round(slope, 3),
        growth_rate_last_year   = round(growth_rate_last_year * 100, 2)
      ) %>%
      dplyr::rename(
        Country              = country,
        `Years`              = n_years,
        `Start Value`        = start_value,
        `End Value`          = end_value,
        `CAGR (%)`           = cagr,
        `Trend Slope`        = slope,
        `Last-Year Growth (%)` = growth_rate_last_year
      )
  }

  list(
    productivity_growth = .clean_growth(growth_data$productivity_growth),
    citations_growth    = .clean_growth(growth_data$citations_growth)
  )
}
