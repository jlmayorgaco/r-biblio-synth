# ============================================================================
# m3_table_distribution_tests.R - Distribution diagnostics tables for M3
# ============================================================================

#' Build distribution diagnostics table
#'
#' @param dist_data Output from \code{m3_compute_distribution_tests}
#' @param config A configuration list (see \code{biblio_config})
#' @return A tibble combining production and citation distribution stats
#' @export
m3_table_distribution_tests <- function(dist_data, config = biblio_config()) {
  if (!is.list(dist_data)) return(tibble::tibble())

  rows <- list()

  if (nrow(dist_data$production_distribution) > 0) {
    rows[["Production"]] <- dist_data$production_distribution %>%
      dplyr::mutate(Dimension = "Production")
  }
  if (nrow(dist_data$citations_distribution) > 0) {
    rows[["Citations"]] <- dist_data$citations_distribution %>%
      dplyr::mutate(Dimension = "Citations")
  }

  if (length(rows) == 0) return(tibble::tibble())

  dplyr::bind_rows(rows) %>%
    dplyr::select(Dimension, dplyr::everything(), -variable) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))
}
