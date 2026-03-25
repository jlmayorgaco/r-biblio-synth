# ============================================================================
# m1_table_keywords.R - Table builder for keywords
# ============================================================================

#' Build M1 keywords table
#'
#' @param result The compute result for keywords.
#' @param config A configuration list.
#' @return A list with \code{status} and \code{table}.
#' @export
build_m1_keywords_table <- function(result, config = biblio_config()) {
  list(
    status = "stub",
    table  = tibble::tibble()
  )
}
