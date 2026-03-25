# ============================================================================
# m1_table_authors.R - Table builder for authors
# ============================================================================

#' Build M1 authors table
#'
#' @param result The compute result for authors.
#' @param config A configuration list.
#' @return A list with \code{status} and \code{table}.
#' @export
build_m1_authors_table <- function(result, config = biblio_config()) {
  list(
    status = "stub",
    table  = tibble::tibble()
  )
}
