# ============================================================================
# m1_summaries.R - Empty summary tables for M1 stubs
# ============================================================================

#' Create an empty summary table
#'
#' @return An empty tibble.
#' @export
m1_empty_summary_table <- function() {
  tibble::tibble(
    metric = character(),
    value  = character()
  )
}

#' Create an empty rank table
#'
#' @return An empty tibble.
#' @export
m1_empty_rank_table <- function() {
  tibble::tibble(
    rank  = integer(),
    label = character(),
    value = numeric()
  )
}
