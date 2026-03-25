# ============================================================================
# m1_table_bradford.R - Table builder for bradford
# ============================================================================

#' @export
build_m1_bradford_table <- function(result, config = biblio_config()) {
  list(
    status = if (inherits(result, "list") && "bradford_table" %in% names(result)) "success" else "stub",
    table  = if (inherits(result, "list") && "bradford_table" %in% names(result)) result$bradford_table else tibble::tibble()
  )
}
