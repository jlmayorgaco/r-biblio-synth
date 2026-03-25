# ============================================================================
# m1_render_overview.R - Render overview
# ============================================================================

#' Render M1 overview
#'
#' @param result The compute result for overview.
#' @param config A configuration list.
#' @return A list with \code{status}, \code{plots}, \code{tables}.
#' @export
render_m1_overview <- function(result, config = biblio_config()) {
  if (inherits(result, "list") && "summary_table" %in% names(result)) {
    list(
      status = "success",
      plots  = list(),
      tables = list(summary = result$summary_table)
    )
  } else {
    list(
      status = "stub",
      plots  = list(),
      tables = list()
    )
  }
}
