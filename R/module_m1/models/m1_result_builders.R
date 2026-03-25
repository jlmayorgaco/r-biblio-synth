# ============================================================================
# m1_result_builders.R - Bundle builders for M1
# ============================================================================

#' Build a compute bundle from M1 metric results
#'
#' @param ... Named \code{m1_metric_result} objects.
#' @return A named list of compute results.
#' @export
build_m1_compute_bundle <- function(...) {
  list(...)
}

#' Build a render bundle from M1 render results
#'
#' @param ... Named render results.
#' @return A named list of render results.
#' @export
build_m1_render_bundle <- function(...) {
  list(...)
}
