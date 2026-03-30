# ============================================================================
# table_hypotheses.R - Table builder for Hypothesis Results
# ============================================================================

#' Build hypothesis results table
#'
#' @param result Output from compute_*_hypotheses
#' @param config Configuration list
#' @return List with hypothesis tables
#' @export
build_hypotheses_table <- function(result, config = biblio_config()) {
  status <- "stub"
  hypothesis_table <- tibble::tibble()
  summary_list <- list()
  
  if (!is.null(result) && is.list(result) && result$status == "success") {
    hypotheses <- result$hyphypotheses %||% result$hypotheses
    
    if (!is.null(hypotheses) && length(hypotheses) > 0) {
      status <- "success"
      
      hypothesis_table <- do.call(rbind, lapply(names(hypotheses), function(h) {
        x <- hypotheses[[h]]
        tibble::tibble(
          hypothesis_id = gsub("H0._", "H", h),
          hypothesis = substr(x$hyphypothesis %||% x$hypothesis %||% h, 1, 100),
          null_hypothesis = substr(x$null %||% "", 1, 100),
          result = x$result %||% "unknown",
          interpretation = substr(x$interpretation %||% "", 1, 150)
        )
      }))
      
      n_total <- length(hypotheses)
      n_rejected <- sum(hypothesis_table$result == "reject")
      n_not_rejected <- sum(hypothesis_table$result == "fail_to_reject")
      
      summary_list <- list(
        n_total = n_total,
        n_rejected = n_rejected,
        n_not_rejected = n_not_rejected,
        n_inconclusive = n_total - n_rejected - n_not_rejected,
        rejection_rate = n_rejected / n_total,
        pass_rate = n_not_rejected / n_total
      )
    }
  }
  
  list(
    status = status,
    hypotheses = hypothesis_table,
    summary = summary_list
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b