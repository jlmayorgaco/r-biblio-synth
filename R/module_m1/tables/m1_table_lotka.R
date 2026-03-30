# ============================================================================
# m1_table_lotka.R - Table builder for Lotka's Law
# ============================================================================

#' @export
build_m1_lotka_table <- function(result, config = biblio_config()) {
  status <- "stub"
  lotka_table <- tibble::tibble()
  summary_list <- list()
  
  if (!is.null(result) && is.list(result) && result$status == "success") {
    status <- "success"
    
    lotka <- result$lotka
    
    summary_list <- list(
      alpha = lotka$alpha %||% NA_real_,
      C = lotka$C %||% NA_real_,
      classical_alpha = lotka$alpha_classical %||% 2,
      alpha_diff = lotka$alpha_diff %||% NA_real_,
      gof_ks = lotka$gof_ks %||% NA_real_,
      gof_pvalue = lotka$gof_pvalue %||% NA_real_,
      is_lotka = lotka$is_lotka %||% NA,
      n_authors = lotka$n_authors %||% NA_integer_,
      n_articles = lotka$n_articles %||% NA_integer_,
      interpretation = lotka$interpretation %||% ""
    )
    
    if (!is.null(lotka$frequencies) && nrow(lotka$frequencies) > 0) {
      lotka_table <- tibble::as_tibble(lotka$frequencies)
    }
  }
  
  list(
    status = status,
    table = lotka_table,
    summary = summary_list
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b