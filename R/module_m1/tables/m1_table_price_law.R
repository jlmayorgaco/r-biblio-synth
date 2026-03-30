# ============================================================================
# m1_table_price_law.R - Table builder for Price's Law
# ============================================================================

#' @export
build_m1_price_law_table <- function(result, config = biblio_config()) {
  status <- "stub"
  price_table <- tibble::tibble()
  summary_list <- list()
  
  if (!is.null(result) && is.list(result) && result$status == "success") {
    status <- "success"
    
    summary_list <- list(
      price_index = result$price_index$index %||% NA_real_,
      sqrt_n_authors = result$price_law$sqrt_n_authors %||% NA_real_,
      total_authors = result$price_law$n_authors %||% NA_integer_,
      total_articles = result$summary$total_papers %||% NA_integer_,
      actual_proportion = result$price_law$actual_proportion %||% NA_real_,
      half_life = result$half_life$median_age %||% NA_real_,
      gini_coefficient = result$author_concentration$gini %||% NA_real_
    )
    
    if (!is.null(result$price_law$author_distribution) && nrow(result$price_law$author_distribution) > 0) {
      price_table <- tibble::as_tibble(result$price_law$author_distribution)
    }
  }
  
  list(
    status = status,
    table = price_table,
    summary = summary_list
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b