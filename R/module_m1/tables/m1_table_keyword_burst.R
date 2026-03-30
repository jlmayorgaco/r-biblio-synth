# ============================================================================
# m1_table_keyword_burst.R - Table builder for keyword burst detection
# ============================================================================

#' @export
build_m1_keyword_burst_table <- function(result, config = biblio_config()) {
  status <- "stub"
  burst_table <- tibble::tibble()
  timing_table <- tibble::tibble()
  summary_list <- list()
  
  if (!is.null(result) && is.list(result) && result$status == "success") {
    status <- "success"
    
    # Burst table
    if (!is.null(result$bursts) && nrow(result$bursts) > 0) {
      burst_table <- tibble::as_tibble(result$bursts)
      burst_table$rank <- seq_len(nrow(burst_table))
      burst_table <- burst_table[, c("rank", "keyword", "start_year", "end_year", 
                                      "duration", "strength", "height", "total_freq")]
    }
    
    # Timing categories table
    if (!is.null(result$timing_categories)) {
      timing <- result$timing_categories
      timing_table <- tibble::tibble(
        category = c("Emerging", "Growing", "Declining", "Bursty"),
        count = c(
          timing$summary$n_emerging,
          timing$summary$n_growing,
          timing$summary$n_declining,
          timing$summary$n_bursty
        ),
        keywords = c(
          paste(head(timing$emerging, 5), collapse = ", "),
          paste(head(timing$growing, 5), collapse = ", "),
          paste(head(timing$declining, 5), collapse = ", "),
          paste(head(timing$bursty, 5), collapse = ", ")
        )
      )
    }
    
    # Summary
    summary_list <- result$summary
  }
  
  list(
    status = status,
    bursts = burst_table,
    timing = timing_table,
    summary = summary_list
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b