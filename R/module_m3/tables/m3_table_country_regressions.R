# ============================================================================
# m3_table_country_regressions.R - Table builder for Country Regressions
# ============================================================================

#' @export
m3_table_country_regressions <- function(result, config = biblio_config()) {
  status <- "stub"
  regression_table <- tibble::tibble()
  hypothesis_table <- tibble::tibble()
  summary_list <- list()
  
  if (!is.null(result) && is.list(result) && result$status == "success") {
    status <- "success"
    
    if (!is.null(result$country_regressions)) {
      successful <- Filter(function(x) x$status == "success", result$country_regressions)
      
      if (length(successful) > 0) {
        regression_table <- do.call(rbind, lapply(names(successful), function(cntry) {
          x <- successful[[cntry]]
          tibble::tibble(
            country = cntry,
            n_years = x$n_years,
            year_start = x$year_range[1],
            year_end = x$year_range[2],
            total_articles = x$total_articles,
            mean_annual = round(x$mean_annual, 1),
            sd_annual = round(x$sd_annual %||% NA_real_, 2),
            slope = round(x$slope %||% NA_real_, 4),
            slope_pvalue = round(x$slope_pvalue %||% NA_real_, 4),
            growth_rate_pct = round(x$growth_rate %||% NA_real_, 2),
            trend_direction = x$trend_direction %||% "unknown",
            best_model = x$best_model %||% NA_character_,
            R2 = round(x$best_fit$R2 %||% NA_real_, 4)
          )
        }))
        
        regression_table <- regression_table[order(-regression_table$total_articles), ]
        regression_table$rank <- seq_len(nrow(regression_table))
        regression_table <- regression_table[, c("rank", "country", "n_years", "year_start", "year_end",
                                                 "total_articles", "mean_annual", "slope", "slope_pvalue",
                                                 "growth_rate_pct", "trend_direction", "best_model", "R2")]
      }
    }
    
    if (!is.null(result$hypotheses)) {
      hyp_list <- result$hypotheses$hyphypotheses
      if (is.null(hyp_list)) hyp_list <- result$hypotheses
      
      hypothesis_table <- do.call(rbind, lapply(names(hyp_list), function(h) {
        x <- hyp_list[[h]]
        tibble::tibble(
          hypothesis_id = gsub("H03_", "H", h),
          hyphypothesis = substr(x$hyphypothesis %||% x$hyphothesis %||% h, 1, 80),
          null_hyphypothesis = substr(x$null %||% "", 1, 80),
          result = x$result %||% "unknown",
          interpretation = substr(x$interpretation %||% "", 1, 100)
        )
      }))
    }
    
    if (!is.null(result$summary)) {
      summary_list <- result$summary
    }
  }
  
  list(
    status = status,
    regressions = regression_table,
    hypotheses = hypothesis_table,
    summary = summary_list
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b