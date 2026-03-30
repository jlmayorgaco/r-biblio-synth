# ============================================================================
# m1_biblio_cache.R - Cache biblioAnalysis results for M1
# ============================================================================

#' Cached biblioAnalysis computation
#'
#' Calls bibliometrix::biblioAnalysis once and caches the result
#' to avoid recomputing for every metric.
#'
#' @param input Bibliographic data frame
#' @return List with res (biblioAnalysis result) and summary (summary result)
#' @export
get_cached_biblio_analysis <- function(input) {
  # Use a global cache to store results
  if (!exists(".m1_biblio_cache", envir = .GlobalEnv)) {
    assign(".m1_biblio_cache", list(), envir = .GlobalEnv)
  }
  
  cache <- get(".m1_biblio_cache", envir = .GlobalEnv)
  
  # Create a simple hash of the input
  cache_key <- digest::digest(input, algo = "xxhash32")
  
  if (!is.null(cache[[cache_key]])) {
    return(cache[[cache_key]])
  }
  
  # Compute and cache
  result <- tryCatch({
    res <- bibliometrix::biblioAnalysis(input, sep = ";")
    s <- summary(res, pause = FALSE, verbose = FALSE)
    list(res = res, summary = s, status = "success")
  }, error = function(e) {
    list(res = NULL, summary = NULL, status = "error", error = conditionMessage(e))
  })
  
  cache[[cache_key]] <- result
  assign(".m1_biblio_cache", cache, envir = .GlobalEnv)
  
  result
}

#' Clear the biblioAnalysis cache
#' @export
clear_biblio_cache <- function() {
  if (exists(".m1_biblio_cache", envir = .GlobalEnv)) {
    rm(".m1_biblio_cache", envir = .GlobalEnv)
  }
}
