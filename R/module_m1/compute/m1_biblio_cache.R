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
rbiblio_m1_cache_env <- local({
  env <- new.env(parent = emptyenv())
  env$cache <- list()
  env
})

get_cached_biblio_analysis <- function(input) {
  input <- m1_prepare_biblio_input(input)
  cache <- rbiblio_m1_cache_env$cache %||% list()
  cache_key <- digest::digest(input, algo = "xxhash32")

  if (!is.null(cache[[cache_key]])) {
    return(cache[[cache_key]])
  }

  result <- tryCatch({
    res <- bibliometrix::biblioAnalysis(input, sep = ";")
    s <- summary(res, pause = FALSE, verbose = FALSE)
    list(res = res, summary = s, status = "success")
  }, error = function(e) {
    list(res = NULL, summary = NULL, status = "error", error = conditionMessage(e))
  })

  cache[[cache_key]] <- result
  rbiblio_m1_cache_env$cache <- cache

  result
}

#' Clear the biblioAnalysis cache
#' @export
clear_biblio_cache <- function() {
  rbiblio_m1_cache_env$cache <- list()
  invisible(TRUE)
}

#' Prepare input for bibliometrix compatibility
#' @keywords internal
m1_prepare_biblio_input <- function(input) {
  if (!is.data.frame(input)) {
    return(input)
  }

  prepared <- input
  if (!"DB" %in% names(prepared)) {
    if ("SOURCE_DB" %in% names(prepared)) {
      prepared$DB <- toupper(as.character(prepared$SOURCE_DB))
    } else {
      prepared$DB <- rep("GENERIC", nrow(prepared))
    }
  }

  if ("SO" %in% names(prepared)) {
    if (!"JI" %in% names(prepared)) {
      prepared$JI <- as.character(prepared$SO)
    }
    if (!"J9" %in% names(prepared)) {
      prepared$J9 <- as.character(prepared$SO)
    }
  }

  if (!"C1" %in% names(prepared)) {
    prepared$C1 <- rep("", nrow(prepared))
  }

  prepared
}
