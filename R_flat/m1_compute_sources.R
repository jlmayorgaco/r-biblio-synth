# ============================================================================
# m1_compute_sources.R - Sources metric for M1
# ============================================================================

#' Compute M1 sources
#'
#' @param input A data frame of bibliographic data.
#' @param config A configuration list.
#' @return A list with \code{top_sources}, \code{source_summary}, \code{status}.
#' @export
compute_m1_sources <- function(input, config = biblio_config()) {
  if (!is.data.frame(input) || nrow(input) == 0) {
    return(list(
      top_sources    = m1_empty_rank_table(),
      source_summary = list(),
      source_gini    = NA_real_,
      status         = "error"
    ))
  }

  # Use bibliometrix summary
  res <- bibliometrix::biblioAnalysis(input, sep = ";")
  s <- summary(res, pause = FALSE, verbose = FALSE)

  most_rel <- s$MostRelSources

  top_n <- config$top_n_sources

  if (!is.null(most_rel) && nrow(most_rel) > 0) {
    colnames(most_rel) <- make.unique(colnames(most_rel))
    art_col <- if ("Articles" %in% colnames(most_rel)) "Articles" else colnames(most_rel)[2]
    most_rel[[art_col]] <- suppressWarnings(as.numeric(most_rel[[art_col]]))
    most_rel <- most_rel[!is.na(most_rel[[art_col]]), ]
    most_rel <- most_rel[order(-most_rel[[art_col]]), ]

    src_col <- if ("Sources" %in% colnames(most_rel)) "Sources" else colnames(most_rel)[1]

    top_sources <- tibble::tibble(
      rank  = seq_len(min(top_n, nrow(most_rel))),
      label = most_rel[[src_col]][1:min(top_n, nrow(most_rel))],
      value = most_rel[[art_col]][1:min(top_n, nrow(most_rel))]
    )

    # Gini for sources
    lorenz <- m1_compute_lorenz(most_rel[[art_col]])
    source_gini <- m1_compute_gini(lorenz$cumulative_x, lorenz$cumulative_y)
  } else {
    top_sources <- m1_empty_rank_table()
    source_gini <- NA_real_
  }

  source_summary <- list(
    total_sources = length(unique(input$SO)),
    top_n         = top_n
  )

  list(
    top_sources    = top_sources,
    source_summary = source_summary,
    source_gini    = source_gini,
    status         = "success"
  )
}
