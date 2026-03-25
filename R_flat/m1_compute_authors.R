# ============================================================================
# m1_compute_authors.R - Authors metric for M1
# ============================================================================

#' Compute M1 authors
#'
#' @param input A data frame of bibliographic data.
#' @param config A configuration list.
#' @return A list with \code{top_authors}, \code{author_productivity}, \code{status}.
#' @export
compute_m1_authors <- function(input, config = biblio_config()) {
  if (!is.data.frame(input) || nrow(input) == 0) {
    return(list(
      top_authors         = m1_empty_rank_table(),
      author_productivity = list(),
      author_gini         = NA_real_,
      status              = "error"
    ))
  }

  # Use bibliometrix summary
  res <- bibliometrix::biblioAnalysis(input, sep = ";")
  s <- summary(res, pause = FALSE, verbose = FALSE)

  most_prod <- s$MostProdAuthors

  # Build top authors table
  top_n <- config$top_n_authors
  if (!is.null(most_prod) && nrow(most_prod) > 0) {
    # Clean column names
    colnames(most_prod) <- make.unique(colnames(most_prod))

    # Get article column
    art_col <- if ("Articles" %in% colnames(most_prod)) "Articles" else colnames(most_prod)[2]

    most_prod[[art_col]] <- suppressWarnings(as.numeric(most_prod[[art_col]]))
    most_prod <- most_prod[!is.na(most_prod[[art_col]]), ]
    most_prod <- most_prod[order(-most_prod[[art_col]]), ]

    top_authors <- tibble::tibble(
      rank  = seq_len(min(top_n, nrow(most_prod))),
      label = most_prod$Authors[1:min(top_n, nrow(most_prod))],
      value = most_prod[[art_col]][1:min(top_n, nrow(most_prod))]
    )
  } else {
    top_authors <- m1_empty_rank_table()
  }

  # Compute Gini coefficient for author productivity
  if (nrow(top_authors) > 1) {
    lorenz <- m1_compute_lorenz(top_authors$value)
    author_gini <- m1_compute_gini(lorenz$cumulative_x, lorenz$cumulative_y)
  } else {
    author_gini <- NA_real_
  }

  list(
    top_authors         = top_authors,
    author_productivity = most_prod,
    author_gini         = author_gini,
    status              = "success"
  )
}
