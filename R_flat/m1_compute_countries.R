# ============================================================================
# m1_compute_countries.R - Countries metric for M1
# ============================================================================

#' Compute M1 countries
#'
#' @param input A data frame of bibliographic data.
#' @param config A configuration list.
#' @return A list with country analysis results and \code{status}.
#' @export
compute_m1_countries <- function(input, config = biblio_config()) {
  if (!is.data.frame(input) || nrow(input) == 0) {
    return(list(
      top_countries_by_articles    = m1_empty_rank_table(),
      top_countries_by_citations   = m1_empty_rank_table(),
      scp_mcp_summary             = tibble::tibble(),
      country_gini_articles        = NA_real_,
      country_gini_citations       = NA_real_,
      status                       = "error"
    ))
  }

  # Use bibliometrix summary
  res <- bibliometrix::biblioAnalysis(input, sep = ";")
  s <- summary(res, pause = FALSE, verbose = FALSE)

  most_prod <- s$MostProdCountries
  tc_per    <- s$TCperCountries

  top_n <- config$top_n_countries

  # Top countries by articles
  if (!is.null(most_prod) && nrow(most_prod) > 0) {
    colnames(most_prod) <- make.unique(colnames(most_prod))
    art_col <- if ("Articles" %in% colnames(most_prod)) "Articles" else colnames(most_prod)[2]
    most_prod[[art_col]] <- suppressWarnings(as.numeric(most_prod[[art_col]]))
    most_prod <- most_prod[!is.na(most_prod[[art_col]]), ]
    most_prod <- most_prod[order(-most_prod[[art_col]]), ]

    top_countries_by_articles <- tibble::tibble(
      rank  = seq_len(min(top_n, nrow(most_prod))),
      label = most_prod$Country[1:min(top_n, nrow(most_prod))],
      value = most_prod[[art_col]][1:min(top_n, nrow(most_prod))]
    )

    # Normalize country names
    most_prod$Country <- m1_normalize_country_names(most_prod$Country)

    # Gini for articles
    lorenz_articles <- m1_compute_lorenz(most_prod[[art_col]])
    country_gini_articles <- m1_compute_gini(lorenz_articles$cumulative_x, lorenz_articles$cumulative_y)
  } else {
    top_countries_by_articles <- m1_empty_rank_table()
    country_gini_articles <- NA_real_
  }

  # Top countries by citations
  if (!is.null(tc_per) && nrow(tc_per) > 0) {
    colnames(tc_per) <- make.unique(colnames(tc_per))
    cit_col <- if ("Total Citations" %in% colnames(tc_per)) "Total Citations" else colnames(tc_per)[2]
    tc_per[[cit_col]] <- suppressWarnings(as.numeric(tc_per[[cit_col]]))
    tc_per <- tc_per[!is.na(tc_per[[cit_col]]), ]
    tc_per <- tc_per[order(-tc_per[[cit_col]]), ]

    top_countries_by_citations <- tibble::tibble(
      rank  = seq_len(min(top_n, nrow(tc_per))),
      label = tc_per$Country[1:min(top_n, nrow(tc_per))],
      value = tc_per[[cit_col]][1:min(top_n, nrow(tc_per))]
    )

    # Gini for citations
    lorenz_citations <- m1_compute_lorenz(tc_per[[cit_col]])
    country_gini_citations <- m1_compute_gini(lorenz_citations$cumulative_x, lorenz_citations$cumulative_y)
  } else {
    top_countries_by_citations <- m1_empty_rank_table()
    country_gini_citations <- NA_real_
  }

  # SCP/MCP summary
  if (!is.null(most_prod) && all(c("SCP", "MCP") %in% colnames(most_prod))) {
    most_prod$SCP <- suppressWarnings(as.numeric(most_prod$SCP))
    most_prod$MCP <- suppressWarnings(as.numeric(most_prod$MCP))
    most_prod <- most_prod[!is.na(most_prod$SCP) & !is.na(most_prod$MCP), ]

    top_scp_mcp <- tibble::tibble(
      rank       = seq_len(min(top_n, nrow(most_prod))),
      country    = most_prod$Country[1:min(top_n, nrow(most_prod))],
      articles   = most_prod[[if ("Articles" %in% colnames(most_prod)) "Articles" else colnames(most_prod)[2]]][1:min(top_n, nrow(most_prod))],
      scp        = most_prod$SCP[1:min(top_n, nrow(most_prod))],
      mcp        = most_prod$MCP[1:min(top_n, nrow(most_prod))],
      mcp_ratio  = round(most_prod$MCP[1:min(top_n, nrow(most_prod))] /
                         (most_prod$SCP[1:min(top_n, nrow(most_prod))] + most_prod$MCP[1:min(top_n, nrow(most_prod))]) * 100, 1)
    )
  } else {
    top_scp_mcp <- tibble::tibble()
  }

  list(
    top_countries_by_articles    = top_countries_by_articles,
    top_countries_by_citations   = top_countries_by_citations,
    scp_mcp_summary             = top_scp_mcp,
    most_prod_countries          = most_prod,
    tc_per_countries             = tc_per,
    country_gini_articles        = country_gini_articles,
    country_gini_citations       = country_gini_citations,
    status                       = "success"
  )
}
