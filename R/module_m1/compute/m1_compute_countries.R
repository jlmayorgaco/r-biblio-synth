# ============================================================================
# m1_compute_countries.R - Countries metric for M1
# ============================================================================

#' @export
compute_m1_countries <- function(input, config = biblio_config()) {
  if (!is.data.frame(input) || nrow(input) == 0) {
    return(list(top_countries_by_articles = m1_empty_rank_table(), top_countries_by_citations = m1_empty_rank_table(),
                avg_citations_by_country = m1_empty_rank_table(), scp_mcp_summary = tibble::tibble(),
                country_gini_articles = NA_real_, country_gini_citations = NA_real_, status = "error"))
  }

  top_n <- config$top_n_countries

  # Try bibliometrix first
  country_data <- tryCatch({
    cached <- get_cached_biblio_analysis(input)
    res <- cached$res
    s <- summary(res, pause = FALSE, verbose = FALSE)
    list(most_prod = s$MostProdCountries, tc_per = s$TCperCountries)
  }, error = function(e) {
    list(most_prod = NULL, tc_per = NULL)
  })

  most_prod <- country_data$most_prod
  tc_per <- country_data$tc_per

  # Top countries by articles
  if (!is.null(most_prod) && nrow(most_prod) > 0) {
    colnames(most_prod) <- make.unique(colnames(most_prod))
    art_col     <- if ("Articles" %in% colnames(most_prod)) "Articles" else colnames(most_prod)[2]
    country_col <- if ("Country"  %in% colnames(most_prod)) "Country"  else colnames(most_prod)[1]
    most_prod[[art_col]] <- suppressWarnings(as.numeric(most_prod[[art_col]]))
    most_prod <- most_prod[!is.na(most_prod[[art_col]]), ]
    most_prod <- most_prod[order(-most_prod[[art_col]]), ]

    n_mp <- min(top_n, nrow(most_prod))
    top_countries_by_articles <- tibble::tibble(
      rank  = seq_len(n_mp),
      label = most_prod[[country_col]][seq_len(n_mp)],
      value = most_prod[[art_col]][seq_len(n_mp)]
    )
    most_prod[[country_col]] <- m1_normalize_country_names(most_prod[[country_col]])
    lorenz <- m1_compute_lorenz(most_prod[[art_col]])
    country_gini_articles <- m1_compute_gini(lorenz$cumulative_entities, lorenz$cumulative_values)
  } else {
    top_countries_by_articles <- m1_empty_rank_table()
    country_gini_articles <- NA_real_
    country_col <- "Country"
  }

  # Top countries by citations + avg citations
  avg_citations_by_country <- m1_empty_rank_table()
  if (!is.null(tc_per) && nrow(tc_per) > 0) {
    colnames(tc_per) <- make.unique(colnames(tc_per))
    cit_col     <- if ("Total Citations" %in% colnames(tc_per)) "Total Citations" else colnames(tc_per)[2]
    tc_country_col <- if ("Country" %in% colnames(tc_per)) "Country" else colnames(tc_per)[1]
    tc_per[[cit_col]] <- suppressWarnings(as.numeric(tc_per[[cit_col]]))
    tc_per <- tc_per[!is.na(tc_per[[cit_col]]), ]
    tc_per <- tc_per[order(-tc_per[[cit_col]]), ]

    n_tc <- min(top_n, nrow(tc_per))
    top_countries_by_citations <- tibble::tibble(
      rank  = seq_len(n_tc),
      label = tc_per[[tc_country_col]][seq_len(n_tc)],
      value = tc_per[[cit_col]][seq_len(n_tc)]
    )
    lorenz_cit <- m1_compute_lorenz(tc_per[[cit_col]])
    country_gini_citations <- m1_compute_gini(lorenz_cit$cumulative_entities, lorenz_cit$cumulative_values)

    # F5h - Average Article Citations per country
    if ("Average Article Citations" %in% colnames(tc_per)) {
      avg_col <- "Average Article Citations"
      tc_per[[avg_col]] <- suppressWarnings(as.numeric(tc_per[[avg_col]]))
      tc_per_avg <- tc_per[!is.na(tc_per[[avg_col]]), ]
      tc_per_avg <- tc_per_avg[order(-tc_per_avg[[avg_col]]), ]
      if (nrow(tc_per_avg) > 0) {
        n_avg <- min(top_n, nrow(tc_per_avg))
        avg_citations_by_country <- tibble::tibble(
          rank  = seq_len(n_avg),
          label = tc_per_avg[[tc_country_col]][seq_len(n_avg)],
          value = tc_per_avg[[avg_col]][seq_len(n_avg)]
        )
      }
    }
  } else {
    top_countries_by_citations <- m1_empty_rank_table()
    country_gini_citations <- NA_real_
  }

  # SCP/MCP — country_col was set in the block above (defaults to "Country" if most_prod was empty)
  if (!is.null(most_prod) && all(c("SCP", "MCP") %in% colnames(most_prod))) {
    most_prod$SCP <- suppressWarnings(as.numeric(most_prod$SCP))
    most_prod$MCP <- suppressWarnings(as.numeric(most_prod$MCP))
    most_prod <- most_prod[!is.na(most_prod$SCP) & !is.na(most_prod$MCP), ]
    n <- min(top_n, nrow(most_prod))
    art_col2 <- if ("Articles" %in% colnames(most_prod)) "Articles" else colnames(most_prod)[2]
    denom <- most_prod$SCP[seq_len(n)] + most_prod$MCP[seq_len(n)]
    mcp_ratio <- ifelse(denom > 0, round(most_prod$MCP[seq_len(n)] / denom * 100, 1), NA_real_)
    scp_mcp <- tibble::tibble(
      rank      = seq_len(n),
      country   = most_prod[[country_col]][seq_len(n)],
      articles  = most_prod[[art_col2]][seq_len(n)],
      scp       = most_prod$SCP[seq_len(n)],
      mcp       = most_prod$MCP[seq_len(n)],
      mcp_ratio = mcp_ratio
    )
  } else {
    scp_mcp <- tibble::tibble()
  }

  list(
    top_countries_by_articles = top_countries_by_articles,
    top_countries_by_citations = top_countries_by_citations,
    avg_citations_by_country = avg_citations_by_country,
    scp_mcp_summary = scp_mcp, most_prod_countries = most_prod, tc_per_countries = tc_per,
    country_gini_articles = country_gini_articles, country_gini_citations = country_gini_citations,
    status = "success"
  )
}
