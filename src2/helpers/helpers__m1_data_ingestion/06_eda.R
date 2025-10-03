# ============================================================================ #
# helpers__m1_data_ingestion/06_eda.R (upgraded)
# JSON-friendly EDA for IEEE-grade review
# ============================================================================ #

m1i_hhi <- function(x) {
  # Herfindahl–Hirschman Index on counts vector
  if (length(x) == 0) return(NA_real_)
  p <- x / sum(x)
  sum(p^2)
}

m1i_gini_vec <- function(x) {
  # lightweight Gini (avoid extra deps)
  x <- as.numeric(x); x <- x[is.finite(x) & !is.na(x)]
  n <- length(x); if (n == 0) return(NA_real_)
  x <- sort(x)
  G <- (2 * sum(x * seq_len(n)))/(n * sum(x)) - (n + 1)/n
  as.numeric(G)
}

m1i_h_index <- function(citations) {
  c <- sort(as.numeric(citations), decreasing = TRUE)
  if (!length(c)) return(0L)
  h <- max(which(c >= seq_along(c)), 0L)
  as.integer(h)
}

m1i_entropy <- function(freq_table) {
  if (is.null(freq_table) || length(freq_table) == 0) return(NA_real_)
  p <- as.numeric(freq_table) / sum(freq_table)
  -sum(p * log(p))
}

m1i_cagr <- function(years, counts) {
  # CAGR based on first and last non-zero endpoints
  ok <- is.finite(years) & is.finite(counts) & counts > 0
  if (!any(ok)) return(NA_real_)
  years <- years[ok]; counts <- counts[ok]
  o <- order(years); years <- years[o]; counts <- counts[o]
  y0 <- years[1]; y1 <- years[length(years)]
  v0 <- counts[1]; v1 <- counts[length(counts)]
  if (y1 <= y0 || v0 <= 0) return(NA_real_)
  (v1 / v0)^(1/(y1 - y0)) - 1
}

m1i_compute_eda <- function(df) {
  n <- nrow(df); cols <- names(df)
  gcol <- function(name) if (name %in% cols) df[[name]] else NULL
  topn <- function(vec, n = 10) {
    if (is.null(vec)) return(NULL)
    tb <- sort(table(vec), decreasing = TRUE)
    as.list(head(tb, n))
  }

  years        <- suppressWarnings(as.integer(gcol("Year")))
  doc_types    <- gcol("Document_Type")
  langs        <- gcol("Language")
  source_titles<- gcol("Source_Title")
  times_cited  <- suppressWarnings(as.numeric(gcol("Times_Cited")))
  main_country <- gcol("Main_Country")
  db_src       <- gcol("Database")
  title_chr    <- gcol("Title")
  doi_chr      <- gcol("DOI")
  abs_chr      <- gcol("Abstract")
  aff_chr      <- gcol("Affiliations")
  ak_chr       <- gcol("Author_Keywords")
  ik_chr       <- gcol("Indexed_Keywords")
  country_list <- if ("Country_List" %in% cols) df$Country_List else NULL

  # --- Missingness (existing) ---
  miss_pct <- lapply(cols, function(cl) {
    x <- df[[cl]]
    is_blank <- is.na(x)
    if (is.character(x)) is_blank <- is_blank | (trimws(x) == "")
    round(mean(is_blank) * 100, 2)
  }); names(miss_pct) <- cols

  # --- Year span + recent share ---
  year_span <- if (!all(is.na(years))) {
    rng <- range(years, na.rm = TRUE)
    list(min = rng[1], max = rng[2])
  } else NULL
  recent_5y_share <- if (!is.null(year_span)) {
    maxy <- year_span$max
    mean(!is.na(years) & years >= (maxy - 4)) * 100
  } else NA_real_

  # --- Year out-of-range quick flag ---
  year_out_of_range_count <- sum(years < 1900 | years > as.integer(format(Sys.Date(), "%Y")) + 1, na.rm = TRUE)

  # --- Annual production & CAGR ---
  yr_tab <- table(years[!is.na(years)])
  year_cagr <- if (length(yr_tab)) m1i_cagr(as.integer(names(yr_tab)), as.numeric(yr_tab)) else NA_real_

  # --- Citations basic stats (existing) + robust bits ---
  tc_stats <- if (!all(is.na(times_cited))) {
    qs <- stats::quantile(times_cited, probs = c(0, .25, .5, .75, .9, .95, .99, 1),
                          na.rm = TRUE, names = FALSE)
    list(
      n_with_tc = sum(!is.na(times_cited)),
      mean      = round(mean(times_cited, na.rm = TRUE), 3),
      sd        = round(stats::sd(times_cited, na.rm = TRUE), 3),
      quantiles = setNames(round(qs, 3), c("min","p25","p50","p75","p90","p95","p99","max"))
    )
  } else NULL

  # --- Citations structure ---
  tc_h        <- m1i_h_index(times_cited)
  tc_gini     <- m1i_gini_vec(times_cited)
  if (length(times_cited) > 0) {
    tc_sorted <- sort(times_cited[is.finite(times_cited)], decreasing = TRUE)
    k <- max(floor(0.10 * length(tc_sorted)), 1)
    tc_top10_share <- if (length(tc_sorted)) sum(tc_sorted[seq_len(k)]) / sum(tc_sorted) else NA_real_
  } else tc_top10_share <- NA_real_

  # --- Per-year citation stats (helps with aging/recency bias) ---
  tc_by_year <- if (!all(is.na(years)) && !all(is.na(times_cited))) {
    split_tc <- split(times_cited, years)
    lapply(split_tc, function(v) {
      v <- v[is.finite(v)]
      if (!length(v)) return(NULL)
      list(mean = mean(v), median = stats::median(v))
    })
  } else NULL

  # --- Completeness & validity ---
  pct_nonempty <- function(x) mean(!is.na(x) & trimws(as.character(x)) != "") * 100
  pct_true     <- function(x) mean(x, na.rm = TRUE) * 100

  doi_valid <- if (!is.null(doi_chr)) {
    # mild DOI regex
    grepl("^10\\.\\d{4,9}/[-._;()/:A-Z0-9]+$", toupper(doi_chr))
  } else rep(NA, n)
  kw_any <- (!is.null(ak_chr) & trimws(ak_chr) != "") | (!is.null(ik_chr) & trimws(ik_chr) != "")

  completeness <- list(
    pct_with_doi        = round(pct_nonempty(doi_chr), 2),
    pct_doi_valid       = round(pct_true(doi_valid), 2),
    pct_with_abstract   = round(pct_nonempty(abs_chr), 2),
    pct_with_affil      = round(pct_nonempty(aff_chr), 2),
    pct_with_keywords   = round(pct_true(kw_any), 2),
    pct_title_nonempty  = round(pct_nonempty(title_chr), 2)
  )

  # --- Dedup/consistency signals ---
  norm_title <- tolower(stringr::str_squish(ifelse(is.na(title_chr), "", title_chr)))
  key <- ifelse(!is.null(doi_chr) & trimws(doi_chr) != "", paste0("doi:", tolower(doi_chr)), paste0("ti:", norm_title))
  dedup_key_unique_ratio <- if (length(key)) round(length(unique(key)) / length(key), 4) else NA_real_
  possible_duplicates <- sum(duplicated(norm_title) & nzchar(norm_title))

  # --- Authorship/collab quick proxies ---
  authors_per_doc <- if (!is.null(df$Authors)) {
    asz <- vapply(strsplit(df$Authors %||% "", ";", fixed = TRUE), function(x) sum(trimws(x) != ""), integer(1))
    stats <- stats::quantile(asz, c(.5,.9), na.rm = TRUE, names = FALSE)
    list(mean = mean(asz, na.rm = TRUE), median = stats[1], p90 = stats[2])
  } else NULL

  country_len <- if (!is.null(country_list)) vapply(country_list, function(x) if (is.null(x)) 0L else length(x), integer(1)) else NULL
  collab <- if (!is.null(country_len)) {
    stats <- stats::quantile(country_len, c(.5,.9), na.rm = TRUE, names = FALSE)
    list(
      countries_per_doc = list(mean = mean(country_len, na.rm = TRUE), median = stats[1], p90 = stats[2]),
      mcp_share_pct     = round(mean(country_len >= 2, na.rm = TRUE) * 100, 2)
    )
  } else NULL

  # --- Keyword stats & entropy ---
  split_semis <- function(x) {
    if (is.null(x)) return(character(0))
    y <- unlist(strsplit(x, ";", fixed = TRUE), use.names = FALSE)
    y <- trimws(y); y[nzchar(y)]
  }
  ak_all <- split_semis(ak_chr)
  ik_all <- split_semis(ik_chr)
  kw_all <- c(ak_all, ik_all)
  kw_per_doc_mean <- mean(lengths(lapply(seq_len(n), function(i) {
    ka <- if (!is.null(ak_chr)) split_semis(ak_chr[i]) else character(0)
    ki <- if (!is.null(ik_chr)) split_semis(ik_chr[i]) else character(0)
    unique(c(ka, ki))
  })), na.rm = TRUE)

  kw_entropy_author <- if (length(ak_all)) m1i_entropy(table(tolower(ak_all))) else NA_real_

  # --- Venue concentration (HHI/Gini) ---
  source_tbl <- table(source_titles[!is.na(source_titles) & nzchar(source_titles)])
  venue <- if (length(source_tbl)) {
    list(
      hhi  = round(m1i_hhi(as.numeric(source_tbl)), 4),
      gini = round(m1i_gini_vec(as.numeric(source_tbl)), 4),
      top10_sources = as.list(head(sort(source_tbl, decreasing = TRUE), 10))
    )
  } else NULL

  # --- Top lists (existing) ---
  top_sources   <- as.list(head(sort(source_tbl, decreasing = TRUE), 10))
  top_types     <- topn(doc_types, 10)
  top_langs     <- topn(langs, 10)
  top_countries <- topn(main_country, 10)

  # --- Country coverage (existing) ---
  country_coverage <- if (!is.null(country_list)) {
    with_country <- vapply(country_list, function(x) !is.null(x) && length(x) > 0, logical(1))
    round(mean(with_country) * 100, 2)
  } else NA_real_

  # --- Source mix ---
  source_mix <- if (!is.null(db_src)) as.list(sort(table(db_src), decreasing = TRUE)) else NULL

  list(
    overview = list(
      n_docs = n,
      n_columns = ncol(df),
      columns = names(df),
      year_span = year_span,
      median_year = if (any(!is.na(years))) stats::median(years, na.rm = TRUE) else NA_real_,
      year_mode = if (length(yr_tab)) as.integer(names(yr_tab)[which.max(yr_tab)]) else NA_integer_,
      recent_5y_share_pct = round(recent_5y_share, 2),
      year_cagr = round(year_cagr, 4),
      source_mix = source_mix
    ),
    completeness = completeness,
    quality_flags = list(
      year_out_of_range_count = year_out_of_range_count,
      dedup_key_unique_ratio = dedup_key_unique_ratio,
      possible_duplicates = possible_duplicates,
      title_nonempty_pct = completeness$pct_title_nonempty
    ),
    citations = list(
      basic = tc_stats,
      h_index_dataset = tc_h,
      gini = round(tc_gini, 4),
      top10_share = round(tc_top10_share, 4),
      by_year = tc_by_year
    ),
    authorship_collab = list(
      authors_per_doc = authors_per_doc,
      collab = collab
    ),
    keywords = list(
      unique_author_kw = length(unique(tolower(ak_all))),
      unique_indexed_kw = length(unique(tolower(ik_all))),
      kw_per_doc_mean = round(kw_per_doc_mean, 3),
      author_kw_entropy = if (is.na(kw_entropy_author)) NA_real_ else round(kw_entropy_author, 4)
    ),
    venues = venue,
    toplists = list(
      doc_types_top10 = top_types,
      languages_top10 = top_langs,
      sources_top10 = top_sources,
      countries_top10 = top_countries
    ),
    missingness_pct = miss_pct,
    country_coverage_pct = country_coverage
  )
}
