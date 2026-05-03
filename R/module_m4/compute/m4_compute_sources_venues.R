# ============================================================================
# m4_compute_sources_venues.R - Source / journal / venue analytics
# ============================================================================

m4_select_source_column <- function(input) {
  candidates <- c("SO", "Source", "SOURCE", "Journal", "JOURNAL", "Publication.Source", "JI", "J9")
  candidates[candidates %in% names(input)][1] %||% NULL
}

m4_prepare_source_data <- function(input, config = biblio_config()) {
  if (!is.data.frame(input) || nrow(input) == 0) {
    return(list(status = "empty", records = tibble::tibble(), source_summary = tibble::tibble()))
  }

  source_col <- m4_select_source_column(input)
  if (is.null(source_col)) {
    return(list(status = "missing_source", records = tibble::tibble(), source_summary = tibble::tibble()))
  }

  records <- tibble::tibble(
    doc_id = seq_len(nrow(input)),
    source = m4_normalize_source(input[[source_col]]),
    year = m4_num_col(input, c("PY", "Year", "year")),
    tc = m4_num_col(input, c("TC", "Times.Cited", "Citations", "total_citations")),
    keywords = m4_text_col(input, c("DE", "ID", "Author.Keywords", "Keywords"))
  ) |>
    dplyr::filter(!is.na(.data$source), nzchar(.data$source))

  if (nrow(records) == 0) {
    return(list(status = "empty_sources", records = records, source_summary = tibble::tibble()))
  }

  annual <- records |>
    dplyr::filter(is.finite(.data$year)) |>
    dplyr::group_by(.data$source, .data$year) |>
    dplyr::summarise(tp = dplyr::n(), tc = sum(.data$tc, na.rm = TRUE), .groups = "drop")

  source_summary <- records |>
    dplyr::group_by(.data$source) |>
    dplyr::summarise(
      tp = dplyr::n(),
      tc = sum(.data$tc, na.rm = TRUE),
      cpp = .data$tc / pmax(.data$tp, .Machine$double.eps),
      h_index = m4_h_index(.data$tc),
      g_index = m4_g_index(.data$tc),
      first_year = suppressWarnings(min(.data$year, na.rm = TRUE)),
      last_year = suppressWarnings(max(.data$year, na.rm = TRUE)),
      active_years = dplyr::n_distinct(.data$year[is.finite(.data$year)]),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      first_year = ifelse(is.finite(.data$first_year), .data$first_year, NA_real_),
      last_year = ifelse(is.finite(.data$last_year), .data$last_year, NA_real_),
      share = .data$tp / sum(.data$tp, na.rm = TRUE)
    ) |>
    dplyr::arrange(dplyr::desc(.data$tp), dplyr::desc(.data$tc))

  list(
    status = "success",
    source_col = source_col,
    records = records,
    source_annual = annual,
    source_summary = source_summary
  )
}

m4_compute_sources <- function(prepared, config = biblio_config()) {
  summary <- prepared$source_summary %||% tibble::tibble()
  top_n <- as.integer(config$top_n_sources %||% 20L)
  if (!is.data.frame(summary) || nrow(summary) == 0) {
    return(list(status = "empty", top_sources = tibble::tibble(), source_summary = tibble::tibble()))
  }
  list(
    status = "success",
    source_summary = summary,
    top_sources = summary |> dplyr::slice(seq_len(min(top_n, nrow(summary)))),
    totals = list(
      total_sources = nrow(summary),
      total_documents = sum(summary$tp, na.rm = TRUE),
      total_citations = sum(summary$tc, na.rm = TRUE)
    )
  )
}

m4_compute_source_impact <- function(prepared, config = biblio_config()) {
  summary <- prepared$source_summary %||% tibble::tibble()
  if (!is.data.frame(summary) || nrow(summary) == 0) return(list(status = "empty", impact = tibble::tibble()))
  impact <- summary |>
    dplyr::mutate(
      impact_rank = dplyr::min_rank(dplyr::desc(.data$tc)),
      productivity_rank = dplyr::min_rank(dplyr::desc(.data$tp)),
      efficiency_rank = dplyr::min_rank(dplyr::desc(.data$cpp))
    ) |>
    dplyr::arrange(.data$impact_rank)
  list(status = "success", impact = impact)
}

m4_compute_bradford <- function(prepared, config = biblio_config()) {
  summary <- prepared$source_summary %||% tibble::tibble()
  if (!is.data.frame(summary) || nrow(summary) == 0) return(list(status = "empty", zones = tibble::tibble(), summary = list()))
  total_tp <- sum(summary$tp, na.rm = TRUE)
  zones <- summary |>
    dplyr::arrange(dplyr::desc(.data$tp), .data$source) |>
    dplyr::mutate(
      cumulative_tp = cumsum(.data$tp),
      cumulative_share = .data$cumulative_tp / total_tp,
      bradford_zone = dplyr::case_when(
        .data$cumulative_share <= 1 / 3 ~ "Zone 1: Core",
        .data$cumulative_share <= 2 / 3 ~ "Zone 2: Moderate",
        TRUE ~ "Zone 3: Peripheral"
      )
    )
  zone_summary <- zones |>
    dplyr::group_by(.data$bradford_zone) |>
    dplyr::summarise(
      n_sources = dplyr::n(),
      tp = sum(.data$tp, na.rm = TRUE),
      tc = sum(.data$tc, na.rm = TRUE),
      cpp = .data$tc / pmax(.data$tp, .Machine$double.eps),
      .groups = "drop"
    )
  list(status = "success", zones = zones, zone_summary = zone_summary)
}

m4_compute_source_growth <- function(prepared, config = biblio_config()) {
  annual <- prepared$source_annual %||% tibble::tibble()
  summary <- prepared$source_summary %||% tibble::tibble()
  if (!is.data.frame(annual) || nrow(annual) == 0) {
    return(list(status = "empty", growth = tibble::tibble()))
  }
  sources <- sort(unique(annual$source))
  growth <- dplyr::bind_rows(lapply(sources, function(src) {
    df <- annual[annual$source == src, , drop = FALSE]
    first <- df[order(df$year), , drop = FALSE][1, , drop = FALSE]
    last <- df[order(-df$year), , drop = FALSE][1, , drop = FALSE]
    span <- max(df$year, na.rm = TRUE) - min(df$year, na.rm = TRUE)
    tibble::tibble(
      source = src,
      tp_slope = m4_slope(df$year, df$tp),
      tc_slope = m4_slope(df$year, df$tc),
      cagr = if (span >= 1 && first$tp > 0) (last$tp / first$tp)^(1 / span) - 1 else NA_real_,
      recent_year = last$year,
      recent_tp = last$tp
    )
  })) |>
    dplyr::left_join(summary |> dplyr::select("source", "tp", "tc", "cpp"), by = "source") |>
    dplyr::arrange(dplyr::desc(.data$tp_slope), dplyr::desc(.data$tp))
  list(status = "success", growth = growth, annual = annual)
}

m4_compute_source_concentration <- function(prepared, config = biblio_config()) {
  summary <- prepared$source_summary %||% tibble::tibble()
  if (!is.data.frame(summary) || nrow(summary) == 0) return(list(status = "empty", metrics = tibble::tibble()))
  shares <- summary$tp / sum(summary$tp, na.rm = TRUE)
  metrics <- tibble::tibble(
    metric = c("source_gini", "source_hhi", "top_5_share", "top_10_share", "n_sources"),
    value = c(
      m4_gini(summary$tp),
      sum(shares^2, na.rm = TRUE),
      sum(utils::head(sort(shares, decreasing = TRUE), 5), na.rm = TRUE),
      sum(utils::head(sort(shares, decreasing = TRUE), 10), na.rm = TRUE),
      nrow(summary)
    )
  )
  list(status = "success", metrics = metrics)
}

m4_compute_source_keywords <- function(prepared, config = biblio_config()) {
  records <- prepared$records %||% tibble::tibble()
  if (!is.data.frame(records) || nrow(records) == 0 || !"keywords" %in% names(records)) {
    return(list(status = "empty", source_keywords = tibble::tibble()))
  }
  rows <- lapply(seq_len(nrow(records)), function(i) {
    terms <- trimws(unlist(strsplit(as.character(records$keywords[i] %||% ""), ";|,", perl = TRUE)))
    terms <- terms[nzchar(terms)]
    if (length(terms) == 0) return(NULL)
    tibble::tibble(source = records$source[i], keyword = tolower(terms))
  })
  long <- dplyr::bind_rows(rows)
  if (!is.data.frame(long) || nrow(long) == 0) return(list(status = "empty", source_keywords = tibble::tibble()))
  source_keywords <- long |>
    dplyr::count(.data$source, .data$keyword, name = "n") |>
    dplyr::group_by(.data$source) |>
    dplyr::arrange(dplyr::desc(.data$n), .by_group = TRUE) |>
    dplyr::slice(seq_len(min(5L, dplyr::n()))) |>
    dplyr::summarise(top_keywords = paste(.data$keyword, collapse = "; "), .groups = "drop")
  list(status = "success", source_keywords = source_keywords)
}

m4_compute_source_clusters <- function(data, config = biblio_config()) {
  impact <- data$impact$impact %||% tibble::tibble()
  growth <- data$growth$growth %||% tibble::tibble()
  bradford <- data$bradford$zones %||% tibble::tibble()
  if (!is.data.frame(impact) || nrow(impact) < 3) return(list(status = "empty", clusters = tibble::tibble()))
  features <- impact |>
    dplyr::select("source", "tp", "tc", "cpp", "h_index", "g_index") |>
    dplyr::left_join(growth |> dplyr::select("source", "tp_slope", "tc_slope", "cagr"), by = "source") |>
    dplyr::left_join(bradford |> dplyr::select("source", "bradford_zone"), by = "source") |>
    dplyr::mutate(
      dplyr::across(c("tp", "tc", "cpp", "h_index", "g_index", "tp_slope", "tc_slope"), ~log1p(pmax(., 0))),
      cagr = dplyr::coalesce(.data$cagr, 0)
    )
  x <- as.data.frame(features[, c("tp", "tc", "cpp", "h_index", "g_index", "tp_slope", "tc_slope", "cagr"), drop = FALSE])
  keep <- stats::complete.cases(x)
  features <- features[keep, , drop = FALSE]
  x <- x[keep, , drop = FALSE]
  if (nrow(x) < 3) return(list(status = "empty", clusters = tibble::tibble()))
  variable_cols <- vapply(x, function(col) stats::sd(col, na.rm = TRUE) > 0, logical(1))
  x <- x[, variable_cols, drop = FALSE]
  if (ncol(x) < 2) return(list(status = "empty", clusters = tibble::tibble()))
  k <- as.integer(config$m4_source_k %||% min(4L, max(2L, floor(sqrt(nrow(x))))))
  k <- max(2L, min(k, nrow(x) - 1L))
  scaled <- scale(x)
  scaled[!is.finite(scaled)] <- 0
  set.seed(as.integer(config$seed %||% 1234L))
  km <- stats::kmeans(scaled, centers = k, nstart = 25)
  pca <- stats::prcomp(scaled, center = FALSE, scale. = FALSE)
  scores <- as.data.frame(pca$x[, seq_len(min(2, ncol(pca$x))), drop = FALSE])
  if (!"PC2" %in% names(scores)) scores$PC2 <- 0
  clusters <- features |>
    dplyr::mutate(cluster = paste0("C", km$cluster), pc1 = scores$PC1, pc2 = scores$PC2)
  list(status = "success", clusters = clusters, centers = km$centers)
}

m4_compute_narrative <- function(data, config = biblio_config()) {
  totals <- data$sources$totals %||% list()
  conc <- data$concentration$metrics %||% tibble::tibble()
  get_metric <- function(nm) {
    if (!is.data.frame(conc) || !nm %in% conc$metric) return(NA_real_)
    ieee_safe_num(conc$value[conc$metric == nm][1])
  }
  core <- data$bradford$zone_summary %||% tibble::tibble()
  core_share <- if (is.data.frame(core) && nrow(core) > 0 && "Zone 1: Core" %in% core$bradford_zone) {
    core$tp[core$bradford_zone == "Zone 1: Core"][1] / sum(core$tp, na.rm = TRUE)
  } else NA_real_
  top_growth <- data$growth$growth %||% tibble::tibble()
  max_growth <- if (is.data.frame(top_growth) && nrow(top_growth) > 0) max(top_growth$tp_slope, na.rm = TRUE) else NA_real_
  metrics <- ieee_bind_metric_rows(
    ieee_metric_row("M4", "Coverage", "Active sources", totals$total_sources %||% NA_real_, score = log1p(totals$total_sources %||% NA_real_) / log1p(100), digits = 0, interpretation = "Shows venue breadth in the corpus."),
    ieee_metric_row("M4", "Concentration", "Source Gini", get_metric("source_gini"), score = get_metric("source_gini"), digits = 3, interpretation = "Measures whether publication output is concentrated in a small venue core."),
    ieee_metric_row("M4", "Concentration", "Top-5 source share", get_metric("top_5_share") * 100, score = get_metric("top_5_share"), units = "%", digits = 1, interpretation = "Quantifies dependence on the leading five venues."),
    ieee_metric_row("M4", "Bradford", "Core-zone share", core_share * 100, score = core_share, units = "%", digits = 1, interpretation = "Shows how much output is captured by Bradford core sources."),
    ieee_metric_row("M4", "Impact", "Total source citations", totals$total_citations %||% NA_real_, score = log1p(totals$total_citations %||% NA_real_) / log1p(max(totals$total_citations %||% NA_real_, 1000, na.rm = TRUE)), digits = 0, interpretation = "Summarizes citation mass across venues."),
    ieee_metric_row("M4", "Growth", "Max TP slope", max_growth, score = log1p(max_growth) / log1p(max(max_growth, 10, na.rm = TRUE)), digits = 2, interpretation = "Identifies the strongest emerging venue trajectory.")
  )
  list(status = if (nrow(metrics) > 0) "success" else "empty", metrics = metrics, narrative = ieee_narrative_lines(metrics, max_lines = 8))
}

m4_normalize_source <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | !nzchar(x)] <- NA_character_
  x <- gsub("\\s+", " ", x)
  x
}

m4_num_col <- function(input, candidates) {
  col <- candidates[candidates %in% names(input)][1]
  if (is.na(col)) return(rep(NA_real_, nrow(input)))
  suppressWarnings(as.numeric(input[[col]]))
}

m4_text_col <- function(input, candidates) {
  col <- candidates[candidates %in% names(input)][1]
  if (is.na(col)) return(rep("", nrow(input)))
  as.character(input[[col]])
}

m4_h_index <- function(citations) {
  c <- sort(suppressWarnings(as.numeric(citations)), decreasing = TRUE)
  c <- c[is.finite(c)]
  if (length(c) == 0) return(0L)
  sum(c >= seq_along(c))
}

m4_g_index <- function(citations) {
  c <- sort(suppressWarnings(as.numeric(citations)), decreasing = TRUE)
  c <- c[is.finite(c)]
  if (length(c) == 0) return(0L)
  cs <- cumsum(c)
  sum(cs >= seq_along(c)^2)
}

m4_slope <- function(year, value) {
  keep <- is.finite(year) & is.finite(value)
  year <- year[keep]
  value <- value[keep]
  if (length(unique(year)) < 2) return(0)
  fit <- tryCatch(stats::lm(value ~ year), error = function(e) NULL)
  if (is.null(fit)) return(0)
  out <- suppressWarnings(as.numeric(stats::coef(fit)[2]))
  if (length(out) == 1 && is.finite(out)) out else 0
}

m4_gini <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x) & x >= 0]
  if (length(x) == 0 || sum(x) == 0) return(NA_real_)
  x <- sort(x)
  n <- length(x)
  (2 * sum(seq_len(n) * x) / (n * sum(x))) - (n + 1) / n
}
