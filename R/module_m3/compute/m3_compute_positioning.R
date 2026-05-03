# ============================================================================
# m3_compute_positioning.R - Country positioning and trajectory metrics
# ============================================================================

m3_compute_positioning <- function(prepared_data, scp_mcp_data = list(), config = biblio_config()) {
  summary <- prepared_data$country_summary %||% tibble::tibble()
  doc_level <- prepared_data$country_doc_level %||% tibble::tibble()
  annual <- prepared_data$country_annual %||% tibble::tibble()
  annual_citations <- prepared_data$country_annual_citations %||% tibble::tibble()
  scp_mcp <- scp_mcp_data$scp_mcp %||% tibble::tibble()

  if (!is.data.frame(summary) || nrow(summary) == 0 || !"country" %in% names(summary)) {
    return(list(status = "empty", reason = "Country summary unavailable.", table = tibble::tibble(), clusters = tibble::tibble()))
  }

  base <- summary |>
    dplyr::transmute(
      country = as.character(.data$country),
      tp = suppressWarnings(as.numeric(.data$article_count)),
      tc = suppressWarnings(as.numeric(.data$total_citations %||% 0))
    )

  if (is.data.frame(scp_mcp) && nrow(scp_mcp) > 0) {
    base <- base |>
      dplyr::left_join(
        scp_mcp |>
          dplyr::transmute(
            country = as.character(.data$country),
            scp = suppressWarnings(as.numeric(.data$scp)),
            mcp = suppressWarnings(as.numeric(.data$mcp)),
            mcp_ratio = suppressWarnings(as.numeric(.data$mcp_ratio))
          ),
        by = "country"
      )
  } else {
    base$scp <- NA_real_
    base$mcp <- NA_real_
    base$mcp_ratio <- NA_real_
  }

  base <- base |>
    dplyr::mutate(
      scp = dplyr::coalesce(.data$scp, 0),
      mcp = dplyr::coalesce(.data$mcp, 0),
      tp = dplyr::coalesce(.data$tp, .data$scp + .data$mcp, 0),
      tc = dplyr::coalesce(.data$tc, 0),
      citations_per_paper = dplyr::if_else(.data$tp > 0, .data$tc / .data$tp, 0),
      mcp_ratio = dplyr::if_else(
        is.finite(.data$mcp_ratio),
        ifelse(.data$mcp_ratio > 1, .data$mcp_ratio / 100, .data$mcp_ratio),
        dplyr::if_else((.data$scp + .data$mcp) > 0, .data$mcp / (.data$scp + .data$mcp), NA_real_)
      )
    )

  scp_mcp_trends <- m3_positioning_scp_mcp_trends(doc_level)
  tp_trends <- m3_positioning_country_slope(annual, value_col = "article_count", slope_name = "tp_slope")
  tc_trends <- m3_positioning_country_slope(annual_citations, value_col = "total_citations", slope_name = "tc_slope")

  table <- base |>
    dplyr::left_join(scp_mcp_trends, by = "country") |>
    dplyr::left_join(tp_trends, by = "country") |>
    dplyr::left_join(tc_trends, by = "country") |>
    dplyr::mutate(
      scp_slope = dplyr::coalesce(.data$scp_slope, 0),
      mcp_slope = dplyr::coalesce(.data$mcp_slope, 0),
      tp_slope = dplyr::coalesce(.data$tp_slope, 0),
      tc_slope = dplyr::coalesce(.data$tc_slope, 0)
    )

  horizon <- as.integer(config$m3_positioning_forecast_horizon %||% config$forecast_horizon %||% 3L)
  if (!is.finite(horizon) || horizon < 1L) horizon <- 3L

  table <- table |>
    dplyr::mutate(
      scp_future = pmax(0, .data$scp + horizon * .data$scp_slope),
      mcp_future = pmax(0, .data$mcp + horizon * .data$mcp_slope),
      tp_future = pmax(0, .data$tp + horizon * .data$tp_slope),
      tc_future = pmax(0, .data$tc + horizon * .data$tc_slope)
    )

  cluster_df <- m3_positioning_kmeans(table, config)
  if (is.data.frame(cluster_df) && nrow(cluster_df) > 0) {
    table <- table |> dplyr::left_join(cluster_df |> dplyr::select("country", "cluster"), by = "country")
  } else {
    table$cluster <- NA_character_
  }

  list(
    status = "success",
    horizon = horizon,
    table = table,
    clusters = cluster_df,
    quadrant_notes = m3_positioning_quadrant_notes()
  )
}

m3_positioning_scp_mcp_trends <- function(doc_level) {
  if (!is.data.frame(doc_level) || nrow(doc_level) == 0 ||
      !all(c("doc_id", "country") %in% names(doc_level))) {
    return(tibble::tibble(country = character(), scp_slope = numeric(), mcp_slope = numeric()))
  }
  year_col <- if ("year" %in% names(doc_level)) "year" else if ("PY" %in% names(doc_level)) "PY" else NULL
  if (is.null(year_col)) {
    return(tibble::tibble(country = character(), scp_slope = numeric(), mcp_slope = numeric()))
  }

  doc_counts <- doc_level |>
    dplyr::filter(!is.na(.data$country), nzchar(as.character(.data$country))) |>
    dplyr::group_by(.data$doc_id) |>
    dplyr::summarise(n_countries = dplyr::n_distinct(.data$country), .groups = "drop")

  annual <- doc_level |>
    dplyr::filter(!is.na(.data$country), nzchar(as.character(.data$country))) |>
    dplyr::left_join(doc_counts, by = "doc_id") |>
    dplyr::mutate(
      year = suppressWarnings(as.numeric(.data[[year_col]])),
      collaboration_type = ifelse(.data$n_countries > 1, "mcp", "scp")
    ) |>
    dplyr::filter(is.finite(.data$year)) |>
    dplyr::count(.data$country, .data$year, .data$collaboration_type, name = "n") |>
    tidyr::pivot_wider(names_from = "collaboration_type", values_from = "n", values_fill = 0)

  if (!"scp" %in% names(annual)) annual$scp <- 0
  if (!"mcp" %in% names(annual)) annual$mcp <- 0

  countries <- sort(unique(annual$country))
  dplyr::bind_rows(lapply(countries, function(country_i) {
    df <- annual[annual$country == country_i, , drop = FALSE]
    tibble::tibble(
      country = country_i,
      scp_slope = m3_positioning_slope(df$year, df$scp),
      mcp_slope = m3_positioning_slope(df$year, df$mcp)
    )
  }))
}

m3_positioning_country_slope <- function(annual, value_col, slope_name) {
  if (!is.data.frame(annual) || nrow(annual) == 0 ||
      !"country" %in% names(annual) || !value_col %in% names(annual)) {
    out <- tibble::tibble(country = character(), slope = numeric())
    names(out)[2] <- slope_name
    return(out)
  }
  year_col <- if ("year" %in% names(annual)) "year" else if ("PY" %in% names(annual)) "PY" else NULL
  if (is.null(year_col)) {
    out <- tibble::tibble(country = character(), slope = numeric())
    names(out)[2] <- slope_name
    return(out)
  }

  countries <- sort(unique(annual$country))
  out <- dplyr::bind_rows(lapply(countries, function(country_i) {
    df <- annual[annual$country == country_i, , drop = FALSE]
    tibble::tibble(
      country = country_i,
      slope = m3_positioning_slope(
        suppressWarnings(as.numeric(df[[year_col]])),
        suppressWarnings(as.numeric(df[[value_col]]))
      )
    )
  }))
  names(out)[2] <- slope_name
  out
}

m3_positioning_slope <- function(year, value) {
  keep <- is.finite(year) & is.finite(value)
  year <- year[keep]
  value <- value[keep]
  if (length(unique(year)) < 2 || length(value) < 2) return(0)
  fit <- tryCatch(stats::lm(value ~ year), error = function(e) NULL)
  if (is.null(fit)) return(0)
  slope <- suppressWarnings(as.numeric(stats::coef(fit)[2]))
  if (length(slope) == 1L && is.finite(slope)) slope else 0
}

m3_positioning_kmeans <- function(table, config = biblio_config()) {
  if (!is.data.frame(table) || nrow(table) < 3) {
    return(tibble::tibble())
  }
  features <- table |>
    dplyr::transmute(
      country = .data$country,
      tp = log1p(.data$tp),
      tc = log1p(.data$tc),
      scp = log1p(.data$scp),
      mcp = log1p(.data$mcp),
      mcp_ratio = dplyr::coalesce(.data$mcp_ratio, 0),
      citations_per_paper = log1p(.data$citations_per_paper)
    )
  x <- as.data.frame(features[, setdiff(names(features), "country"), drop = FALSE])
  keep <- stats::complete.cases(x)
  features <- features[keep, , drop = FALSE]
  x <- x[keep, , drop = FALSE]
  if (nrow(x) < 3) return(tibble::tibble())

  k <- as.integer(config$m3_positioning_k %||% min(4L, max(2L, floor(sqrt(nrow(x))))))
  k <- max(2L, min(k, nrow(x) - 1L))
  scaled <- scale(x)
  set.seed(as.integer(config$seed %||% 1234L))
  km <- stats::kmeans(scaled, centers = k, nstart = 25)
  pca <- stats::prcomp(scaled, center = FALSE, scale. = FALSE)
  scores <- as.data.frame(pca$x[, seq_len(min(2, ncol(pca$x))), drop = FALSE])
  if (!"PC2" %in% names(scores)) scores$PC2 <- 0
  tibble::tibble(
    country = features$country,
    cluster = paste0("C", km$cluster),
    pc1 = scores$PC1,
    pc2 = scores$PC2,
    cluster_size = as.integer(tabulate(km$cluster, nbins = k)[km$cluster])
  )
}

m3_positioning_quadrant_notes <- function() {
  list(
    scp_mcp = c(
      "High SCP / high MCP: large countries with strong domestic and international production bases.",
      "High SCP / low MCP: domestically anchored producers with lower internationalization.",
      "Low SCP / high MCP: collaboration-dependent connectors.",
      "Low SCP / low MCP: emerging or low-volume contributors."
    ),
    tc_tp = c(
      "High TP / high TC: high-volume, high-impact leaders.",
      "High TP / low TC: high-volume producers whose citation impact lags output.",
      "Low TP / high TC: selective high-impact contributors.",
      "Low TP / low TC: emerging or peripheral contributors."
    )
  )
}
