# ============================================================================
# m1_compute_bradford.R - Bradford metric (CORRECT ALGORITHM)
#
# Bradford's Law states:
# - Zone 1: Top sources producing 1/3 of articles (core)
# - Zone 2: Next group producing 1/3 (moderate)
# - Zone 3: Remaining sources producing 1/3 (peripheral)
# - Sources in zones follow geometric progression: 1 : n : n²
# ============================================================================

#' @export
compute_m1_bradford <- function(input, config = biblio_config()) {
  if (!is.data.frame(input) || nrow(input) == 0) {
    return(list(bradford_table = tibble::tibble(), core_sources = m1_empty_rank_table(),
                zone_summary = list(), status = "error"))
  }

  source_df <- tryCatch({
    bibliometrix::bradford(input)$table
  }, error = function(e) m1_bradford_manual(input))

  if (nrow(source_df) == 0) {
    return(list(bradford_table = tibble::tibble(), core_sources = m1_empty_rank_table(),
                zone_summary = list(), status = "error"))
  }

  colnames(source_df) <- make.unique(colnames(source_df))

  # Ensure Zone column exists
  if (!"Zone" %in% colnames(source_df)) {
    source_df <- m1_bradford_assign_zones(source_df)
  }

  bradford_table <- tibble::tibble(
    rank = seq_len(nrow(source_df)),
    source = source_df$SO,
    freq = source_df$Freq,
    zone = source_df$Zone
  )

  core <- source_df[source_df$Zone == "Zone 1: Core", ]
  core_sources <- if (nrow(core) > 0) {
    tibble::tibble(rank = seq_len(nrow(core)), label = core$SO, value = core$Freq)
  } else {
    m1_empty_rank_table()
  }

  zone_summary <- list(
    zone1 = list(n_sources = sum(source_df$Zone == "Zone 1: Core"),
                 n_articles = sum(source_df$Freq[source_df$Zone == "Zone 1: Core"])),
    zone2 = list(n_sources = sum(source_df$Zone == "Zone 2: Moderate"),
                 n_articles = sum(source_df$Freq[source_df$Zone == "Zone 2: Moderate"])),
    zone3 = list(n_sources = sum(source_df$Zone == "Zone 3: Peripheral"),
                 n_articles = sum(source_df$Freq[source_df$Zone == "Zone 3: Peripheral"]))
  )

  list(bradford_table = bradford_table, core_sources = core_sources,
       zone_summary = zone_summary, status = "success")
}

#' Manual Bradford computation
m1_bradford_manual <- function(input) {
  if (!"SO" %in% names(input)) return(data.frame())

  source_counts <- table(input$SO, useNA = "no")
  sdf <- data.frame(SO = names(source_counts), Freq = as.integer(source_counts), stringsAsFactors = FALSE)
  sdf <- sdf[order(-sdf$Freq), ]
  sdf$Rank <- seq_len(nrow(sdf))
  sdf$CumFreq <- cumsum(sdf$Freq)

  m1_bradford_assign_zones(sdf)
}

#' Assign Bradford zones using cumulative article distribution
m1_bradford_assign_zones <- function(sdf) {
  total <- sum(sdf$Freq)
  sdf$Zone <- ifelse(
    sdf$CumFreq <= total / 3, "Zone 1: Core",
    ifelse(sdf$CumFreq <= 2 * total / 3, "Zone 2: Moderate",
           "Zone 3: Peripheral"))
  sdf
}
