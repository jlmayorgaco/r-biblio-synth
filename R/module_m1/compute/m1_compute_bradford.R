# ============================================================================
# m1_compute_bradford.R - Bradford metric for M1
# ============================================================================

#' @export
compute_m1_bradford <- function(input, config = biblio_config()) {
  if (!is.data.frame(input) || nrow(input) == 0) {
    return(list(bradford_table = tibble::tibble(), core_sources = m1_empty_rank_table(), zone_summary = list(), status = "error"))
  }

  # Try bibliometrix first
  source_df <- tryCatch({
    br <- bibliometrix::bradford(input)
    br$table
  }, error = function(e) {
    # Manual Bradford computation
    if ("SO" %in% names(input)) {
      source_counts <- table(input$SO, useNA = "no")
      sdf <- data.frame(SO = names(source_counts), Freq = as.integer(source_counts), stringsAsFactors = FALSE)
      sdf <- sdf[order(-sdf$Freq), ]
      sdf$Rank <- seq_len(nrow(sdf))

      # Assign zones (roughly 1/3 each)
      n <- nrow(sdf)
      n3 <- ceiling(n / 3)
      sdf$Zone <- c(
        rep("Zone 1: Core", min(n3, n)),
        rep("Zone 2: Moderate", min(n3, max(0, n - n3))),
        rep("Zone 3: Peripheral", max(0, n - 2 * n3))
      )
      sdf
    } else {
      data.frame()
    }
  })

  if (nrow(source_df) > 0) {
    colnames(source_df) <- make.unique(colnames(source_df))
    bradford_table <- tibble::tibble(
      rank = seq_len(nrow(source_df)),
      source = source_df$SO,
      freq = source_df$Freq,
      zone = source_df$Zone
    )

    core <- source_df[source_df$Zone == "Zone 1: Core", ]
    core_sources <- tibble::tibble(
      rank = seq_len(nrow(core)),
      label = core$SO,
      value = core$Freq
    )

    zone_summary <- list(
      zone1 = list(n_sources = sum(source_df$Zone == "Zone 1: Core"), n_articles = sum(source_df$Freq[source_df$Zone == "Zone 1: Core"])),
      zone2 = list(n_sources = sum(source_df$Zone == "Zone 2: Moderate"), n_articles = sum(source_df$Freq[source_df$Zone == "Zone 2: Moderate"])),
      zone3 = list(n_sources = sum(source_df$Zone == "Zone 3: Peripheral"), n_articles = sum(source_df$Freq[source_df$Zone == "Zone 3: Peripheral"]))
    )
  } else {
    bradford_table <- tibble::tibble()
    core_sources <- m1_empty_rank_table()
    zone_summary <- list()
  }

  list(bradford_table = bradford_table, core_sources = core_sources, zone_summary = zone_summary, status = "success")
}
