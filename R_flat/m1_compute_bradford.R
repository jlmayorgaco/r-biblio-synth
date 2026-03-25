# ============================================================================
# m1_compute_bradford.R - Bradford metric for M1
# ============================================================================

#' Compute M1 Bradford analysis
#'
#' @param input A data frame of bibliographic data.
#' @param config A configuration list.
#' @return A list with \code{bradford_table}, \code{core_sources}, \code{status}.
#' @export
compute_m1_bradford <- function(input, config = biblio_config()) {
  if (!is.data.frame(input) || nrow(input) == 0) {
    return(list(
      bradford_table = tibble::tibble(),
      core_sources   = m1_empty_rank_table(),
      zone_summary   = list(),
      status         = "error"
    ))
  }

  # Use bibliometrix Bradford function
  bradford_result <- tryCatch(
    bibliometrix::bradford(input),
    error = function(e) NULL
  )

  if (is.null(bradford_result) || is.null(bradford_result$table)) {
    # Fallback: compute Bradford manually
    source_counts <- table(input$SO, useNA = "no")
    source_df <- data.frame(
      SO    = names(source_counts),
      Freq  = as.integer(source_counts),
      stringsAsFactors = FALSE
    )
    source_df <- source_df[order(-source_df$Freq), ]
    source_df$Rank <- seq_len(nrow(source_df))

    # Assign zones (rough: 1/3 of sources in each zone)
    n <- nrow(source_df)
    n3 <- ceiling(n / 3)
    source_df$Zone <- c(
      rep("Zone 1: Core", min(n3, n)),
      rep("Zone 2: Moderate", min(n3, max(0, n - n3))),
      rep("Zone 3: Peripheral", max(0, n - 2 * n3))
    )
  } else {
    source_df <- bradford_result$table
    colnames(source_df) <- make.unique(colnames(source_df))
  }

  # Build bradford_table
  bradford_table <- tibble::tibble(
    rank   = seq_len(nrow(source_df)),
    source = source_df$SO,
    freq   = source_df$Freq,
    zone   = source_df$Zone
  )

  # Core sources (Zone 1)
  core <- source_df[source_df$Zone == "Zone 1: Core", ]
  core_sources <- tibble::tibble(
    rank  = seq_len(nrow(core)),
    label = core$SO,
    value = core$Freq
  )

  # Zone summary
  zone_summary <- list(
    zone1 = list(
      n_sources = sum(source_df$Zone == "Zone 1: Core"),
      n_articles = sum(source_df$Freq[source_df$Zone == "Zone 1: Core"])
    ),
    zone2 = list(
      n_sources = sum(source_df$Zone == "Zone 2: Moderate"),
      n_articles = sum(source_df$Freq[source_df$Zone == "Zone 2: Moderate"])
    ),
    zone3 = list(
      n_sources = sum(source_df$Zone == "Zone 3: Peripheral"),
      n_articles = sum(source_df$Freq[source_df$Zone == "Zone 3: Peripheral"])
    )
  )

  list(
    bradford_table = bradford_table,
    core_sources   = core_sources,
    zone_summary   = zone_summary,
    status         = "success"
  )
}
