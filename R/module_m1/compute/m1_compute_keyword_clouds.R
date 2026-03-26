# ============================================================================
# module_m1/compute/m1_compute_keyword_clouds.R - Keyword cloud computation
# ============================================================================

#' @export
compute_m1_keyword_clouds <- function(bib_data, n_chunks = 9, config = biblio_config()) {
  if (!is.data.frame(bib_data) || !all(c("PY", "DE") %in% names(bib_data))) {
    return(list(status = "error", chunks = list(), grid_data = data.frame()))
  }

  years <- as.numeric(bib_data$PY)
  year_range <- range(years, na.rm = TRUE)
  chunk_size <- diff(year_range) / n_chunks

  # Split data into temporal chunks
  chunks <- list()
  for (i in seq_len(n_chunks)) {
    chunk_start <- year_range[1] + (i - 1) * chunk_size
    chunk_end <- year_range[1] + i * chunk_size
    chunk_data <- bib_data[years >= chunk_start & years <= chunk_end, ]

    # Extract keywords
    kw <- chunk_data$DE[!is.na(chunk_data$DE)]
    kw <- trimws(unlist(strsplit(kw, ";")))
    kw <- kw[kw != ""]

    if (length(kw) > 0) {
      kw_counts <- sort(table(kw), decreasing = TRUE)
      chunks[[paste0("chunk_", i)]] <- data.frame(
        keyword = names(kw_counts),
        freq = as.integer(kw_counts),
        chunk = i,
        year_start = round(chunk_start),
        year_end = round(chunk_end),
        stringsAsFactors = FALSE
      )
    }
  }

  # Combine all chunks for grid
  grid_data <- do.call(rbind, chunks)
  grid_data <- head(grid_data[order(-grid_data$freq), ], 200)  # Top 200 overall

  list(
    status = "success",
    chunks = chunks,
    grid_data = grid_data,
    year_range = year_range,
    n_chunks = n_chunks
  )
}
