# ============================================================================
# m0_enrichment_audit.R - Coverage summary for free metadata enrichment
# ============================================================================

m0_build_enrichment_audit <- function(before, after, enrichment = list()) {
  before <- if (is.data.frame(before)) before else data.frame()
  after <- if (is.data.frame(after)) after else data.frame()
  fields <- c("DI", "AB", "CR", "SO", "JI", "ISSN", "FU", "C1", "AU_UN", "AU1_UN", "TC")

  coverage <- dplyr::bind_rows(lapply(fields, function(field) {
    data.frame(
      field = field,
      before_non_missing = m0_enrichment_non_missing(before, field),
      after_non_missing = m0_enrichment_non_missing(after, field),
      records = max(nrow(before), nrow(after)),
      stringsAsFactors = FALSE
    )
  })) |>
    dplyr::mutate(
      before_coverage = ifelse(.data$records > 0, .data$before_non_missing / .data$records, NA_real_),
      after_coverage = ifelse(.data$records > 0, .data$after_non_missing / .data$records, NA_real_),
      coverage_gain = .data$after_coverage - .data$before_coverage
    )

  provider_rows <- list()
  for (nm in names(enrichment)) {
    obj <- enrichment[[nm]]
    if (is.data.frame(obj)) {
      provider_rows[[length(provider_rows) + 1L]] <- data.frame(
        provider = nm,
        rows = nrow(obj),
        columns = ncol(obj),
        status = if (nrow(obj) > 0) "available" else "empty",
        stringsAsFactors = FALSE
      )
    } else if (is.list(obj)) {
      provider_rows[[length(provider_rows) + 1L]] <- data.frame(
        provider = nm,
        rows = length(obj),
        columns = NA_integer_,
        status = if (length(obj) > 0) "available" else "empty",
        stringsAsFactors = FALSE
      )
    }
  }
  providers <- if (length(provider_rows) > 0) dplyr::bind_rows(provider_rows) else data.frame(provider = character(), rows = integer(), columns = integer(), status = character(), stringsAsFactors = FALSE)

  list(
    status = if (length(enrichment) > 0) "success" else "not_run",
    coverage = coverage,
    providers = providers,
    notes = if (length(enrichment) > 0) {
      "Enrichment coverage is reported by field; gains depend on open API availability and input identifiers."
    } else {
      "Enrichment was not enabled for this run."
    }
  )
}

m0_enrichment_non_missing <- function(df, field) {
  if (!is.data.frame(df) || !field %in% names(df)) {
    return(0L)
  }
  vals <- df[[field]]
  sum(!is.na(vals) & nzchar(trimws(as.character(vals))), na.rm = TRUE)
}
