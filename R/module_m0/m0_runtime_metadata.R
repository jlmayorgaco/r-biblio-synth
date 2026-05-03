# ============================================================================
# m0_runtime_metadata.R - Runtime summaries for M0 orchestration
# ============================================================================

#' Build per-source runtime summary
#' @keywords internal
m0_build_source_summary <- function(raw_list, sources = list()) {
  if (!is.list(raw_list) || length(raw_list) == 0) {
    return(data.frame())
  }

  rows <- lapply(names(raw_list), function(nm) {
    spec <- sources[[nm]] %||% list()
    df <- raw_list[[nm]]
    source_file <- spec[["file"]] %||% NA_character_
    source_files <- spec[["files"]] %||% character()
    if ((is.na(source_file) || !nzchar(as.character(source_file)[1])) && length(source_files) > 0) {
      source_file <- paste(as.character(source_files), collapse = ";")
    }

    data.frame(
      source_name = nm,
      db = spec[["db"]] %||% NA_character_,
      mode = if (m0_is_api_source_spec(spec)) "api" else "file",
      format = spec[["format"]] %||% "bibtex",
      prisma_role = m0_resolve_prisma_role(spec),
      file = source_file,
      endpoint = spec[["endpoint"]] %||% NA_character_,
      query = spec[["query"]] %||% NA_character_,
      search_date = spec[["search_date"]] %||% NA_character_,
      n_records = if (is.data.frame(df)) nrow(df) else 0L,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

#' Build deduplication summary table
#' @keywords internal
m0_build_dedup_summary <- function(raw_list, merged, config = biblio_config()) {
  total_raw <- sum(vapply(raw_list, function(df) if (is.data.frame(df)) nrow(df) else 0L, integer(1)))
  total_unique <- if (is.data.frame(merged)) nrow(merged) else 0L
  duplicates_removed <- max(total_raw - total_unique, 0L)

  method_counts <- if (is.data.frame(merged) && "M0_DEDUP_METHOD" %in% names(merged)) {
    stats::na.omit(trimws(as.character(merged$M0_DEDUP_METHOD)))
  } else {
    character(0)
  }
  method_counts <- if (length(method_counts) > 0) table(method_counts) else integer(0)

  data.frame(
    total_raw_records = total_raw,
    unique_records = total_unique,
    duplicates_removed = duplicates_removed,
    dedup_rate = safe_divide(duplicates_removed, total_raw, default = 0),
    methods_configured = paste(config$dedup_method %||% character(), collapse = ";"),
    methods_applied = if (length(method_counts) > 0) {
      paste(sprintf("%s=%s", names(method_counts), as.integer(method_counts)), collapse = ";")
    } else {
      ""
    },
    multi_source_records = if (is.data.frame(merged) && "M0_PROVENANCE_COUNT" %in% names(merged)) {
      sum(suppressWarnings(as.numeric(merged$M0_PROVENANCE_COUNT)) > 1, na.rm = TRUE)
    } else {
      0L
    },
    average_provenance_count = if (is.data.frame(merged) && "M0_PROVENANCE_COUNT" %in% names(merged)) {
      mean(suppressWarnings(as.numeric(merged$M0_PROVENANCE_COUNT)), na.rm = TRUE)
    } else {
      1
    },
    stringsAsFactors = FALSE
  )
}

#' Build document-level provenance table
#' @keywords internal
m0_build_provenance_table <- function(merged) {
  if (!is.data.frame(merged) || nrow(merged) == 0) {
    return(data.frame())
  }

  cols <- c(
    "M0_DOC_ID", "TI", "PY", "DI", "DT",
    "SOURCE_DB", "SOURCE_TAG",
    "M0_PRIMARY_SOURCE_DB", "M0_PRIMARY_SOURCE_TAG",
    "M0_SOURCE_DBS", "M0_SOURCE_TAGS",
    "M0_PROVENANCE_COUNT", "M0_DEDUP_METHOD",
    "M0_CONFLICT_FIELDS", "M0_CONFLICT_COUNT"
  )
  out <- merged[, intersect(cols, names(merged)), drop = FALSE]

  if ("TI" %in% names(out)) names(out)[names(out) == "TI"] <- "title"
  if ("PY" %in% names(out)) names(out)[names(out) == "PY"] <- "year"
  if ("DI" %in% names(out)) names(out)[names(out) == "DI"] <- "doi"
  if ("DT" %in% names(out)) names(out)[names(out) == "DT"] <- "doc_type"
  if ("SOURCE_DB" %in% names(out)) names(out)[names(out) == "SOURCE_DB"] <- "source_db_primary"
  if ("SOURCE_TAG" %in% names(out)) names(out)[names(out) == "SOURCE_TAG"] <- "source_tag_primary"
  if ("M0_PRIMARY_SOURCE_DB" %in% names(out)) names(out)[names(out) == "M0_PRIMARY_SOURCE_DB"] <- "source_db_selected"
  if ("M0_PRIMARY_SOURCE_TAG" %in% names(out)) names(out)[names(out) == "M0_PRIMARY_SOURCE_TAG"] <- "source_tag_selected"
  if ("M0_SOURCE_DBS" %in% names(out)) names(out)[names(out) == "M0_SOURCE_DBS"] <- "source_dbs"
  if ("M0_SOURCE_TAGS" %in% names(out)) names(out)[names(out) == "M0_SOURCE_TAGS"] <- "source_tags"
  if ("M0_PROVENANCE_COUNT" %in% names(out)) names(out)[names(out) == "M0_PROVENANCE_COUNT"] <- "provenance_count"
  if ("M0_DEDUP_METHOD" %in% names(out)) names(out)[names(out) == "M0_DEDUP_METHOD"] <- "dedup_method"
  if ("M0_CONFLICT_FIELDS" %in% names(out)) names(out)[names(out) == "M0_CONFLICT_FIELDS"] <- "conflict_fields"
  if ("M0_CONFLICT_COUNT" %in% names(out)) names(out)[names(out) == "M0_CONFLICT_COUNT"] <- "conflict_count"

  out
}

#' Resolve PRISMA role for a source specification
#' @keywords internal
m0_resolve_prisma_role <- function(spec = list()) {
  role <- tolower(trimws(as.character(spec$prisma_role %||% NA_character_)))
  if (identical(role, "database") || identical(role, "other")) {
    return(role)
  }

  db <- tolower(trimws(as.character(spec$db %||% NA_character_)))
  if (db %in% c("scopus", "wos", "openalex")) {
    return("database")
  }

  "other"
}
