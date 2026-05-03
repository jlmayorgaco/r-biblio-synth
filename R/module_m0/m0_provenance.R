# ============================================================================
# m0_provenance.R - Field-level provenance and conflict resolution
# ============================================================================

m0_resolve_duplicate_group <- function(group_df, config, method) {
  config <- merge_biblio_config(config)
  group_df <- data.frame(group_df, stringsAsFactors = FALSE)
  order_idx <- m0_rank_group_rows(group_df, config)
  resolved <- group_df[order_idx[1], , drop = FALSE]

  field_provenance <- list()
  field_conflicts <- list()
  skip_cols <- c(
    "M0_SOURCE_DBS", "M0_SOURCE_TAGS", "M0_PROVENANCE_COUNT", "M0_DEDUP_METHOD",
    "M0_FIELD_PROVENANCE", "M0_FIELD_CONFLICTS", "M0_PRIMARY_SOURCE_DB",
    "M0_PRIMARY_SOURCE_TAG", "M0_CONFLICT_FIELDS", "M0_CONFLICT_COUNT",
    "M0_RESOLUTION_NOTES"
  )

  target_cols <- setdiff(names(group_df), skip_cols)
  for (field in target_cols) {
    field_info <- m0_resolve_group_field(group_df, field, config, order_idx)
    resolved[[field]] <- field_info$value
    field_provenance[[field]] <- list(
      source_db = field_info$source_db,
      source_tag = field_info$source_tag,
      strategy = field_info$strategy,
      n_candidates = field_info$n_candidates
    )
    if (isTRUE(field_info$conflict)) {
      field_conflicts[[field]] <- list(
        chosen_value = m0_stringify_value(field_info$value),
        alternatives = field_info$alternatives,
        source_tags = field_info$source_tags,
        source_dbs = field_info$source_dbs,
        strategy = field_info$strategy
      )
    }
  }

  combined_dbs <- unique(unlist(strsplit(paste(group_df$SOURCE_DB, collapse = ";"), ";", fixed = TRUE)))
  combined_dbs <- trimws(combined_dbs[nzchar(trimws(combined_dbs))])
  combined_tags <- unique(unlist(strsplit(paste(group_df$SOURCE_TAG, collapse = ";"), ";", fixed = TRUE)))
  combined_tags <- trimws(combined_tags[nzchar(trimws(combined_tags))])

  resolved$SOURCE_DB <- toupper(trimws(as.character(group_df$SOURCE_DB[order_idx[1]] %||% NA_character_)))
  resolved$SOURCE_TAG <- trimws(as.character(group_df$SOURCE_TAG[order_idx[1]] %||% NA_character_))
  resolved$M0_PRIMARY_SOURCE_DB <- resolved$SOURCE_DB
  resolved$M0_PRIMARY_SOURCE_TAG <- resolved$SOURCE_TAG
  resolved$M0_SOURCE_DBS <- paste(unique(combined_dbs), collapse = ";")
  resolved$M0_SOURCE_TAGS <- paste(unique(combined_tags), collapse = ";")
  resolved$M0_PROVENANCE_COUNT <- nrow(group_df)
  resolved$M0_DEDUP_METHOD <- method
  resolved$M0_FIELD_PROVENANCE <- jsonlite::toJSON(field_provenance, auto_unbox = TRUE, null = "null")
  resolved$M0_FIELD_CONFLICTS <- jsonlite::toJSON(field_conflicts, auto_unbox = TRUE, null = "null")
  resolved$M0_CONFLICT_FIELDS <- paste(names(field_conflicts), collapse = ";")
  resolved$M0_CONFLICT_COUNT <- length(field_conflicts)
  resolved$M0_RESOLUTION_NOTES <- paste0("Resolved ", nrow(group_df), " records using ", method)

  resolved
}

m0_resolve_group_field <- function(group_df, field, config, order_idx) {
  values <- group_df[[field]]
  present <- which(m0_has_value(values))

  if (length(present) == 0) {
    return(list(
      value = NA,
      source_db = NA_character_,
      source_tag = NA_character_,
      source_dbs = character(0),
      source_tags = character(0),
      strategy = "missing",
      n_candidates = 0L,
      conflict = FALSE,
      alternatives = character(0)
    ))
  }

  strategy <- m0_group_merge_strategy(field, values)
  unique_values <- unique(m0_stringify_value(values[present]))

  if (identical(strategy, "union")) {
    merged <- m0_merge_union_values(values[present])
    src_dbs <- unique(toupper(trimws(as.character(group_df$SOURCE_DB[present]))))
    src_tags <- unique(trimws(as.character(group_df$SOURCE_TAG[present])))
    return(list(
      value = merged,
      source_db = paste(stats::na.omit(src_dbs), collapse = ";"),
      source_tag = paste(stats::na.omit(src_tags), collapse = ";"),
      source_dbs = src_dbs,
      source_tags = src_tags,
      strategy = strategy,
      n_candidates = length(present),
      conflict = length(unique_values) > 1,
      alternatives = unique_values
    ))
  }

  candidate_idx <- order_idx[order_idx %in% present]
  if (identical(strategy, "max_numeric")) {
    numeric_values <- suppressWarnings(as.numeric(values[present]))
    best_local <- present[which.max(numeric_values)]
    candidate_idx <- c(best_local, setdiff(candidate_idx, best_local))
  } else if (identical(strategy, "prefer_longest")) {
    lengths <- nchar(m0_stringify_value(values[candidate_idx]))
    candidate_idx <- candidate_idx[order(-lengths)]
  }

  chosen <- candidate_idx[1]
  value <- values[chosen]
  list(
    value = value,
    source_db = toupper(trimws(as.character(group_df$SOURCE_DB[chosen] %||% NA_character_))),
    source_tag = trimws(as.character(group_df$SOURCE_TAG[chosen] %||% NA_character_)),
    source_dbs = unique(toupper(trimws(as.character(group_df$SOURCE_DB[present])))),
    source_tags = unique(trimws(as.character(group_df$SOURCE_TAG[present]))),
    strategy = strategy,
    n_candidates = length(present),
    conflict = length(unique_values) > 1,
    alternatives = unique_values
  )
}

m0_group_merge_strategy <- function(field, values) {
  union_fields <- c(
    "AU", "DE", "ID", "C1", "AU_UN", "AU_CO", "AU1_CO", "FX", "FU", "CR", "EM",
    "M0_SOURCE_FILE", "M0_SOURCE_PATH", "M0_SOURCE_ROW"
  )
  max_fields <- c("TC", "NR")
  longest_fields <- c("AB", "TI", "RP")

  if (field %in% union_fields) return("union")
  if (field %in% max_fields) return("max_numeric")
  if (field %in% longest_fields) return("prefer_longest")
  if (is.numeric(values) || is.integer(values)) return("max_numeric")
  "prefer_priority"
}

m0_rank_group_rows <- function(group_df, config) {
  n <- nrow(group_df)
  source_db <- if ("SOURCE_DB" %in% names(group_df)) group_df$SOURCE_DB else rep(NA_character_, n)
  priorities <- m0_source_priority_rank(source_db, config)
  completeness <- apply(group_df, 1, function(row) sum(m0_has_value(row)))
  title_length <- if ("TI" %in% names(group_df)) nchar(m0_stringify_value(group_df$TI)) else rep(0, n)
  order(priorities, -completeness, -title_length, seq_len(n))
}

m0_source_priority_rank <- function(source_db, config) {
  priority <- toupper(as.character(config$source_priority %||% character()))
  db <- toupper(trimws(as.character(source_db)))
  rank <- match(db, priority)
  rank[is.na(rank)] <- length(priority) + 1L
  rank
}

m0_has_value <- function(x) {
  if (is.list(x)) return(lengths(x) > 0)
  if (is.numeric(x) || is.integer(x)) return(!is.na(x))
  if (is.logical(x)) return(!is.na(x))
  x <- as.character(x)
  !is.na(x) & nzchar(trimws(x))
}

m0_merge_union_values <- function(values) {
  parts <- unlist(lapply(values, function(value) {
    value <- as.character(value)
    if (is.na(value) || !nzchar(trimws(value))) {
      return(character(0))
    }
    tokens <- unlist(strsplit(value, ";", fixed = TRUE))
    tokens <- trimws(tokens)
    tokens[!is.na(tokens) & nzchar(tokens)]
  }), use.names = FALSE)

  parts <- unique(parts)
  if (length(parts) == 0) return(NA_character_)
  paste(parts, collapse = ";")
}

m0_stringify_value <- function(x) {
  if (length(x) > 1) {
    return(vapply(x, m0_stringify_value, character(1)))
  }
  if (is.null(x) || length(x) == 0 || is.na(x)) return(NA_character_)
  if (is.numeric(x) || is.integer(x)) return(as.character(x))
  trimws(as.character(x))
}

m0_finalize_provenance_columns <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0) return(df)

  needed <- c(
    "M0_SOURCE_DBS", "M0_SOURCE_TAGS", "M0_PROVENANCE_COUNT", "M0_DEDUP_METHOD",
    "M0_FIELD_PROVENANCE", "M0_FIELD_CONFLICTS", "M0_PRIMARY_SOURCE_DB",
    "M0_PRIMARY_SOURCE_TAG", "M0_CONFLICT_FIELDS", "M0_CONFLICT_COUNT",
    "M0_RESOLUTION_NOTES"
  )
  for (col in needed) {
    if (!col %in% names(df)) df[[col]] <- NA
  }

  for (i in seq_len(nrow(df))) {
    if (is.na(df$M0_SOURCE_DBS[i]) || !nzchar(trimws(as.character(df$M0_SOURCE_DBS[i])))) {
      df$M0_SOURCE_DBS[i] <- toupper(trimws(as.character(df$SOURCE_DB[i] %||% NA_character_)))
    }
    if (is.na(df$M0_SOURCE_TAGS[i]) || !nzchar(trimws(as.character(df$M0_SOURCE_TAGS[i])))) {
      df$M0_SOURCE_TAGS[i] <- trimws(as.character(df$SOURCE_TAG[i] %||% NA_character_))
    }
    if (is.na(df$M0_PROVENANCE_COUNT[i])) df$M0_PROVENANCE_COUNT[i] <- 1L
    if (is.na(df$M0_PRIMARY_SOURCE_DB[i]) || !nzchar(trimws(as.character(df$M0_PRIMARY_SOURCE_DB[i])))) {
      df$M0_PRIMARY_SOURCE_DB[i] <- toupper(trimws(as.character(df$SOURCE_DB[i] %||% NA_character_)))
    }
    if (is.na(df$M0_PRIMARY_SOURCE_TAG[i]) || !nzchar(trimws(as.character(df$M0_PRIMARY_SOURCE_TAG[i])))) {
      df$M0_PRIMARY_SOURCE_TAG[i] <- trimws(as.character(df$SOURCE_TAG[i] %||% NA_character_))
    }
    if (is.na(df$M0_CONFLICT_COUNT[i])) df$M0_CONFLICT_COUNT[i] <- 0L
    if (is.na(df$M0_CONFLICT_FIELDS[i])) df$M0_CONFLICT_FIELDS[i] <- ""
    if (is.na(df$M0_RESOLUTION_NOTES[i])) df$M0_RESOLUTION_NOTES[i] <- "Native record retained"

    if (is.na(df$M0_FIELD_PROVENANCE[i]) || !nzchar(trimws(as.character(df$M0_FIELD_PROVENANCE[i])))) {
      field_map <- list()
      for (field in names(df)) {
        if (field %in% needed) next
        if (!m0_has_value(df[[field]][i])) next
        field_map[[field]] <- list(
          source_db = df$M0_PRIMARY_SOURCE_DB[i],
          source_tag = df$M0_PRIMARY_SOURCE_TAG[i],
          strategy = "native",
          n_candidates = 1L
        )
      }
      df$M0_FIELD_PROVENANCE[i] <- jsonlite::toJSON(field_map, auto_unbox = TRUE, null = "null")
    }

    if (is.na(df$M0_FIELD_CONFLICTS[i]) || !nzchar(trimws(as.character(df$M0_FIELD_CONFLICTS[i])))) {
      df$M0_FIELD_CONFLICTS[i] <- jsonlite::toJSON(list(), auto_unbox = TRUE, null = "null")
    }
  }

  df
}

m0_build_field_provenance_table <- function(merged) {
  if (!is.data.frame(merged) || nrow(merged) == 0 || !"M0_FIELD_PROVENANCE" %in% names(merged)) {
    return(data.frame())
  }

  rows <- list()
  for (i in seq_len(nrow(merged))) {
    payload <- merged$M0_FIELD_PROVENANCE[i]
    if (is.na(payload) || !nzchar(trimws(as.character(payload)))) next
    parsed <- tryCatch(jsonlite::fromJSON(payload, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(parsed) || length(parsed) == 0) next
    for (field in names(parsed)) {
      info <- parsed[[field]]
      rows[[length(rows) + 1L]] <- data.frame(
        M0_DOC_ID = merged$M0_DOC_ID[i] %||% i,
        field = field,
        source_db = info$source_db %||% NA_character_,
        source_tag = info$source_tag %||% NA_character_,
        strategy = info$strategy %||% NA_character_,
        n_candidates = suppressWarnings(as.integer(info$n_candidates %||% NA_integer_)),
        stringsAsFactors = FALSE
      )
    }
  }

  dplyr::bind_rows(rows)
}

m0_build_field_conflict_table <- function(merged) {
  if (!is.data.frame(merged) || nrow(merged) == 0 || !"M0_FIELD_CONFLICTS" %in% names(merged)) {
    return(data.frame())
  }

  rows <- list()
  for (i in seq_len(nrow(merged))) {
    payload <- merged$M0_FIELD_CONFLICTS[i]
    if (is.na(payload) || !nzchar(trimws(as.character(payload)))) next
    parsed <- tryCatch(jsonlite::fromJSON(payload, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(parsed) || length(parsed) == 0) next
    for (field in names(parsed)) {
      info <- parsed[[field]]
      rows[[length(rows) + 1L]] <- data.frame(
        M0_DOC_ID = merged$M0_DOC_ID[i] %||% i,
        field = field,
        chosen_value = info$chosen_value %||% NA_character_,
        alternatives = paste(unlist(info$alternatives %||% character()), collapse = ";"),
        source_tags = paste(unlist(info$source_tags %||% character()), collapse = ";"),
        source_dbs = paste(unlist(info$source_dbs %||% character()), collapse = ";"),
        strategy = info$strategy %||% NA_character_,
        stringsAsFactors = FALSE
      )
    }
  }

  dplyr::bind_rows(rows)
}
