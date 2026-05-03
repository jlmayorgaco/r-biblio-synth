# ============================================================================
# m0_validate.R - Validation for M0 Data Orchestrator
# ============================================================================

#' Validate source specifications for M0
#'
#' @param sources A named list of source specifications.
#' @return A list with \code{ok}, \code{error}, \code{details}.
#' @export
m0_validate_sources <- function(sources) {
  if (!is.list(sources) || length(sources) == 0) {
    return(list(ok = FALSE, error = "sources must be a non-empty named list"))
  }

  if (is.null(names(sources)) || any(names(sources) == "")) {
    return(list(ok = FALSE, error = "All source entries must be named"))
  }

  allowed_dbs <- c("scopus", "wos", "openalex", "crossref", "pubmed", "generic")
  allowed_fmt <- c("bibtex", "plaintext", "csv", "xlsx", "json")
  allowed_schemas <- c("scopus_csv", "wos_csv", "generic_csv")

  details <- list()
  for (nm in names(sources)) {
    spec <- sources[[nm]]
    is_api <- m0_is_api_source_spec(spec)
    if (!is.list(spec)) {
      return(list(ok = FALSE, error = paste0("Source '", nm, "' must be a list")))
    }
    if (is.null(spec$db) || !(spec$db %in% allowed_dbs)) {
      return(list(ok = FALSE, error = paste0("Source '", nm, "': db must be one of ",
                                              paste(allowed_dbs, collapse = ", "))))
    }
    schema <- m0_resolve_source_schema(spec)
    source_files <- if (!is_api) m0_resolve_source_files(spec) else character()
    if (!is_api && length(source_files) == 0) {
      return(list(ok = FALSE, error = paste0("Source '", nm, "': file not found: ", spec$file %||% spec$file_pattern %||% "<unspecified>")))
    }
    if (!is_api && any(!file.exists(source_files))) {
      missing_files <- source_files[!file.exists(source_files)]
      return(list(ok = FALSE, error = paste0("Source '", nm, "': file not found: ", paste(missing_files, collapse = ", "))))
    }
    if (is_api && is.null(spec$query) && is.null(spec$doi) && is.null(spec$ids) && is.null(spec$filter) && spec$db != "crossref") {
      return(list(ok = FALSE, error = paste0("Source '", nm, "': API sources require query, doi, or ids.")))
    }
    if (!is.null(spec$format) && !(spec$format %in% allowed_fmt)) {
      return(list(ok = FALSE, error = paste0("Source '", nm, "': format must be one of ",
                                              paste(allowed_fmt, collapse = ", "))))
    }
    if (!is.null(spec$schema) && !(tolower(as.character(spec$schema)[1]) %in% allowed_schemas)) {
      return(list(ok = FALSE, error = paste0("Source '", nm, "': schema must be one of ",
                                             paste(allowed_schemas, collapse = ", "))))
    }
    if (!is.null(spec$chunk_size_rows)) {
      chunk_size <- suppressWarnings(as.integer(spec$chunk_size_rows))
      if (is.na(chunk_size) || chunk_size <= 0) {
        return(list(ok = FALSE, error = paste0("Source '", nm, "': chunk_size_rows must be a positive integer")))
      }
    }
    if (!is.null(spec$delimiter) && !nzchar(as.character(spec$delimiter)[1])) {
      return(list(ok = FALSE, error = paste0("Source '", nm, "': delimiter must be a non-empty string")))
    }
    if (!is.null(spec$encoding) && !nzchar(as.character(spec$encoding)[1])) {
      return(list(ok = FALSE, error = paste0("Source '", nm, "': encoding must be a non-empty string")))
    }
    details[[nm]] <- list(
      db = spec$db,
      file = spec$file %||% NA_character_,
      files = if (length(source_files) > 0) source_files else spec$files %||% character(),
      n_files = length(source_files),
      file_pattern = spec$file_pattern %||% NA_character_,
      format = spec$format %||% if (!is.na(schema) && grepl("_csv$", schema)) "csv" else "bibtex",
      schema = schema,
      chunk_size_rows = spec$chunk_size_rows %||% NA_integer_,
      mode = if (is_api) "api" else "file"
    )
  }

  list(ok = TRUE, error = NULL, details = details)
}

#' Validate merged dataset has minimum required columns
#'
#' @param df A merged data frame.
#' @return A list with \code{ok}, \code{missing}, \code{n_rows}.
#' @export
m0_validate_merged <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(list(ok = FALSE, missing = NULL, n_rows = 0L,
                error = "Merged data is empty"))
  }

  # Minimal columns that bibliometrix modules expect
  minimal <- c("AU", "TI", "PY", "SO")
  present <- minimal %in% names(df)
  missing <- minimal[!present]

  list(ok = length(missing) == 0, missing = missing,
       n_rows = nrow(df), n_cols = ncol(df))
}

#' Validate loaded sources have content
#'
#' @param raw_list Named list of loaded data frames from sources.
#' @return A list with \code{ok}, \code{empty_sources}, \code{total_records}.
#' @export
m0_validate_loaded <- function(raw_list) {
  if (!is.list(raw_list) || length(raw_list) == 0) {
    return(list(ok = FALSE, empty_sources = names(raw_list), total_records = 0L,
                error = "No loaded sources"))
  }
  
  empty_sources <- character()
  total_records <- 0L
  
  for (nm in names(raw_list)) {
    df <- raw_list[[nm]]
    n <- if (is.data.frame(df)) nrow(df) else 0L
    total_records <- total_records + n
    if (n == 0) {
      empty_sources <- c(empty_sources, nm)
    }
  }
  
  if (length(empty_sources) == length(raw_list)) {
    return(list(ok = FALSE, empty_sources = empty_sources, total_records = 0L,
                error = "All sources produced empty results"))
  }
  
  if (length(empty_sources) > 0) {
    # Warning but not error - partial data is acceptable
    return(list(ok = TRUE, empty_sources = empty_sources, total_records = total_records,
                warning = paste0("Sources produced empty results: ", 
                                 paste(empty_sources, collapse = ", "))))
  }
  
  list(ok = TRUE, empty_sources = character(), total_records = total_records)
}

#' Validate column data types
#'
#' @param df A data frame.
#' @return A list with \code{ok}, \code{issues}.
#' @export
m0_validate_column_types <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(list(ok = FALSE, issues = list(error = "Empty data frame")))
  }
  
  issues <- list()
  
  # PY should be numeric/integer
  if ("PY" %in% names(df)) {
    py <- df$PY
    if (!is.numeric(py) && !is.integer(py)) {
      py_num <- suppressWarnings(as.numeric(py))
      if (any(is.na(py_num) & !is.na(py))) {
        issues$PY <- "Publication year (PY) contains non-numeric values"
      }
    }
    # Check for unreasonable years
    valid_years <- py[!is.na(py) & py > 0]
    if (length(valid_years) > 0) {
      current_year <- as.numeric(format(Sys.Date(), "%Y"))
      if (any(valid_years < 1500) || any(valid_years > current_year + 2)) {
        issues$PY_range <- paste0("Publication year outside reasonable range (1500-", current_year + 2, ")")
      }
    }
  }
  
  # TC should be numeric
  if ("TC" %in% names(df)) {
    tc <- df$TC
    if (!is.numeric(tc) && !is.integer(tc)) {
      issues$TC <- "Citation count (TC) is not numeric"
    }
    if (any(tc < 0, na.rm = TRUE)) {
      issues$TC_negative <- "Citation count (TC) contains negative values"
    }
  }
  
  list(ok = length(issues) == 0, issues = issues)
}
