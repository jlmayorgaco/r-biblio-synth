# ============================================================================
# m0_load_sources.R - Load bibliographic sources into bibliometrix data frames
# ============================================================================

#' Load all sources into a named list of bibliometrix data frames
#'
#' @param sources Named list of source specifications.
#' @param config Configuration list.
#' @return Named list of data frames, one per source.
#' @export
m0_load_all_sources <- function(sources, config = biblio_config()) {
  config <- merge_biblio_config(config)
  results <- list()

  for (nm in names(sources)) {
    spec <- sources[[nm]]
    if (config$verbose) cli::cli_alert_info("Loading source: {nm} ({spec$db})")

    df <- tryCatch(
      m0_load_single_source(spec, config),
      error = function(e) {
        cli::cli_warn("Failed to load source '{nm}': {e$message}")
        NULL
      }
    )

    if (!is.null(df) && nrow(df) > 0) {
      # Tag source origin
      df$SOURCE_DB <- toupper(spec$db)
      df$SOURCE_TAG <- nm
      results[[nm]] <- df
      if (config$verbose) cli::cli_alert_success("  Loaded {nrow(df)} records from {nm}")
    } else {
      cli::cli_warn("  Source '{nm}' produced 0 records")
    }
  }

  results
}

#' Load a single bibliographic source
#'
#' @param spec A list with \code{file}, \code{db}, and optionally \code{format}.
#' @return A bibliometrix-compatible data frame.
#' @keywords internal
m0_load_single_source <- function(spec, config = biblio_config()) {
  config <- merge_biblio_config(config)
  db <- tolower(as.character(spec$db %||% ""))
  fmt <- tolower(as.character(spec$format %||% "bibtex"))
  schema <- m0_resolve_source_schema(spec)

  if (m0_is_api_source_spec(spec)) {
    return(m0_fetch_api_source(spec, config))
  }

  file_paths <- m0_resolve_source_files(spec)
  if (length(file_paths) == 0) {
    stop("Source file not found: ", spec$file %||% spec$file_pattern %||% "<unspecified>")
  }
  missing_files <- file_paths[!file.exists(file_paths)]
  if (length(missing_files) > 0) {
    stop("Source file not found: ", missing_files[[1]])
  }

  if (!is.na(schema) && schema %in% c("scopus_csv", "wos_csv", "generic_csv")) {
    return(m0_load_csv_source_collection(file_paths, schema, spec, config))
  }

  if (length(file_paths) > 1) {
    loaded <- lapply(file_paths, function(path) {
      m0_load_single_source(m0_single_file_source_spec(spec, path), config = config)
    })
    loaded <- Filter(function(x) is.data.frame(x) && nrow(x) > 0, loaded)
    if (length(loaded) == 0) {
      return(data.frame(stringsAsFactors = FALSE))
    }
    return(m0_standardize_columns(dplyr::bind_rows(loaded)))
  }

  file_path <- file_paths[[1]]

  switch(db,
    scopus = {
      m0_load_bibtex_safely(file_path, "scopus", fmt)
    },
    wos = {
      m0_load_bibtex_safely(file_path, "wos", fmt)
    },
    openalex = {
      if (fmt == "csv") {
        m0_load_openalex_csv(file_path)
      } else {
        m0_load_bibtex_safely(file_path, "openalex", fmt)
      }
    },
    crossref = {
      m0_fetch_api_source(c(spec, list(mode = "api")), config)
    },
    pubmed = {
      m0_fetch_api_source(c(spec, list(mode = "api")), config)
    },
    generic = {
      m0_load_generic(file_path, fmt)
    },
    stop("Unknown db source: ", db)
  )
}

#' Resolve schema for a source specification
#' @keywords internal
m0_resolve_source_schema <- function(spec) {
  schema <- tolower(as.character(spec$schema %||% ""))
  if (nzchar(schema)) {
    return(schema)
  }

  db <- tolower(as.character(spec$db %||% ""))
  fmt <- tolower(as.character(spec$format %||% ""))
  if (fmt == "csv" && db == "scopus") return("scopus_csv")
  if (fmt == "csv" && db == "wos") return("wos_csv")
  if (fmt == "csv") return("generic_csv")

  NA_character_
}

#' Resolve source files from file, files, or file_pattern fields
#' @keywords internal
m0_resolve_source_files <- function(spec) {
  file_values <- character()

  if (!is.null(spec$file) && nzchar(as.character(spec$file)[1])) {
    file_values <- c(file_values, as.character(spec$file)[1])
  }

  if (!is.null(spec$files)) {
    file_values <- c(file_values, as.character(unlist(spec$files, use.names = FALSE)))
  }

  if (!is.null(spec$file_pattern) && nzchar(as.character(spec$file_pattern)[1])) {
    pattern_raw <- as.character(spec$file_pattern)[1]
    pattern <- gsub("\\\\", "/", pattern_raw)
    matched <- Sys.glob(pattern)
    if (length(matched) == 0 && grepl("[*?]", pattern_raw)) {
      pattern_dir <- normalizePath(dirname(pattern_raw), winslash = "/", mustWork = FALSE)
      if (dir.exists(pattern_dir)) {
        candidates <- list.files(pattern_dir, full.names = TRUE)
        matched <- candidates[grepl(utils::glob2rx(basename(pattern_raw)), basename(candidates))]
      }
    }
    file_values <- c(file_values, matched)
  }

  file_values <- trimws(file_values)
  file_values <- file_values[nzchar(file_values)]
  wildcard_mask <- grepl("[*?\\[]", file_values) & !file.exists(file_values)
  file_values <- file_values[!wildcard_mask]
  unique(file_values)
}

#' Convert any source specification into a single-file specification
#' @keywords internal
m0_single_file_source_spec <- function(spec, file_path) {
  spec$file <- file_path
  spec$files <- NULL
  spec$file_pattern <- NULL
  spec
}

#' Load a multi-file or chunked CSV source collection
#' @keywords internal
m0_load_csv_source_collection <- function(file_paths, schema, spec, config) {
  delimiter <- as.character(spec$delimiter %||% config$m0_csv_delimiter %||% ",")[1]
  encoding <- as.character(spec$encoding %||% config$m0_csv_encoding %||% "UTF-8")[1]
  chunk_size_rows <- suppressWarnings(as.integer(spec$chunk_size_rows %||% config$m0_chunk_size_rows %||% 50000L))
  if (length(chunk_size_rows) == 0 || is.na(chunk_size_rows) || chunk_size_rows <= 0) {
    chunk_size_rows <- NULL
  }

  mapped_list <- lapply(file_paths, function(file_path) {
    raw <- m0_read_csv_maybe_chunked(
      file_path = file_path,
      delimiter = delimiter,
      encoding = encoding,
      chunk_size_rows = chunk_size_rows
    )

    if (!is.data.frame(raw) || nrow(raw) == 0) {
      return(data.frame(stringsAsFactors = FALSE))
    }

    mapped <- switch(
      schema,
      scopus_csv = m0_map_scopus_csv(raw),
      wos_csv = m0_map_wos_csv(raw),
      generic_csv = m0_harmonize_columns(raw),
      raw
    )
    mapped$M0_SOURCE_FILE <- basename(file_path)
    mapped$M0_SOURCE_PATH <- normalizePath(file_path, winslash = "/", mustWork = FALSE)
    mapped$M0_SOURCE_ROW <- seq_len(nrow(mapped))
    m0_standardize_columns(m0_harmonize_columns(mapped))
  })

  mapped_list <- Filter(function(x) is.data.frame(x) && nrow(x) > 0, mapped_list)
  if (length(mapped_list) == 0) {
    return(data.frame(stringsAsFactors = FALSE))
  }

  m0_standardize_columns(dplyr::bind_rows(mapped_list))
}

#' Read a CSV file, optionally in chunks, while removing embedded header rows
#' @keywords internal
m0_read_csv_maybe_chunked <- function(file_path,
                                      delimiter = ",",
                                      encoding = "UTF-8",
                                      chunk_size_rows = NULL) {
  if (is.null(chunk_size_rows) || is.na(chunk_size_rows)) {
    raw <- utils::read.csv(
      file_path,
      stringsAsFactors = FALSE,
      encoding = encoding,
      sep = delimiter,
      quote = "\"",
      fill = TRUE,
      check.names = FALSE
    )
    return(m0_drop_embedded_header_rows(raw))
  }

  con <- file(file_path, open = "rt", encoding = encoding)
  on.exit(close(con), add = TRUE)

  first_chunk <- utils::read.csv(
    con,
    stringsAsFactors = FALSE,
    sep = delimiter,
    quote = "\"",
    fill = TRUE,
    check.names = FALSE,
    nrows = chunk_size_rows
  )

  if (!is.data.frame(first_chunk) || nrow(first_chunk) == 0) {
    return(first_chunk)
  }

  col_names <- names(first_chunk)
  first_chunk[] <- lapply(first_chunk, as.character)
  chunks <- list(m0_drop_embedded_header_rows(first_chunk))

  repeat {
    chunk <- tryCatch(
      utils::read.csv(
        con,
        header = FALSE,
        col.names = col_names,
        stringsAsFactors = FALSE,
        sep = delimiter,
        quote = "\"",
        fill = TRUE,
        check.names = FALSE,
        nrows = chunk_size_rows
      ),
      error = function(e) data.frame(stringsAsFactors = FALSE)
    )

    if (!is.data.frame(chunk) || nrow(chunk) == 0) {
      break
    }

    chunk[] <- lapply(chunk, as.character)
    chunks[[length(chunks) + 1]] <- m0_drop_embedded_header_rows(chunk)
  }

  dplyr::bind_rows(chunks)
}

#' Remove repeated header rows embedded inside exported CSV files
#' @keywords internal
m0_drop_embedded_header_rows <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(df)
  }

  header_values <- m0_csv_normalize_header(names(df))
  repeated_header <- apply(df, 1, function(row) {
    row_values <- m0_csv_normalize_header(as.character(row))
    comparable <- nzchar(row_values) & nzchar(header_values)
    comparable[is.na(comparable)] <- FALSE
    if (!any(comparable, na.rm = TRUE)) {
      return(FALSE)
    }
    matches <- row_values[comparable] == header_values[comparable]
    matches[is.na(matches)] <- FALSE
    mean(matches) >= 0.8
  })

  df[!repeated_header, , drop = FALSE]
}

#' Normalize CSV headers for tolerant schema matching
#' @keywords internal
m0_csv_normalize_header <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- enc2utf8(x)
  x <- gsub("^\ufeff", "", x)
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x
}

#' Safely retrieve the first available column from a raw export
#' @keywords internal
m0_csv_pick_column <- function(raw, candidates, default = NA_character_) {
  for (candidate in candidates) {
    if (candidate %in% names(raw)) {
      return(raw[[candidate]])
    }
  }

  normalized_names <- m0_csv_normalize_header(names(raw))
  normalized_candidates <- m0_csv_normalize_header(candidates)
  idx <- match(normalized_candidates, normalized_names)
  idx <- idx[!is.na(idx)]
  if (length(idx) > 0) {
    return(raw[[idx[[1]]]])
  }

  rep(default, nrow(raw))
}

#' Compose page strings from start/end or fallback page columns
#' @keywords internal
m0_csv_compose_pages <- function(raw, start_candidates, end_candidates, fallback_candidates = character()) {
  start_page <- m0_csv_pick_column(raw, start_candidates, default = "")
  end_page <- m0_csv_pick_column(raw, end_candidates, default = "")
  fallback <- m0_csv_pick_column(raw, fallback_candidates, default = "")

  pages <- ifelse(
    nzchar(trimws(as.character(start_page))) & nzchar(trimws(as.character(end_page))),
    paste0(start_page, "-", end_page),
    as.character(fallback)
  )
  pages[!nzchar(trimws(pages))] <- NA_character_
  pages
}

#' Derive author countries from affiliation strings
#' @keywords internal
m0_csv_derive_author_countries <- function(affiliations) {
  extracted <- m0_extract_countries(affiliations)
  vapply(extracted, function(x) {
    vals <- unique(m0_normalize_countries(x))
    vals <- vals[!is.na(vals) & nzchar(vals)]
    if (length(vals) == 0) {
      return(NA_character_)
    }
    paste(vals, collapse = ";")
  }, character(1))
}

#' Map Scopus CSV exports to the canonical bibliometrix-compatible schema
#' @keywords internal
m0_map_scopus_csv <- function(raw) {
  df <- raw[, FALSE, drop = FALSE]
  df$AU <- m0_csv_pick_column(raw, c("Authors", "Author(s)"))
  df$TI <- m0_csv_pick_column(raw, c("Title"))
  df$PY <- m0_csv_pick_column(raw, c("Year"))
  df$SO <- m0_csv_pick_column(raw, c("Source title", "Source Title"))
  df$AB <- m0_csv_pick_column(raw, c("Abstract"))
  df$DE <- m0_csv_pick_column(raw, c("Author Keywords"))
  df$ID <- m0_csv_pick_column(raw, c("Index Keywords"))
  df$C1 <- m0_csv_pick_column(raw, c("Affiliations", "Authors with affiliations"))
  df$RP <- m0_csv_pick_column(raw, c("Correspondence Address"))
  df$CR <- m0_csv_pick_column(raw, c("References", "Cited References"))
  df$TC <- m0_csv_pick_column(raw, c("Cited by", "Citations"))
  df$DI <- m0_csv_pick_column(raw, c("DOI"))
  df$DT <- m0_csv_pick_column(raw, c("Document Type"))
  df$LA <- m0_csv_pick_column(raw, c("Language of Original Document", "Language"))
  df$PU <- m0_csv_pick_column(raw, c("Publisher"))
  df$VL <- m0_csv_pick_column(raw, c("Volume"))
  df$IS <- m0_csv_pick_column(raw, c("Issue"))
  df$PG <- m0_csv_compose_pages(
    raw,
    start_candidates = c("Page start", "Start Page"),
    end_candidates = c("Page end", "End Page"),
    fallback_candidates = c("Page count", "Pages", "Art. No.", "Article Number")
  )
  df$SN <- m0_csv_pick_column(raw, c("ISSN", "ISSN/eISSN"))
  df$URL <- m0_csv_pick_column(raw, c("Link"))
  df$FU <- m0_csv_pick_column(raw, c("Funding Details", "Funding Texts"))
  df$AU_UN <- df$C1
  df$AU_CO <- m0_csv_derive_author_countries(df$C1)
  df
}

#' Map Web of Science CSV exports to the canonical bibliometrix-compatible schema
#' @keywords internal
m0_map_wos_csv <- function(raw) {
  df <- raw[, FALSE, drop = FALSE]
  df$AU <- m0_csv_pick_column(raw, c("AU", "Authors"))
  df$TI <- m0_csv_pick_column(raw, c("TI", "Article Title", "Title"))
  df$PY <- m0_csv_pick_column(raw, c("PY", "Publication Year", "Year Published"))
  df$SO <- m0_csv_pick_column(raw, c("SO", "Source Title", "Publication Name"))
  df$AB <- m0_csv_pick_column(raw, c("AB", "Abstract"))
  df$DE <- m0_csv_pick_column(raw, c("DE", "Author Keywords"))
  df$ID <- m0_csv_pick_column(raw, c("ID", "Keywords Plus"))
  df$C1 <- m0_csv_pick_column(raw, c("C1", "Addresses", "Affiliations"))
  df$RP <- m0_csv_pick_column(raw, c("RP", "Reprint Addresses", "Reprint Address"))
  df$CR <- m0_csv_pick_column(raw, c("CR", "Cited References"))
  df$TC <- m0_csv_pick_column(raw, c("TC", "Times Cited", "Times Cited, All Databases", "Times Cited, WoS Core"))
  df$DI <- m0_csv_pick_column(raw, c("DI", "DOI"))
  df$DT <- m0_csv_pick_column(raw, c("DT", "Document Type"))
  df$LA <- m0_csv_pick_column(raw, c("LA", "Language"))
  df$PU <- m0_csv_pick_column(raw, c("PU", "Publisher"))
  df$VL <- m0_csv_pick_column(raw, c("VL", "Volume"))
  df$IS <- m0_csv_pick_column(raw, c("IS", "Issue"))
  df$PG <- m0_csv_compose_pages(
    raw,
    start_candidates = c("BP", "Beginning Page"),
    end_candidates = c("EP", "Ending Page"),
    fallback_candidates = c("PG", "Pages")
  )
  df$SN <- m0_csv_pick_column(raw, c("SN", "ISSN"))
  df$URL <- m0_csv_pick_column(raw, c("UT", "Accession Number"))
  df$AU_UN <- m0_csv_pick_column(raw, c("C1", "Addresses", "Affiliations"))
  df$AU_CO <- m0_csv_derive_author_countries(df$C1)
  df
}

#' Safely load BibTeX with fallback parsing
#'
#' Tries bibliometrix::convert2df first, then falls back to RefManageR
#' and finally manual parsing if needed.
#'
#' @param file_path Path to BibTeX file.
#' @param dbsource Database source for bibliometrix.
#' @param format Format (bibtex, plaintext, csv).
#' @return A data frame with standardized columns.
#' @keywords internal
m0_load_bibtex_safely <- function(file_path, dbsource, format = "bibtex") {
  # Try bibliometrix first
  result <- tryCatch({
    output <- utils::capture.output(
      result <- suppressMessages(suppressWarnings(
        bibliometrix::convert2df(file = file_path, dbsource = dbsource, format = format)
      ))
    )
    invisible(output)
    result
  }, error = function(e) {
    msg <- conditionMessage(e)
    if (grepl("undefined columns|columnas no definidas|no definidas seleccionadas", msg, ignore.case = TRUE)) {
      NULL
    } else {
      stop(e)
    }
  })

  if (!is.null(result) && is.data.frame(result) && nrow(result) > 0) {
    return(m0_standardize_columns(result))
  }

  # Fallback 1: Try RefManageR
  result <- tryCatch({
    m0_load_via_refmanageR(file_path)
  }, error = function(e) {
    NULL
  })

  if (!is.null(result) && is.data.frame(result) && nrow(result) > 0) {
    return(m0_standardize_columns(result))
  }

  # Fallback 2: Manual BibTeX parsing
  result <- tryCatch({
    m0_load_bibtex_manual(file_path)
  }, error = function(e) {
    stop("Manual BibTeX parsing failed: ", e$message,
         "\n  File: ", file_path,
         "\n  Please ensure the file is valid BibTeX format.")
  })

  m0_standardize_columns(result)
}

#' Load BibTeX via RefManageR
#'
#' @param file_path Path to BibTeX file.
#' @return A bibliometrix-style data frame.
#' @keywords internal
m0_load_via_refmanageR <- function(file_path) {
  if (!requireNamespace("RefManageR", quietly = TRUE)) {
    stop("Package 'RefManageR' is required for fallback parsing. ",
         "Install with: install.packages('RefManageR')")
  }

  bib <- suppressWarnings(
    RefManageR::ReadBib(file_path, check = "warn")
  )
  if (is.null(bib) || length(bib) == 0) {
    stop("RefManageR parsed 0 entries from file")
  }

  df <- RefManageR::as.data.frame(bib)
  names(df) <- toupper(names(df))

  # Map RefManageR columns to bibliometrix columns
  col_map <- c(
    "AUTHOR"      = "AU",
    "TITLE"       = "TI",
    "JOURNAL"     = "SO",
    "YEAR"        = "PY",
    "PUBLISHER"   = "PU",
    "VOLUME"      = "VL",
    "NUMBER"      = "IS",
    "PAGES"       = "PG",
    "DOI"         = "DI",
    "ISSN"        = "ISSN",
    "ABSTRACT"    = "AB",
    "KEYWORDS"    = "DE",
    "URL"         = "URL"
  )

  for (from in names(col_map)) {
    to <- col_map[[from]]
    if (from %in% names(df) && !to %in% names(df)) {
      df[[to]] <- df[[from]]
    }
  }

  # RefManageR stores authors as a list; convert to semicolon-separated
  if ("AU" %in% names(df) && is.list(df$AU)) {
    df$AU <- sapply(df$AU, function(x) {
      if (is.list(x)) {
        paste(RefManageR::FormatAuthors(x, braces = FALSE), collapse = ";")
      } else {
        as.character(x)
      }
    })
  }

  df
}

#' Manual BibTeX parser as last resort
#'
#' @param file_path Path to BibTeX file.
#' @return A data frame with basic columns.
#' @keywords internal
m0_load_bibtex_manual <- function(file_path) {
  raw_lines <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
  raw_lines <- raw_lines[nzchar(trimws(raw_lines))]

  entries <- list()
  current_entry <- NULL
  current_key <- NULL

  for (line in raw_lines) {
    line <- enc2utf8(line)

    if (grepl("^@\\w+\\{", line)) {
      if (!is.null(current_entry)) {
        entries[[current_key]] <- current_entry
      }
      current_key <- gsub("^@\\w+\\{([^,]+).*", "\\1", line)
      current_entry <- list()
      next
    }

    if (!is.null(current_entry) && grepl("\\s*=\\s*\\{", line)) {
      field <- sub("\\s*=.*$", "", line)
      field <- trimws(gsub("^\\s*", "", field))
      field <- toupper(field)

      rest <- sub("^[^=]+=\\s*", "", line)
      rest <- trimws(rest)
      rest <- sub(",$", "", rest)
      rest <- sub("^\\{", "", rest)
      rest <- sub("\\}$", "", rest)
      rest <- trimws(rest)

      if (field == "AUTHOR") {
        authors <- strsplit(rest, "\\s+and\\s+")[[1]]
        current_entry$AU <- paste(authors, collapse = ";")
      } else if (field == "YEAR") {
        current_entry$PY <- as.integer(gsub("\\D", "", rest))
      } else if (field == "TITLE") {
        current_entry$TI <- rest
      } else if (field == "JOURNAL" || field == "BOOKTITLE") {
        current_entry$SO <- rest
      } else if (field == "VOLUME") {
        current_entry$VL <- rest
      } else if (field == "NUMBER" || field == "ISSUE") {
        current_entry$IS <- rest
      } else if (field == "PAGES") {
        current_entry$PG <- rest
      } else if (field == "DOI") {
        current_entry$DI <- rest
      } else if (field == "AUTHOR_KEYWORDS") {
        kws <- strsplit(rest, "\\s*;\\s*")[[1]]
        current_entry$DE <- paste(kws, collapse = ";")
      } else if (field == "KEYWORDS") {
        kws <- strsplit(rest, "\\s*;\\s*")[[1]]
        current_entry$ID <- paste(kws, collapse = ";")
      } else if (field == "ABSTRACT") {
        current_entry$AB <- rest
      } else if (field == "PUBLISHER") {
        current_entry$PU <- rest
      } else if (field == "URL") {
        current_entry$UR <- rest
      } else if (field == "AFFILIATIONS") {
        current_entry$C1 <- rest
        countries <- unique(stats::na.omit(m0_extract_countries(rest)[[1]]))
        current_entry$AU_CO <- if (length(countries) > 0) paste(countries, collapse = ";") else NA_character_
      } else if (field == "CORRESPONDENCE_ADDRESS") {
        current_entry$RP <- rest
      } else if (field == "ISSN") {
        current_entry$ISSN <- rest
      } else if (field == "ISBN") {
        current_entry$ISBN <- rest
      } else if (field == "LANGUAGE") {
        current_entry$LA <- rest
      } else if (field == "TYPE" || field == "DT") {
        current_entry$DT <- rest
      } else if (field == "NOTE") {
        cited_by <- regmatches(rest, regexpr("Cited by:\\s*[0-9]+", rest, ignore.case = TRUE))
        if (length(cited_by) > 0 && !is.na(cited_by)) {
          current_entry$TC <- suppressWarnings(as.integer(gsub("\\D", "", cited_by)))
        }
      }
    }
  }

  if (!is.null(current_entry)) {
    entries[[current_key]] <- current_entry
  }

  if (length(entries) == 0) {
    stop("Manual parser found 0 entries")
  }

  all_cols <- unique(unlist(lapply(entries, names)))
  rows <- lapply(entries, function(x) {
    row <- as.list(rep(NA_character_, length(all_cols)))
    names(row) <- all_cols
    for (col in names(x)) {
      row[[col]] <- x[[col]]
    }
    as.data.frame(row, stringsAsFactors = FALSE)
  })
  df <- dplyr::bind_rows(rows)

  if ("PY" %in% names(df)) {
    df$PY <- suppressWarnings(as.integer(df$PY))
  }
  if ("TC" %in% names(df) && any(!is.na(df$TC))) {
    df$TC <- suppressWarnings(as.integer(df$TC))
  } else {
    df$TC <- 0L
  }

  df$SR <- paste(df$TI %||% "", df$PY %||% "", sep = "-")

  df
}

#' Standardize column names across different parsers
#'
#' Ensures common columns exist and have correct names.
#'
#' @param df A data frame from any parser.
#' @return A data frame with standardized columns.
#' @keywords internal
m0_standardize_columns <- function(df) {
  required <- c("TI", "PY", "AU", "SO", "DT", "TC", "DI")
  optional <- c("AB", "DE", "VL", "IS", "PG", "PU", "URL", "ISSN", "ISBN",
                "LA", "AU_CO", "C1", "RP", "EM", "EY", "TX", "ID", "NC",
                "WOS", "SC", "DOI", "SRC", "ORIG", "DB")

  for (col in required) {
    if (!col %in% names(df)) {
      df[[col]] <- NA_character_
    }
  }

  # Normalize year
  if ("PY" %in% names(df)) {
    df$PY <- suppressWarnings(as.integer(df$PY))
  }

  # Normalize citations
  if ("TC" %in% names(df)) {
    df$TC <- suppressWarnings(as.integer(df$TC))
    df$TC[is.na(df$TC)] <- 0L
  }

  # Create short reference if missing
  if (!"SR" %in% names(df) && "TI" %in% names(df)) {
    df$SR <- paste(substr(df$TI %||% "", 1, 50), df$PY %||% "", sep = "-")
  }

  # Remove internal columns if present
  drop_cols <- c("X", "BKS", "BI", "UT")
  for (col in drop_cols) {
    if (col %in% names(df)) df[[col]] <- NULL
  }

  df
}

#' Load OpenAlex CSV export and map to bibliometrix columns
#'
#' @param file_path Path to OpenAlex CSV.
#' @return A data frame with bibliometrix-compatible columns.
#' @keywords internal
m0_load_openalex_csv <- function(file_path) {
  raw <- utils::read.csv(file_path, stringsAsFactors = FALSE, encoding = "UTF-8",
                         fill = TRUE, quote = '"')

  # Initialize result dataframe
  df <- data.frame(stringsAsFactors = FALSE)

  # Map basic columns
  if ("title" %in% names(raw)) df$TI <- raw$title
  if ("publication_year" %in% names(raw)) df$PY <- as.integer(raw$publication_year)
  if ("cited_by_count" %in% names(raw)) df$TC <- as.integer(raw$cited_by_count)
  if ("doi" %in% names(raw)) df$DI <- raw$doi
  if ("type" %in% names(raw)) df$DT <- raw$type
  if ("language" %in% names(raw)) df$LA <- raw$language

  # Author extraction from authorships column (JSON-like)
  # OpenAlex exports authorships as JSON array of objects
  if ("authorships" %in% names(raw)) {
    df$AU <- sapply(raw$authorships, m0_extract_openalex_authors)
    df$AU_CO <- sapply(raw$authorships, m0_extract_openalex_countries)
  }

  # Source/Journal extraction from primary_location
  if ("primary_location" %in% names(raw)) {
    df$SO <- sapply(raw$primary_location, m0_extract_openalex_source)
  }

  # Keywords from concepts (OpenAlex uses "concepts" not "keywords")
  if ("concepts" %in% names(raw)) {
    df$DE <- sapply(raw$concepts, m0_extract_openalex_concepts)
  }

  # Abstract (if available, stored as inverted index in OpenAlex)
  if ("abstract_inverted_index" %in% names(raw)) {
    df$AB <- sapply(raw$abstract_inverted_index, m0_reconstruct_abstract)
  }

  # ISBN if available
  if ("isbn" %in% names(raw)) df$ISBN <- raw$isbn

  # ISSN if available
  if ("issn" %in% names(raw)) df$ISSN <- raw$issn

  # Publisher
  if ("publisher" %in% names(raw)) df$PU <- raw$publisher

  # Volume, Issue, Pages
  if ("biblio" %in% names(raw)) {
    df$VL <- sapply(raw$biblio, function(x) {
      if (is.na(x) || x == "") return(NA)
      jsonlite::fromJSON(x)$volume
    })
    df$IS <- sapply(raw$biblio, function(x) {
      if (is.na(x) || x == "") return(NA)
      jsonlite::fromJSON(x)$issue
    })
    df$PG <- sapply(raw$biblio, function(x) {
      if (is.na(x) || x == "") return(NA)
      jsonlite::fromJSON(x)$first_page
    })
  }

  # Ensure all columns exist
  for (col in c("TI", "PY", "TC", "DI", "DT", "AU", "SO", "DE", "AB")) {
    if (!col %in% names(df)) df[[col]] <- NA_character_
  }

  # Convert types
  df$PY <- suppressWarnings(as.integer(df$PY))
  df$TC <- suppressWarnings(as.integer(df$TC))

  df
}

#' Extract authors from OpenAlex authorships JSON
#' @keywords internal
m0_extract_openalex_authors <- function(x) {
  if (is.na(x) || x == "" || !nzchar(x)) return(NA_character_)
  tryCatch({
    authors <- jsonlite::fromJSON(x)
    if (is.data.frame(authors) && "display_name" %in% names(authors)) {
      paste(authors$display_name, collapse = ";")
    } else if (is.list(authors) && !is.null(authors$display_name)) {
      paste(authors$display_name, collapse = ";")
    } else {
      NA_character_
    }
  }, error = function(e) NA_character_)
}

#' Extract author countries from OpenAlex authorships
#' @keywords internal
m0_extract_openalex_countries <- function(x) {
  if (is.na(x) || x == "" || !nzchar(x)) return(NA_character_)
  tryCatch({
    authors <- jsonlite::fromJSON(x)
    if (is.data.frame(authors) && "countries" %in% names(authors)) {
      countries <- unique(unlist(authors$countries))
      paste(countries, collapse = ";")
    } else {
      NA_character_
    }
  }, error = function(e) NA_character_)
}

#' Extract source/journal from OpenAlex primary_location
#' @keywords internal
m0_extract_openalex_source <- function(x) {
  if (is.na(x) || x == "" || !nzchar(x)) return(NA_character_)
  tryCatch({
    loc <- jsonlite::fromJSON(x)
    if (is.list(loc) && "source" %in% names(loc)) {
      source <- loc$source
      if (is.list(source) && "display_name" %in% names(source)) {
        return(source$display_name)
      }
    }
    NA_character_
  }, error = function(e) NA_character_)
}

#' Extract concepts as keywords from OpenAlex
#' @keywords internal
m0_extract_openalex_concepts <- function(x) {
  if (is.na(x) || x == "" || !nzchar(x)) return(NA_character_)
  tryCatch({
    concepts <- jsonlite::fromJSON(x)
    if (is.data.frame(concepts) && "display_name" %in% names(concepts)) {
      # Filter to top concepts (score > 0.3 typically)
      top_concepts <- concepts$display_name[concepts$score > 0.3]
      paste(head(top_concepts, 20), collapse = ";")
    } else {
      NA_character_
    }
  }, error = function(e) NA_character_)
}

#' Reconstruct abstract from inverted index
#' @keywords internal
m0_reconstruct_abstract <- function(x) {
  if (is.na(x) || x == "" || !nzchar(x)) return(NA_character_)
  tryCatch({
    idx <- jsonlite::fromJSON(x)
    if (is.list(idx) && length(idx) > 0) {
      words <- unlist(idx)
      # words are named with positions, sort by position
      pos <- as.integer(names(words))
      paste(words[order(pos)], collapse = " ")
    } else {
      NA_character_
    }
  }, error = function(e) NA_character_)
}

#' Load a generic CSV/Excel file
#'
#' @param file_path Path to the file.
#' @param fmt Format ("csv" or "xlsx").
#' @return A data frame.
#' @keywords internal
m0_load_generic <- function(file_path, fmt) {
  switch(fmt,
    csv = m0_standardize_columns(utils::read.csv(file_path, stringsAsFactors = FALSE, encoding = "UTF-8")),
    xlsx = {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        cli::cli_abort("Package 'readxl' required for xlsx import")
      }
      m0_standardize_columns(as.data.frame(readxl::read_excel(file_path), stringsAsFactors = FALSE))
    },
    stop("Unsupported generic format: ", fmt)
  )
}
