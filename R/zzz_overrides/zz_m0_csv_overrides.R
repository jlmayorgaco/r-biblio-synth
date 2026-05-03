# ============================================================================
# zz_m0_csv_overrides.R - Late-binding M0 CSV ingestion overrides
# ============================================================================

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

m0_single_file_source_spec <- function(spec, file_path) {
  spec$file <- file_path
  spec$files <- NULL
  spec$file_pattern <- NULL
  spec
}

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

m0_merge_sources <- function(raw_list, config = biblio_config()) {
  config <- merge_biblio_config(config)

  if (length(raw_list) == 0) {
    cli::cli_warn("No sources to merge")
    return(data.frame())
  }

  if (length(raw_list) == 1) {
    merged <- m0_harmonize_columns(raw_list[[1]])
    merged <- m0_deduplicate(merged, config)
    merged <- m0_clean_merged(merged)
    return(m0_finalize_provenance_columns(merged))
  }

  harmonized <- lapply(raw_list, m0_harmonize_columns)
  merged <- dplyr::bind_rows(harmonized)
  merged <- m0_deduplicate(merged, config)
  merged <- m0_clean_merged(merged)
  merged <- m0_finalize_provenance_columns(merged)

  if (config$verbose) {
    total_raw <- sum(sapply(raw_list, nrow))
    cli::cli_alert_success("Merged {total_raw} records -> {nrow(merged)} unique records")
  }

  merged
}
