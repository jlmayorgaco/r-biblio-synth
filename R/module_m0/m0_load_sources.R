# ============================================================================
# m0_load_sources.R - Load bibliographic sources into bibliometrix data frames
# ============================================================================

# Null-coalescing operator
`%||%` <- function(a, b) if (!is.null(a)) a else b

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
      m0_load_single_source(spec),
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
m0_load_single_source <- function(spec) {
  file_path <- spec$file
  db        <- spec$db
  fmt       <- spec$format %||% "bibtex"

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
    generic = {
      m0_load_generic(file_path, fmt)
    },
    stop("Unknown db source: ", db)
  )
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
    bibliometrix::convert2df(file = file_path, dbsource = dbsource, format = format)
  }, error = function(e) {
    msg <- conditionMessage(e)
    if (grepl("undefined columns", msg, ignore.case = TRUE)) {
      cli::cli_warn("bibliometrix::convert2df failed ({msg}), trying RefManageR fallback")
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
    cli::cli_warn("RefManageR fallback also failed: {e$message}")
    NULL
  })

  if (!is.null(result) && is.data.frame(result) && nrow(result) > 0) {
    return(m0_standardize_columns(result))
  }

  # Fallback 2: Manual BibTeX parsing
  cli::cli_warn("All standard parsers failed, attempting manual BibTeX parsing")
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

  bib <- RefManageR::ReadBib(file_path, check = "warn")
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
      field <- gsub("\\s*=\\s*\\{.*", "", line)
      field <- trimws(gsub("^\\s*", "", field))
      field <- toupper(field)

      rest <- sub(".*=\\s*\\{(.*)\\}\\s*$", "\\1", line)
      rest <- gsub("\\}\\s*$", "", rest)

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
      } else if (field == "KEYWORDS" || field == "AUTHOR_KEYWORDS") {
        kws <- strsplit(rest, "\\s*;\\s*")[[1]]
        current_entry$DE <- paste(kws, collapse = ";")
      } else if (field == "ABSTRACT") {
        current_entry$AB <- rest
      } else if (field == "PUBLISHER") {
        current_entry$PU <- rest
      } else if (field == "URL") {
        current_entry$UR <- rest
      } else if (field == "ISSN") {
        current_entry$ISSN <- rest
      } else if (field == "ISBN") {
        current_entry$ISBN <- rest
      } else if (field == "LANGUAGE") {
        current_entry$LA <- rest
      } else if (field == "TYPE" || field == "DT") {
        current_entry$DT <- rest
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
  df <- as.data.frame(do.call(rbind, lapply(entries, function(x) {
    row <- lapply(all_cols, function(col) x[[col]] %||% NA)
    names(row) <- all_cols
    row
  })), stringsAsFactors = FALSE)

  if ("PY" %in% names(df)) {
    df$PY <- suppressWarnings(as.integer(df$PY))
  }
  if ("TC" %in% names(df) && !is.na(df$TC)) {
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
    csv = utils::read.csv(file_path, stringsAsFactors = FALSE, encoding = "UTF-8"),
    xlsx = {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        cli::cli_abort("Package 'readxl' required for xlsx import")
      }
      readxl::read_excel(file_path)
    },
    stop("Unsupported generic format: ", fmt)
  )
}
