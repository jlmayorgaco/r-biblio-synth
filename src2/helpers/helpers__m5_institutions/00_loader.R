# ============================================================================ #
# m5i_source_helpers — Load all .r/.R helpers in a folder, except 00_loader.R
# ============================================================================ #
m5i_source_helpers <- function(dir, debug = TRUE, dry_run = FALSE) {
  stopifnot(dir.exists(dir))

  # List all .r/.R files (non-recursive)
  all_r <- list.files(dir, pattern = "\\.[Rr]$", full.names = TRUE, recursive = FALSE)

  # Exclude 00_loader.R (exact basename match, case-sensitive on purpose)
  exclude_name <- "00_loader.R"
  keep <- basename(all_r) != exclude_name
  files <- all_r[keep]

  # Sort deterministically by basename
  files <- files[order(basename(files))]

  if (debug) {
    message(sprintf("[M5I Loader] Dir: %s", normalizePath(dir, winslash = "/")))
    message(sprintf("[M5I Loader] Found %d .r/.R files, excluding '%s': %d to load",
                    length(all_r), exclude_name, length(files)))
    if (length(files)) message("  Will load:\n  - ", paste(basename(files), collapse = "\n  - "))
  }

  if (dry_run) {
    if (debug) message("[M5I Loader] Dry-run: no files will be sourced.")
    return(invisible(list(files = files, loaded = character(), skipped = setdiff(all_r, files))))
  }

  loaded <- character()
  for (f in files) {
    if (debug) message(sprintf("[M5I Loader] Sourcing: %s", basename(f)))
    tryCatch({
      sys.source(f, envir = parent.frame(), keep.source = TRUE)
      loaded <- c(loaded, basename(f))
      if (debug) message(sprintf("[M5I Loader] ✔ %s", basename(f)))
    }, error = function(e) {
      stop(sprintf("[M5I Loader] ✖ Error in %s: %s", basename(f), conditionMessage(e)), call. = FALSE)
    })
  }

  if (debug) message(sprintf("[M5I Loader] Done. Loaded %d file(s).", length(loaded)))
  invisible(list(files = files, loaded = loaded, skipped = setdiff(all_r, files)))
}



# -----------------------------------------------------------------------------
# helpers__m5_institutions / 00_loader.R  (append this)
# Robust reader for institutions alias metadata
# Expected CSV columns (case/spacing-insensitive):
#   - NAME        : canonical institution name (required)
#   - ALIAS       : a known alias / variant (optional; can be blank)
#   - COUNTRY     : country (optional)
#   - GRID_ID     : GRID id (optional)
#   - ROR_ID      : ROR id (optional)
# You may include additional columns; they’ll be carried through if present.
# The function returns a tibble with at least:
#   inst_canon, alias, country, grid_id, ror_id
# One row per alias (or per NAME if ALIAS missing).
# -----------------------------------------------------------------------------

m5i_institution_meta_read <- function(csv_path) {
  if (is.null(csv_path) || !nzchar(csv_path) || !file.exists(csv_path)) {
    stop(sprintf("[M5][meta_read] File not found: %s", as.character(csv_path)))
  }

  # small local helpers (no janitor needed)
  .norm_names <- function(x) {
    x <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
    gsub("^_|_$", "", x)
  }
  .trim <- function(x) {
    x <- as.character(x)
    x <- gsub("^\\s+|\\s+$", "", x)
    x[nchar(x) == 0] <- NA_character_
    x
  }

  # read as character by default (keeps ids intact), coerce later if needed
  suppressWarnings({
    df_raw <- readr::read_csv(csv_path,
                              col_types = readr::cols(.default = readr::col_character()))
  })

  if (!nrow(df_raw)) {
    stop(sprintf("[M5][meta_read] Empty CSV: %s", csv_path))
  }

  # normalize column names once
  names(df_raw) <- .norm_names(names(df_raw))

  # try to find expected columns (very forgiving)
  col_name    <- dplyr::coalesce(
    dplyr::first(names(df_raw)[names(df_raw) %in% c("name","institution","inst","canonical","canon")]),
    NA_character_
  )
  col_alias   <- dplyr::first(names(df_raw)[names(df_raw) %in% c("alias","aka","synonym","variant")])
  col_country <- dplyr::first(names(df_raw)[names(df_raw) %in% c("country","pais")])
  col_grid    <- dplyr::first(names(df_raw)[names(df_raw) %in% c("grid_id","grid")])
  col_ror     <- dplyr::first(names(df_raw)[names(df_raw) %in% c("ror_id","ror")])

  if (is.na(col_name)) {
    stop(paste0(
      "[M5][meta_read] Required column not found. ",
      "Please include a 'NAME' (or 'Institution', 'Canonical') column in: ", csv_path
    ))
  }

  # build the tidy table
  df <- df_raw %>%
    dplyr::transmute(
      inst_canon = .trim(.data[[col_name]]),
      alias      = if (!is.null(col_alias)) .trim(.data[[col_alias]]) else NA_character_,
      country    = if (!is.null(col_country)) .trim(.data[[col_country]]) else NA_character_,
      grid_id    = if (!is.null(col_grid)) .trim(.data[[col_grid]]) else NA_character_,
      ror_id     = if (!is.null(col_ror))  .trim(.data[[col_ror]])  else NA_character_
    ) %>%
    # if alias is missing, use the canonical name as its own alias
    dplyr::mutate(alias = dplyr::coalesce(alias, inst_canon)) %>%
    # drop rows without canonical name
    dplyr::filter(!is.na(inst_canon)) %>%
    # unify spacing/case for robust matching
    dplyr::mutate(
      inst_canon_norm = stringr::str_squish(stringr::str_to_lower(inst_canon)),
      alias_norm      = stringr::str_squish(stringr::str_to_lower(alias))
    ) %>%
    # de-dup in a stable way
    dplyr::distinct(alias_norm, inst_canon_norm, .keep_all = TRUE) %>%
    # final, clean order
    dplyr::select(inst_canon, alias, country, grid_id, ror_id,
                  inst_canon_norm, alias_norm)

  if (!nrow(df)) {
    stop(sprintf("[M5][meta_read] No valid rows after cleaning: %s", csv_path))
  }

  message(sprintf(
    "[M5][meta_read] OK | institutions=%d | aliases=%d | file=%s",
    length(unique(df$inst_canon_norm)), nrow(df), csv_path
  ))

  df
}
