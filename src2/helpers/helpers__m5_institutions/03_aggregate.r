# ============================================================================ #
# m5i_expand_institution_year — robust row expander (doc -> inst-year rows)
# Skips empty Institution_List and forces TC to a scalar (defaults to 0).
# ============================================================================ #
m5i_expand_institution_year <- function(inst_list, y, tc) {
  # Safe scalar helpers
  .scalar_or <- function(x, default = NA_real_) {
    if (length(x) == 0) return(default)
    if (length(x) > 1)  return(x[[1]])
    x
  }
  .norm_str <- function(x) {
    if (is.null(x) || length(x) == 0) return(character(0))
    x <- as.character(x)
    x <- gsub("[|]", ";", x, perl = TRUE)
    x <- gsub("\\s*[,;]\\s*", ";", x, perl = TRUE)   # normalize separators
    x <- trimws(x)
    if (identical(x, NA_character_)) x <- ""
    x
  }

  y  <- .scalar_or(y, default = NA_integer_)
  tc <- .scalar_or(tc, default = 0)

  inst_raw <- .norm_str(inst_list)
  if (length(inst_raw) == 0 || nchar(inst_raw) == 0) {
    return(utils::head(data.frame(institution=character(0),
                                  year=integer(0),
                                  TP=integer(0),
                                  TC=numeric(0)), 0))
  }

  insts <- unlist(strsplit(inst_raw, ";", fixed = TRUE), use.names = FALSE)
  insts <- unique(trimws(insts))
  insts <- insts[nzchar(insts)]
  if (length(insts) == 0 || is.na(y)) {
    return(utils::head(data.frame(institution=character(0),
                                  year=integer(0),
                                  TP=integer(0),
                                  TC=numeric(0)), 0))
  }

  # One row per institution; TC is attributed per-doc (can later choose to divide)
  data.frame(
    institution = insts,
    year        = as.integer(y),
    TP          = 1L,
    TC          = as.numeric(ifelse(is.na(tc), 0, tc)),
    stringsAsFactors = FALSE
  )
}

# ============================================================================ #
# m5i_aggregate_documents_to_inst_year — from docs (with Institution_List)
# to Institution–Year table with TP and TC.
# Args:
#   df: data.frame with columns Institution_List, year, citations (optional)
#   inst_list_col: name of the institution list column
#   year_col: year column (e.g., "Year" or "Publication Year")
#   citation_col: optional citations column (e.g., "Times Cited")
# Returns: data.frame(institution, year, TP, TC)
# ============================================================================ #
m5i_aggregate_documents_to_inst_year <- function(df,
                                                 inst_list_col = "Institution_List",
                                                 year_col = NULL,
                                                 citation_col = NULL) {
  stopifnot(is.data.frame(df))
  if (!inst_list_col %in% names(df)) {
    stop(sprintf("'%s' not found. Did m5i_prepare_documents() run?", inst_list_col))
  }

  # Guess sensible defaults if not provided
  .col_first <- function(d, candidates) {
    hits <- candidates[candidates %in% names(d)]
    if (length(hits)) hits[[1]] else NULL
  }
  year_col     <- year_col %||% .col_first(df, c("Year","Publication Year","PY","Publication.Year","year"))
  citation_col <- citation_col %||% .col_first(df, c("Times Cited","TC","Citations","times_cited","tc"))

  if (is.null(year_col)) stop("No year column found; provide 'year_col'.")

  # Vectorized expand, robust to empties and length-0
  exp_list <- lapply(seq_len(nrow(df)), function(i) {
    inst_list <- df[[inst_list_col]][[i]]
    y         <- df[[year_col]][[i]]
    tc        <- if (!is.null(citation_col)) df[[citation_col]][[i]] else 0
    m5i_expand_institution_year(inst_list, y, tc)
  })

  out <- dplyr::bind_rows(exp_list)

  if (!nrow(out)) {
    # Return empty but typed frame (prevents later errors)
    return(utils::head(data.frame(
      institution = character(0),
      year        = integer(0),
      TP          = integer(0),
      TC          = numeric(0)
    ), 0))
  }

  out %>%
    dplyr::group_by(institution, year) %>%
    dplyr::summarise(
      TP = sum(TP, na.rm = TRUE),
      TC = sum(TC, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(institution, year)
}

# Safe infix (if not already defined)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
