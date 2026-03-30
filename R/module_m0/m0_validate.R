# ============================================================================
# m0_validate.R - Validation for M0 Data Orchestrator
# ============================================================================

# Null-coalescing operator
`%||%` <- function(a, b) if (!is.null(a)) a else b

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

  allowed_dbs <- c("scopus", "wos", "openalex", "generic")
  allowed_fmt <- c("bibtex", "plaintext", "csv", "xlsx")

  details <- list()
  for (nm in names(sources)) {
    spec <- sources[[nm]]
    if (!is.list(spec)) {
      return(list(ok = FALSE, error = paste0("Source '", nm, "' must be a list")))
    }
    if (is.null(spec$file) || !file.exists(spec$file)) {
      return(list(ok = FALSE, error = paste0("Source '", nm, "': file not found: ", spec$file)))
    }
    if (is.null(spec$db) || !(spec$db %in% allowed_dbs)) {
      return(list(ok = FALSE, error = paste0("Source '", nm, "': db must be one of ",
                                              paste(allowed_dbs, collapse = ", "))))
    }
    if (!is.null(spec$format) && !(spec$format %in% allowed_fmt)) {
      return(list(ok = FALSE, error = paste0("Source '", nm, "': format must be one of ",
                                              paste(allowed_fmt, collapse = ", "))))
    }
    details[[nm]] <- list(db = spec$db, file = spec$file,
                          format = spec$format %||% "bibtex")
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
