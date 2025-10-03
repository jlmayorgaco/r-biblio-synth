# ============================================================================ #
# helpers__m1_data_ingestion/01_utils.R
# Small utilities shared across M1 helpers
# ============================================================================ #

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}
m1i_ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}
m1i_is_true <- function(x) isTRUE(x)

m1i_debug_enabled <- function() isTRUE(getOption("m1i.debug", FALSE))

m1i_dbg <- function(...) {
  if (m1i_debug_enabled()) cat("[M1][debug]", sprintf(...), "\n")
}

m1i_try_require <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) return(FALSE)
  TRUE
}

# Flatten list-columns (e.g., Country_List) so CSV export works
m1i_flatten_list_columns_for_csv <- function(df) {
  is_list_col <- vapply(df, is.list, logical(1))
  if (!any(is_list_col)) return(df)
  for (nm in names(df)[is_list_col]) {
    df[[nm]] <- vapply(
      df[[nm]],
      function(x) {
        if (is.null(x)) return(NA_character_)
        if (is.atomic(x)) return(paste(x, collapse = "; "))
        if (is.list(x))   return(paste(unlist(x), collapse = "; "))
        as.character(x)
      },
      FUN.VALUE = character(1)
    )
  }
  df
}
