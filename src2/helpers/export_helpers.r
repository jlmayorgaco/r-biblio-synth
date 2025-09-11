# Compact a module's results so exports stay small.
# - removes known heavy fields by name (df, plots, spectra, raw data)
# - replaces any data.frame/tibble anywhere in the tree with a small placeholder
# - truncates giant atomic vectors
compact_results <- function(x,
                            drop_names = c(
                              "df", "plots", "data", "raw", "raw_df",
                              "df_tp", "df_tc", "spectrum", "resid_spectrum"
                            ),
                            max_atomic_len = 2000) {

  # 1) Data frames / tibbles -> lightweight stub
  if (inherits(x, c("data.frame", "tbl", "tbl_df"))) {
    return(sprintf("<omitted %s: %d rows x %d cols>",
                   class(x)[1], nrow(x), ncol(x)))
  }

  # 2) Big atomic vectors -> stub
  if (is.atomic(x) && length(x) > max_atomic_len) {
    return(sprintf("<omitted %s length %d>", typeof(x), length(x)))
  }

  # 3) Environments / R6 / external ptrs / S4 -> stub
  if (is.environment(x)) {
    return("<omitted environment>")
  }
  if (isS4(x)) {
    return(sprintf("<omitted S4 object: %s>", paste(class(x), collapse = "/")))
  }

  # 4) List-like objects
  if (is.list(x)) {
    # If it's a complex S3 object (not a plain list), stub to avoid fragile recursion
    cls <- class(x)
    if (!is.null(cls) && !identical(cls, "list")) {
      # keep common simple classes; otherwise stub
      keep_classes <- c()  # add "list" only; everything else gets stubbed
      if (!any(cls %in% keep_classes)) {
        return(sprintf("<omitted object of class %s>", paste(cls, collapse = "/")))
      }
    }

    # Drop heavy named fields first (without mutating during iteration)
    if (!is.null(names(x))) {
      x <- x[setdiff(names(x), drop_names)]
    }

    # Pairlists can be awkward; normalize to plain list
    if (is.pairlist(x)) x <- as.list(x)

    # Recurse safely using lapply (no in-place [[<-])
    x <- lapply(x, function(el) compact_results(el, drop_names, max_atomic_len))

    # Preserve names
    return(x)
  }

  # 5) Anything else (language objects, formulas, etc.) -> leave as-is
  x
}
