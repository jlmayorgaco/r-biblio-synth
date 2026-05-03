# ============================================================================
# json_service.R - JSON export service
# ============================================================================

#' Write a JSON artifact file
#'
#' @param x Object to serialize.
#' @param path Character. Output file path.
#' @param auto_unbox Logical. Auto-unbox single-element vectors.
#' @param pretty Logical. Pretty-print JSON.
#' @return Invisibly returns the path.
#' @export
write_json_artifact <- function(x, path,
                                auto_unbox = TRUE,
                                pretty = TRUE) {
  # Create directory if needed
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) {
    dir_created <- dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    if (!dir_created && !dir.exists(dir_path)) {
      cli::cli_warn("Failed to create directory: {dir_path}")
      return(invisible(NULL))
    }
  }
  
  # Validate input is serializable
  if (is.null(x)) {
    cli::cli_warn("Cannot write NULL object to JSON")
    return(invisible(NULL))
  }

  x <- json_sanitize_artifact(x)
  
  # Write with error handling
  tryCatch({
    jsonlite::write_json(x, path, auto_unbox = auto_unbox, pretty = pretty)
  }, error = function(e) {
    cli::cli_warn("Failed to write JSON to {path}: {e$message}")
  })
  
  invisible(path)
}

json_sanitize_artifact <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

  if (inherits(x, "table")) {
    return(as.data.frame(x, stringsAsFactors = FALSE))
  }

  if (inherits(x, "dist")) {
    return(json_sanitize_artifact(as.matrix(x)))
  }

  if (is.matrix(x)) {
    out <- as.data.frame(x, stringsAsFactors = FALSE)
    row_id <- rownames(x)
    if (is.null(row_id)) {
      row_id <- seq_len(nrow(out))
    }
    out <- cbind(rowname = row_id, out, stringsAsFactors = FALSE)
    rownames(out) <- NULL
    return(out)
  }

  if (is.data.frame(x)) {
    out <- x
    factor_cols <- vapply(out, is.factor, logical(1))
    out[factor_cols] <- lapply(out[factor_cols], as.character)
    return(out)
  }

  if (is.list(x)) {
    return(lapply(x, json_sanitize_artifact))
  }

  if (is.factor(x)) {
    return(as.character(x))
  }

  if (is.language(x) || inherits(x, "call")) {
    return(paste(deparse(x), collapse = " "))
  }

  x
}
