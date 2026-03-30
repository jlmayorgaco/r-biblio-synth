# ============================================================================
# logging.R - Structured Logging Framework for RBiblioSynth
# ============================================================================
# Provides IEEE-standard logging with levels, context, and file output

#' Initialize logging system
#'
#' @param level Log level: "DEBUG", "INFO", "WARN", "ERROR", "FATAL"
#' @param file Optional log file path. If NULL, logs to console only.
#' @param format Log format: "simple", "detailed", "json"
#' @return Invisible NULL
#' @export
#' @examples
#' init_logging(level = "INFO")
#' init_logging(level = "DEBUG", file = "pipeline.log")
init_logging <- function(level = "INFO", file = NULL, format = "detailed") {
  # Validate level
  valid_levels <- c("DEBUG", "INFO", "WARN", "ERROR", "FATAL")
  if (!level %in% valid_levels) {
    stop("Invalid log level. Must be one of: ", paste(valid_levels, collapse = ", "))
  }
  
  # Store configuration in global option
  options(rbibliosynth.log.level = level)
  options(rbibliosynth.log.file = file)
  options(rbibliosynth.log.format = format)
  options(rbibliosynth.log.enabled = TRUE)
  
  # Initialize log file if specified
  if (!is.null(file)) {
    # Create directory if needed
    log_dir <- dirname(file)
    if (!dir.exists(log_dir)) {
      dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Write header
    write_log_header(file)
  }
  
  log_message("INFO", "Logging initialized at {level} level")
  invisible(NULL)
}

#' Write log header
#' @keywords internal
write_log_header <- function(file) {
  header <- sprintf(
    "# RBiblioSynth Log\n# Started: %s\n# R Version: %s\n# Platform: %s\n%s\n",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    R.version.string,
    R.version$platform,
    paste(rep("-", 70), collapse = "")
  )
  writeLines(header, file)
}

#' Log a message
#'
#' @param level Log level
#' @param msg Message with glue-style interpolation {var}
#' @param ... Named variables for interpolation
#' @param context Optional context list (module, function, etc.)
#' @export
#' @examples
#' log_message("INFO", "Processing {n} records", n = 100)
#' log_message("WARN", "Missing column: {col}", col = "PY", 
#'             context = list(module = "M1", function = "run_m1"))
log_message <- function(level, msg, ..., context = NULL) {
  # Check if logging is enabled
  if (!isTRUE(getOption("rbibliosynth.log.enabled", TRUE))) {
    return(invisible(NULL))
  }
  
  # Get current log level
  current_level <- getOption("rbibliosynth.log.level", "INFO")
  
  # Check if this message should be logged
  level_order <- c("DEBUG" = 1, "INFO" = 2, "WARN" = 3, "ERROR" = 4, "FATAL" = 5)
  if (level_order[level] < level_order[current_level]) {
    return(invisible(NULL))
  }
  
  # Get format
  format <- getOption("rbibliosynth.log.format", "detailed")
  
  # Format message
  formatted_msg <- format_log_message(level, msg, ..., context = context, format = format)
  
  # Output to console
  if (level %in% c("WARN", "ERROR", "FATAL")) {
    message(formatted_msg)
  } else if (isTRUE(getOption("rbibliosynth.verbose", TRUE))) {
    cat(formatted_msg, "\n")
  }
  
  # Output to file if configured
  log_file <- getOption("rbibliosynth.log.file", NULL)
  if (!is.null(log_file)) {
    writeLines(formatted_msg, log_file)
  }
  
  invisible(NULL)
}

#' Format log message
#' @keywords internal
format_log_message <- function(level, msg, ..., context = NULL, format = "detailed") {
  # Interpolate message
  args <- list(...)
  if (length(args) > 0) {
    for (nm in names(args)) {
      msg <- gsub(paste0("\\{", nm, "\\}"), as.character(args[[nm]]), msg, fixed = TRUE)
    }
  }
  
  # Get timestamp
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  if (format == "simple") {
    sprintf("[%s] %s: %s", timestamp, level, msg)
  } else if (format == "json") {
    # JSON format for structured logging
    jsonlite::toJSON(list(
      timestamp = timestamp,
      level = level,
      message = msg,
      context = context
    ), auto_unbox = TRUE)
  } else {
    # Detailed format
    context_str <- ""
    if (!is.null(context)) {
      ctx_parts <- sapply(names(context), function(k) {
        sprintf("%s=%s", k, context[[k]])
      })
      context_str <- paste0(" [", paste(ctx_parts, collapse = " "), "]")
    }
    
    sprintf("[%s] %-5s: %s%s", timestamp, level, msg, context_str)
  }
}

#' Convenience functions for specific log levels
#' @param msg Message with optional interpolation
#' @param ... Variables for interpolation
#' @param context Optional context
#' @export
debug <- function(msg, ..., context = NULL) {
  log_message("DEBUG", msg, ..., context = context)
}

#' @export
info <- function(msg, ..., context = NULL) {
  log_message("INFO", msg, ..., context = context)
}

#' @export
warn <- function(msg, ..., context = NULL) {
  log_message("WARN", msg, ..., context = context)
}

#' @export
error <- function(msg, ..., context = NULL) {
  log_message("ERROR", msg, ..., context = context)
}

#' @export
fatal <- function(msg, ..., context = NULL) {
  log_message("FATAL", msg, ..., context = context)
}

#' Progress logging for long-running operations
#'
#' @param current Current step
#' @param total Total steps
#' @param msg Optional message
#' @export
log_progress <- function(current, total, msg = NULL) {
  percent <- round(100 * current / total, 1)
  progress_msg <- if (is.null(msg)) {
    sprintf("Progress: %d/%d (%.1f%%)", current, total, percent)
  } else {
    sprintf("%s: %d/%d (%.1f%%)", msg, current, total, percent)
  }
  
  # Only log progress at certain intervals to avoid spam
  if (current == 1 || current == total || current %% max(1, round(total / 10)) == 0) {
    log_message("INFO", progress_msg)
  }
  
  invisible(NULL)
}

#' Enable/disable logging
#'
#' @param enabled TRUE to enable, FALSE to disable
#' @export
set_logging_enabled <- function(enabled = TRUE) {
  options(rbibliosynth.log.enabled = enabled)
}

#' Get current log configuration
#'
#' @return List with current logging configuration
#' @export
get_log_config <- function() {
  list(
    level = getOption("rbibliosynth.log.level", "INFO"),
    file = getOption("rbibliosynth.log.file", NULL),
    format = getOption("rbibliosynth.log.format", "detailed"),
    enabled = getOption("rbibliosynth.log.enabled", TRUE)
  )
}

#' Close log file
#'
#' Should be called at end of pipeline
#' @export
close_logging <- function() {
  log_message("INFO", "Logging session ended")
  options(rbibliosynth.log.file = NULL)
  invisible(NULL)
}