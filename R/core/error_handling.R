# ============================================================================
# error_handling.R - Robust Error Handling for RBiblioSynth
# ============================================================================
# Provides retry logic, graceful degradation, and comprehensive error reporting

#' Try expression with retry logic
#'
#' Attempts to evaluate an expression, retrying on failure with exponential backoff.
#' Ideal for network operations or file I/O that may fail transiently.
#'
#' @param expr Expression to evaluate
#' @param max_retries Maximum number of retry attempts (default: 3)
#' @param initial_delay Initial delay in seconds (default: 1)
#' @param backoff Backoff strategy: "exponential", "fixed", "linear"
#' @param retry_on Function that returns TRUE if error is retryable
#' @param on_retry Function to call on each retry attempt
#' @return Result of expr
#' @export
#' @examples
#' \dontrun{
#' # Retry network request up to 3 times
#' result <- try_with_retry({
#'   download.file(url, destfile)
#' }, max_retries = 3, backoff = "exponential")
#'
#' # Custom retry condition
#' result <- try_with_retry({
#'   api_call()
#' }, retry_on = function(e) grepl("timeout", e$message))
#' }
try_with_retry <- function(expr, max_retries = 3, initial_delay = 1,
                           backoff = "exponential",
                           retry_on = NULL,
                           on_retry = NULL) {
  last_error <- NULL
  delay <- initial_delay
  
  for (attempt in 0:max_retries) {
    result <- tryCatch(
      {
        # Success - return result
        return(force(expr))
      },
      error = function(e) {
        last_error <<- e
        
        # Check if we should retry
        should_retry <- if (is.null(retry_on)) {
          TRUE  # Retry all errors by default
        } else {
          retry_on(e)
        }
        
        if (!should_retry || attempt >= max_retries) {
          return(e)
        }
        
        # Log retry
        log_message("WARN", 
                   "Attempt {attempt}/{max} failed: {msg}. Retrying in {delay}s...",
                   attempt = attempt + 1,
                   max = max_retries + 1,
                   msg = conditionMessage(e),
                   delay = delay,
                   context = list(module = "retry", attempt = attempt + 1))
        
        # Call on_retry callback if provided
        if (!is.null(on_retry)) {
          on_retry(e, attempt + 1)
        }
        
        # Wait before retry
        Sys.sleep(delay)
        
        # Update delay for next iteration
        delay <- switch(backoff,
                       exponential = delay * 2,
                       linear = delay + initial_delay,
                       fixed = delay,
                       delay * 2)  # Default to exponential
        
        NULL  # Return NULL to continue loop
      }
    )
    
    # If result is error, it was the last attempt
    if (inherits(result, "error")) {
      break
    }
  }
  
  # All retries exhausted
  stop(sprintf("Failed after %d attempts: %s", 
               max_retries + 1, 
               conditionMessage(last_error)))
}

#' Safe computation wrapper
#'
#' Wraps a function to return a default value on error instead of failing.
#' Useful for non-critical operations where partial results are acceptable.
#'
#' @param fun Function to wrap
#' @param default Default value to return on error
#' @param on_error Optional callback function called on error
#' @return Wrapped function
#' @export
#' @examples
#' \dontrun{
#' # Wrap a function that might fail
#' safe_mean <- safe_compute(mean, default = NA_real_)
#' result <- safe_mean(c())  # Returns NA instead of error
#'
#' # With error callback
#' safe_read <- safe_compute(read.csv, default = NULL,
#'                           on_error = function(e) message("File read failed"))
#' }
safe_compute <- function(fun, default = NULL, on_error = NULL) {
  function(...) {
    tryCatch(
      fun(...),
      error = function(e) {
        # Log error
        log_message("WARN", "Computation failed: {msg}", 
                   msg = conditionMessage(e),
                   context = list(func = deparse(substitute(fun))))
        
        # Call error callback
        if (!is.null(on_error)) {
          on_error(e)
        }
        
        # Return default
        default
      }
    )
  }
}

#' Validate input with detailed error messages
#'
#' @param input Input to validate
#' @param checks List of check functions returning TRUE/FALSE
#' @param descriptions Named list describing each check
#' @return TRUE if valid, stops with error message if not
#' @export
validate_input <- function(input, checks, descriptions = NULL) {
  errors <- character()
  
  for (check_name in names(checks)) {
    check_fn <- checks[[check_name]]
    
    if (!is.function(check_fn)) {
      next
    }
    
    result <- tryCatch(
      check_fn(input),
      error = function(e) FALSE
    )
    
    if (!isTRUE(result)) {
      desc <- if (!is.null(descriptions) && check_name %in% names(descriptions)) {
        descriptions[[check_name]]
      } else {
        paste0("Check '", check_name, "' failed")
      }
      errors <- c(errors, desc)
    }
  }
  
  if (length(errors) > 0) {
    stop("Input validation failed:\n  - ", paste(errors, collapse = "\n  - "))
  }
  
  TRUE
}

#' Common validation checks
#' @export
validation_checks <- list(
  is_data_frame = function(x) is.data.frame(x),
  is_not_empty = function(x) nrow(x) > 0,
  has_rows = function(x, min_rows = 1) nrow(x) >= min_rows,
  has_columns = function(x, min_cols = 1) ncol(x) >= min_cols,
  has_no_na_rows = function(x) !any(apply(x, 1, function(row) all(is.na(row)))),
  is_numeric = function(x, col) is.numeric(x[[col]]),
  is_character = function(x, col) is.character(x[[col]]),
  has_column = function(x, col) col %in% names(x),
  has_columns_named = function(x, cols) all(cols %in% names(x))
)

#' Graceful fallback chain
#'
#' Tries multiple alternatives until one succeeds
#'
#' @param ... Expressions to try in order
#' @param default Final fallback value if all fail
#' @return Result of first successful expression, or default
#' @export
#' @examples
#' \dontrun{
#' # Try multiple data sources
#' data <- fallback_chain(
#'   readRDS("cache/data.rds"),
#'   read.csv("data.csv"),
#'   query_database(),
#'   default = data.frame()
#' )
#' }
fallback_chain <- function(..., default = NULL) {
  expressions <- list(...)
  
  for (i in seq_along(expressions)) {
    result <- tryCatch(
      force(expressions[[i]]),
      error = function(e) {
        log_message("DEBUG", "Fallback {i} failed: {msg}",
                   i = i, msg = conditionMessage(e))
        NULL
      }
    )
    
    if (!is.null(result)) {
      return(result)
    }
  }
  
  log_message("WARN", "All fallback options exhausted, returning default")
  default
}

#' Context-aware error handler
#'
#' Creates an error handler with context information
#'
#' @param context List with context (module, function, step, etc.)
#' @param on_error "stop", "warn", "ignore", or custom function
#' @return Error handler function
#' @export
create_error_handler <- function(context = list(), on_error = "stop") {
  function(e) {
    # Enrich error with context
    enriched_msg <- sprintf("[%s::%s] %s",
                           context$module %||% "unknown",
                           context$func %||% "unknown",
                           conditionMessage(e))
    
    # Log error
    log_message("ERROR", enriched_msg, context = context)
    
    # Handle based on policy
    switch(on_error,
           stop = stop(enriched_msg, call. = FALSE),
           warn = warning(enriched_msg, call. = FALSE),
           ignore = invisible(NULL),
           if (is.function(on_error)) on_error(e) else stop(e))
  }
}

#' Batch processing with error isolation
#'
#' Processes items in batch, isolating errors so one failure doesn't stop all
#'
#' @param items List of items to process
#' @param fun Function to apply to each item
#' @param on_error How to handle errors: "skip", "default", "collect"
#' @param default Default value for failed items
#' @return List of results (may contain NULLs or defaults for failed items)
#' @export
#' @examples
#' \dontrun{
#' # Process files, skipping failures
#' results <- batch_process(file_list, read.csv, on_error = "skip")
#'
#' # Collect all errors
#' results <- batch_process(file_list, process_file, on_error = "collect")
#' # results$successes and results$errors
#' }
batch_process <- function(items, fun, on_error = "skip", default = NULL) {
  n <- length(items)
  results <- vector("list", n)
  errors <- list()
  
  for (i in seq_len(n)) {
    result <- tryCatch(
      fun(items[[i]]),
      error = function(e) {
        log_message("WARN", "Item {i}/{n} failed: {msg}",
                   i = i, n = n, msg = conditionMessage(e))
         list(error_obj = e, error_index = i)
      }
    )
    
    if (is.list(result) && !is.null(result$error_obj)) {
      # Error occurred
      errors[[length(errors) + 1]] <- list(
        index = result$error_index,
        item = items[[result$error_index]],
        error = result$error_obj
      )
      
      results[[i]] <- switch(on_error,
                            skip = NULL,
                            default = default,
                            collect = list(error = result$error_obj))
    } else {
      results[[i]] <- result
    }
  }
  
  if (on_error == "collect" && length(errors) > 0) {
    list(
      successes = results[!sapply(results, function(r) !is.null(r$error))],
      errors = errors,
      error_rate = length(errors) / n
    )
  } else {
    results
  }
}

#' Assertion with custom error message
#'
#' @param condition Condition to check
#' @param msg Error message (glue-style interpolation supported)
#' @param ... Variables for interpolation
#' @export
assert <- function(condition, msg, ...) {
  if (!isTRUE(condition)) {
    args <- list(...)
    for (nm in names(args)) {
      msg <- gsub(paste0("\\{", nm, "\\}"), as.character(args[[nm]]), msg, fixed = TRUE)
    }
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

#' Check if error is retryable
#'
#' Determines if an error is likely transient and worth retrying
#'
#' @param error Error object
#' @return TRUE if error appears retryable
#' @export
is_retryable_error <- function(error) {
  msg <- tolower(conditionMessage(error))
  
  # Network/IO errors that are often transient
  retryable_patterns <- c(
    "timeout",
    "connection",
    "network",
    "temporarily",
    "unavailable",
    "try again",
    "busy",
    "rate limit",
    "503",  # Service unavailable
    "502",  # Bad gateway
    "504"   # Gateway timeout
  )
  
  any(sapply(retryable_patterns, function(p) grepl(p, msg, fixed = TRUE)))
}

#' Comprehensive error report
#'
#' Generates a detailed error report for debugging
#'
#' @param error Error object
#' @param context Additional context
#' @return Character string with full error report
#' @export
generate_error_report <- function(error, context = list()) {
  lines <- c(
    "=== ERROR REPORT ===",
    paste0("Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("R Version: ", R.version.string),
    paste0("Package Version: ", as.character(utils::packageVersion("RBiblioSynth"))),
    "",
    "Error Message:",
    conditionMessage(error),
    "",
    "Call Stack:",
    paste(capture.output(traceback()), collapse = "\n")
  )
  
  if (length(context) > 0) {
    lines <- c(lines, "", "Context:",
               paste(names(context), context, sep = ": "))
  }
  
  lines <- c(lines, "", "=== END REPORT ===")
  
  paste(lines, collapse = "\n")
}