# ============================================================================
# parallel.R - Parallel Processing Infrastructure for RBiblioSynth
# ============================================================================

#' Setup parallel backend
#'
#' Configures the future package for parallel processing with automatic
#' detection of available cores and fallback to sequential if unavailable.
#'
#' @param n_cores Number of cores to use (NULL = auto-detect, 1 = sequential)
#' @param strategy Parallel strategy: "multisession", "multicore", "cluster"
#' @return TRUE if parallel backend is active, FALSE if sequential
#' @export
#' @examples
#' \dontrun{
#' # Auto-detect cores
#' setup_parallel()
#'
#' # Use specific number of cores
#' setup_parallel(n_cores = 4)
#'
#' # Force sequential processing
#' setup_parallel(n_cores = 1)
#' }
setup_parallel <- function(n_cores = NULL, strategy = "multisession") {
  # Check if future package is available
  if (!requireNamespace("future", quietly = TRUE)) {
    warning("future package not available. Running sequentially.")
    options(rbibliosynth.parallel = FALSE)
    return(FALSE)
  }
  
  # Detect cores if not specified
  if (is.null(n_cores)) {
    n_cores <- max(1, future::availableCores() - 1)
  }
  
  # Sequential mode
  if (n_cores <= 1) {
    future::plan("sequential")
    options(rbibliosynth.parallel = FALSE)
    options(rbibliosynth.n_cores = 1)
    log_message("INFO", "Parallel processing disabled (sequential mode)")
    return(FALSE)
  }
  
  # Try to set up parallel backend
  tryCatch({
    # Choose strategy based on OS and availability
    if (strategy == "multicore" && .Platform$OS.type == "unix") {
      future::plan("multicore", workers = n_cores)
    } else {
      future::plan("multisession", workers = n_cores)
    }
    
    options(rbibliosynth.parallel = TRUE)
    options(rbibliosynth.n_cores = n_cores)
    
    log_message("INFO", "Parallel processing enabled with {n} cores", n = n_cores,
                context = list(strategy = strategy))
    
    TRUE
  }, error = function(e) {
    warning("Failed to setup parallel backend: ", conditionMessage(e),
            "\nFalling back to sequential processing.")
    future::plan("sequential")
    options(rbibliosynth.parallel = FALSE)
    options(rbibliosynth.n_cores = 1)
    FALSE
  })
}

#' Cleanup parallel workers
#'
#' Resets to sequential plan and cleans up resources
#' @export
cleanup_parallel <- function() {
  if (requireNamespace("future", quietly = TRUE)) {
    future::plan("sequential")
  }
  options(rbibliosynth.parallel = FALSE)
  log_message("INFO", "Parallel workers cleaned up")
  invisible(TRUE)
}

#' Check if parallel processing is available
#'
#' @return TRUE if parallel processing is available and configured
#' @export
is_parallel_active <- function() {
  isTRUE(getOption("rbibliosynth.parallel", FALSE))
}

#' Get number of cores
#'
#' @return Number of cores (1 if sequential)
#' @export
get_n_cores <- function() {
  getOption("rbibliosynth.n_cores", 1)
}

#' Parallel map with progress tracking
#'
#' Maps a function over a list/vector in parallel with optional progress bar.
#' Automatically falls back to sequential if parallel is not available.
#'
#' @param x List or vector to iterate over
#' @param fun Function to apply to each element
#' @param ... Additional arguments to fun
#' @param progress Show progress bar (default: TRUE if verbose)
#' @return List of results
#' @export
#' @examples
#' \dontrun{
#' # Process items in parallel
#' results <- parallel_map(items, process_item)
#'
#' # With additional arguments
#' results <- parallel_map(items, compute_metric, param = 5)
#' }
parallel_map <- function(x, fun, ..., progress = NULL) {
  # Determine if we should show progress
  if (is.null(progress)) {
    progress <- isTRUE(getOption("rbibliosynth.verbose", TRUE))
  }
  
  # Check if parallel is available and active
  use_parallel <- is_parallel_active() && requireNamespace("furrr", quietly = TRUE)
  
  if (use_parallel) {
    # Use furrr for parallel mapping
    if (progress && requireNamespace("progressr", quietly = TRUE)) {
      # With progress bar
      results <- progressr::with_progress({
        p <- progressr::progressor(steps = length(x))
        furrr::future_map(x, function(item) {
          result <- fun(item, ...)
          p()
          result
        })
      })
    } else {
      # Without progress bar
      results <- furrr::future_map(x, fun, ...)
    }
  } else {
    # Sequential fallback
    if (progress) {
      results <- lapply(seq_along(x), function(i) {
        if (i %% max(1, round(length(x) / 10)) == 0) {
          log_progress(i, length(x))
        }
        fun(x[[i]], ...)
      })
    } else {
      results <- lapply(x, fun, ...)
    }
  }
  
  results
}

#' Parallel computation of bibliometric metrics
#'
#' Specialized function for computing metrics across multiple entities
#' (countries, authors, sources) in parallel.
#'
#' @param entities List of entity data (e.g., list of country data frames)
#' @param metric_fn Function to compute metric for each entity
#' @param ... Additional arguments to metric_fn
#' @return Named list of metrics
#' @export
#' @examples
#' \dontrun{
#' # Compute metrics for all countries in parallel
#' country_metrics <- parallel_metrics(
#'   country_data_list,
#'   compute_country_production
#' )
#' }
parallel_metrics <- function(entities, metric_fn, ...) {
  # Get entity names
  entity_names <- names(entities)
  if (is.null(entity_names)) {
    entity_names <- seq_along(entities)
  }
  
  # Compute in parallel
  results <- parallel_map(
    entities,
    function(entity) {
      tryCatch(
        metric_fn(entity, ...),
        error = function(e) {
          log_message("WARN", "Failed to compute metric: {msg}", 
                     msg = conditionMessage(e))
          NULL
        }
      )
    },
    progress = TRUE
  )
  
  # Name results
  names(results) <- entity_names
  results
}

#' Parallel apply for data frames
#'
#' Split-apply-combine pattern for data frames with parallel processing
#'
#' @param df Data frame
#' @param by Column name(s) to split by
#' @param fun Function to apply to each group
#' @param ... Additional arguments to fun
#' @return Combined results
#' @export
#' @examples
#' \dontrun{
#' # Compute statistics by country in parallel
#' results <- parallel_by(
#'   bib_data,
#'   by = "AU_CO",
#'   function(group) {
#'     list(n = nrow(group), cites = sum(group$TC, na.rm = TRUE))
#'   }
#' )
#' }
parallel_by <- function(df, by, fun, ...) {
  if (!is.data.frame(df)) {
    stop("Input must be a data frame")
  }
  
  # Split data
  if (length(by) == 1) {
    groups <- split(df, df[[by]])
  } else {
    # Multiple grouping columns
    group_id <- interaction(df[, by, drop = FALSE], drop = TRUE)
    groups <- split(df, group_id)
  }
  
  # Apply function in parallel
  results <- parallel_map(groups, fun, ..., progress = TRUE)
  
  # Combine results if they're data frames
  if (all(sapply(results, is.data.frame))) {
    do.call(rbind, results)
  } else {
    results
  }
}

#' Estimate memory requirements
#'
#' Estimates memory requirements for parallel processing
#'
#' @param n_items Number of items to process
#' @param item_size_mb Estimated size per item in MB
#' @param n_cores Number of cores
#' @return Estimated memory in MB
#' @export
estimate_memory <- function(n_items, item_size_mb, n_cores = NULL) {
  if (is.null(n_cores)) {
    n_cores <- get_n_cores()
  }
  
  # Base memory + per-item overhead
  base_memory <- 500  # MB for R overhead
  parallel_overhead <- n_cores * 200  # MB per parallel worker
  data_memory <- n_items * item_size_mb
  
  total_mb <- base_memory + parallel_overhead + data_memory
  
  log_message("DEBUG", 
             "Estimated memory: {total} MB (base: {base}, workers: {workers}, data: {data})",
             total = round(total_mb),
             base = base_memory,
             workers = parallel_overhead,
             data = round(data_memory))
  
  total_mb
}

#' Check if sufficient memory available
#'
#' @param required_mb Required memory in MB
#' @param safety_factor Multiplier for safety margin (default: 1.5)
#' @return TRUE if sufficient memory, FALSE otherwise
#' @export
check_memory <- function(required_mb, safety_factor = 1.5) {
  # Try to get available memory
  available_mb <- tryCatch({
    if (.Platform$OS.type == "windows") {
      # Windows
      mem <- system("wmic ComputerSystem get TotalPhysicalMemory", intern = TRUE)
      as.numeric(mem[2]) / 1024 / 1024
    } else {
      # Unix/Linux/Mac
      mem <- system("free -m | grep Mem | awk '{print $7}'", intern = TRUE)
      as.numeric(mem[1])
    }
  }, error = function(e) {
    # Can't determine - assume sufficient
    Inf
  })
  
  required_with_safety <- required_mb * safety_factor
  
  if (available_mb < required_with_safety) {
    warning(sprintf(
      "Insufficient memory: %.0f MB available, %.0f MB required (with %.1fx safety factor)",
      available_mb, required_with_safety, safety_factor
    ))
    return(FALSE)
  }
  
  TRUE
}