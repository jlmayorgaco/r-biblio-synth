# ============================================================================
# cache.R - Intelligent Caching System for RBiblioSynth
# ============================================================================
# Provides disk-based caching with TTL, compression, and automatic cleanup

#' Cache computation result
#'
#' Caches the result of an expression to disk, avoiding recomputation if
#' the cache is still valid. Uses SHA-256 hashing for cache keys.
#'
#' @param key Cache key (string) or expression to hash
#' @param expr Expression to evaluate and cache
#' @param ttl Time-to-live in seconds (default: 1 hour). Use Inf for no expiration.
#' @param dir Cache directory (default: "cache" in project root)
#' @param compress Compression method: "none", "gzip", "xz", "qs" (requires qs package)
#' @return Result of expr (either cached or freshly computed)
#' @export
#' @examples
#' \dontrun{
#' # Cache expensive computation
#' result <- cache_compute("expensive_analysis", {
#'   run_m1(bib_data)
#' }, ttl = 3600)
#'
#' # Cache with automatic key generation from expression
#' result <- cache_compute(quote(run_m2(annual_data)), ttl = 1800)
#' }
cache_compute <- function(key, expr, ttl = 3600, dir = "cache", 
                          compress = "gzip") {
  # Ensure cache directory exists
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Generate cache key if expression provided
  if (is.call(key) || is.name(key)) {
    key <- generate_cache_key(key)
  }
  
  # Sanitize key for filename
  key <- sanitize_cache_key(key)
  cache_file <- file.path(dir, paste0(key, ".cache"))
  meta_file <- file.path(dir, paste0(key, ".meta"))
  
  # Check if valid cache exists
  if (is_cache_valid(cache_file, meta_file, ttl)) {
    log_message("DEBUG", "Cache hit for key: {key}", key = key,
                context = list(module = "cache", operation = "read"))
    return(read_cache(cache_file, compress))
  }
  
  # Compute result
  log_message("DEBUG", "Cache miss for key: {key}, computing...", key = key,
              context = list(module = "cache", operation = "compute"))
  
  result <- force(expr)
  
  # Save to cache
  write_cache(result, cache_file, meta_file, compress)
  
  result
}

#' Generate cache key from expression
#' @keywords internal
generate_cache_key <- function(expr) {
  # Create hash of deparsed expression
  expr_text <- paste(deparse(expr), collapse = "")
  digest::digest(expr_text, algo = "sha256", serialize = FALSE)
}

#' Sanitize cache key for filesystem
#' @keywords internal
sanitize_cache_key <- function(key) {
  # Replace problematic characters
  key <- gsub("[^a-zA-Z0-9_-]", "_", key)
  # Limit length
  if (nchar(key) > 100) {
    key <- paste0(substr(key, 1, 50), "_", digest::digest(key, algo = "sha256", serialize = FALSE))
  }
  key
}

#' Check if cache is valid
#' @keywords internal
is_cache_valid <- function(cache_file, meta_file, ttl) {
  # Check if files exist
  if (!file.exists(cache_file) || !file.exists(meta_file)) {
    return(FALSE)
  }
  
  # Check TTL
  if (is.infinite(ttl)) {
    return(TRUE)
  }
  
  # Read metadata
  meta <- tryCatch(
    jsonlite::fromJSON(readLines(meta_file, warn = FALSE)),
    error = function(e) NULL
  )
  
  if (is.null(meta)) {
    return(FALSE)
  }
  
  # Check if expired
  cache_time <- as.POSIXct(meta$timestamp, format = "%Y-%m-%d %H:%M:%S")
  age <- as.numeric(difftime(Sys.time(), cache_time, units = "secs"))
  
  age < ttl
}

#' Read cache from disk
#' @keywords internal
read_cache <- function(cache_file, compress) {
  if (compress == "qs") {
    if (!requireNamespace("qs", quietly = TRUE)) {
      warning("qs package not available, falling back to RDS")
      compress <- "gzip"
    } else {
      return(qs::qread(cache_file))
    }
  }
  
  if (compress %in% c("gzip", "xz", "bz2")) {
    con <- gzfile(cache_file, "rb")
    on.exit(close(con))
    return(readRDS(con))
  }
  
  # No compression
  readRDS(cache_file)
}

#' Write cache to disk
#' @keywords internal
write_cache <- function(result, cache_file, meta_file, compress) {
  # Write data
  if (compress == "qs") {
    if (requireNamespace("qs", quietly = TRUE)) {
      qs::qsave(result, cache_file)
    } else {
      saveRDS(result, file = cache_file, compress = "gzip")
    }
  } else if (compress %in% c("gzip", "xz", "bz2")) {
    con <- gzfile(cache_file, "wb")
    on.exit(close(con))
    saveRDS(result, file = con)
  } else {
    saveRDS(result, file = cache_file)
  }
  
  # Write metadata
  meta <- list(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    r_version = R.version.string,
    package_version = as.character(utils::packageVersion("RBiblioSynth")),
    compress = compress
  )
  writeLines(jsonlite::toJSON(meta, auto_unbox = TRUE), meta_file)
}

#' Clear cache directory
#'
#' @param dir Cache directory
#' @param pattern Optional pattern to match keys (regex)
#' @param older_than Remove cache older than this many seconds (NULL = all)
#' @return Number of files removed
#' @export
clear_cache <- function(dir = "cache", pattern = NULL, older_than = NULL) {
  if (!dir.exists(dir)) {
    return(0)
  }
  
  # List cache files
  files <- list.files(dir, pattern = "\\.cache$", full.names = TRUE)
  meta_files <- list.files(dir, pattern = "\\.meta$", full.names = TRUE)
  
  # Apply pattern filter
  if (!is.null(pattern)) {
    basenames <- basename(files)
    matches <- grepl(pattern, basenames)
    files <- files[matches]
    meta_files <- meta_files[matches]
  }
  
  # Apply age filter
  if (!is.null(older_than)) {
    file_info <- file.info(files)
    age <- as.numeric(difftime(Sys.time(), file_info$mtime, units = "secs"))
    files <- files[age > older_than]
    meta_files <- meta_files[basename(meta_files) %in% basename(files)]
  }
  
  # Remove files
  removed <- 0
  for (f in c(files, meta_files)) {
    if (file.exists(f)) {
      unlink(f)
      removed <- removed + 1
    }
  }
  
  log_message("INFO", "Cleared {n} cache files from {dir}", 
              n = removed, dir = dir,
              context = list(module = "cache", operation = "clear"))
  
  removed
}

#' Get cache statistics
#'
#' @param dir Cache directory
#' @return List with cache statistics
#' @export
cache_stats <- function(dir = "cache") {
  if (!dir.exists(dir)) {
    return(list(
      n_files = 0,
      total_size = 0,
      oldest = NULL,
      newest = NULL
    ))
  }
  
  files <- list.files(dir, pattern = "\\.cache$", full.names = TRUE)
  
  if (length(files) == 0) {
    return(list(
      n_files = 0,
      total_size = 0,
      oldest = NULL,
      newest = NULL
    ))
  }
  
  file_info <- file.info(files)
  sizes <- file_info$size
  times <- file_info$mtime
  
  list(
    n_files = length(files),
    total_size = sum(sizes),
    total_size_mb = round(sum(sizes) / 1024 / 1024, 2),
    oldest = min(times),
    newest = max(times),
    avg_file_size = round(mean(sizes) / 1024, 2)  # KB
  )
}

#' Auto-cleanup old cache files
#'
#' Removes cache files older than max_age
#'
#' @param dir Cache directory
#' @param max_age Maximum age in seconds (default: 7 days)
#' @param max_size Maximum total size in MB (default: 1000)
#' @return Number of files removed
#' @export
auto_cleanup_cache <- function(dir = "cache", max_age = 7 * 24 * 3600, 
                               max_size = 1000) {
  stats <- cache_stats(dir)
  
  removed <- 0
  
  # Remove by age
  if (!is.null(stats$oldest)) {
    age <- as.numeric(difftime(Sys.time(), stats$oldest, units = "secs"))
    if (age > max_age) {
      removed <- removed + clear_cache(dir, older_than = max_age)
    }
  }
  
  # Remove by size
  if (stats$total_size_mb > max_size) {
    # Get files sorted by age (oldest first)
    files <- list.files(dir, pattern = "\\.cache$", full.names = TRUE)
    file_info <- file.info(files)
    files <- files[order(file_info$mtime)]
    
    # Remove oldest until under limit
    current_size <- stats$total_size_mb
    for (f in files) {
      if (current_size <= max_size * 0.8) break  # Target 80% of limit
      
      f_size <- file.info(f)$size / 1024 / 1024
      meta_file <- sub("\\.cache$", ".meta", f)
      
      unlink(f)
      if (file.exists(meta_file)) unlink(meta_file)
      
      current_size <- current_size - f_size
      removed <- removed + 1
    }
  }
  
  if (removed > 0) {
    log_message("INFO", "Auto-cleaned {n} cache files", n = removed,
                context = list(module = "cache", operation = "auto_cleanup"))
  }
  
  removed
}

#' Invalidate specific cache key
#'
#' @param key Cache key
#' @param dir Cache directory
#' @return TRUE if cache existed and was removed
#' @export
invalidate_cache <- function(key, dir = "cache") {
  key <- sanitize_cache_key(key)
  cache_file <- file.path(dir, paste0(key, ".cache"))
  meta_file <- file.path(dir, paste0(key, ".meta"))
  
  existed <- file.exists(cache_file)
  
  if (existed) {
    unlink(cache_file)
    unlink(meta_file)
    log_message("DEBUG", "Invalidated cache key: {key}", key = key,
                context = list(module = "cache", operation = "invalidate"))
  }
  
  existed
}