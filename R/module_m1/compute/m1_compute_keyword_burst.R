# ============================================================================
# m1_compute_keyword_burst.R - Kleinberg Burst Detection for Keywords
# ============================================================================
# Implements Kleinberg's burst detection algorithm to identify
# keywords that suddenly increase in frequency over time.

#' Compute keyword burst detection using Kleinberg's algorithm
#'
#' @param input Bibliographic data frame
#' @param config Configuration list
#' @return List with burst detection results
#' @export
compute_m1_keyword_burst <- function(input, config = biblio_config()) {
  validate_is_data_frame(input)
  
  # Find keyword column
  kw_col <- NULL
  for (col in c("KW", "DE", "ID", "Keywords", "KW_Merged")) {
    if (col %in% names(input)) {
      kw_col <- col
      break
    }
  }
  
  if (is.null(kw_col)) {
    return(list(
      bursts = data.frame(),
      keyword_bursts = list(),
      summary = list(),
      status = "error: no keyword column found"
    ))
  }
  
  # Find year column
  year_col <- NULL
  for (col in c("PY", "Year", "PublicationYear")) {
    if (col %in% names(input)) {
      year_col <- col
      break
    }
  }
  
  if (is.null(year_col)) {
    return(list(
      bursts = data.frame(),
      keyword_bursts = list(),
      summary = list(),
      status = "error: no year column found"
    ))
  }
  
  # Extract keyword-year pairs
  kw_year <- extract_keyword_year_pairs(input, kw_col, year_col)
  
  if (nrow(kw_year) == 0) {
    return(list(
      bursts = data.frame(),
      keyword_bursts = list(),
      summary = list(),
      status = "error: no keyword-year pairs found"
    ))
  }
  
  # Get time range
  years <- sort(unique(kw_year$year))
  year_range <- range(years)
  
  # Compute keyword frequencies by year
  kw_freq_by_year <- compute_keyword_frequencies(kw_year, years)
  
  # Find all keywords
  all_keywords <- unique(kw_year$keyword)
  
  # Detect bursts for each keyword
  burst_results <- list()
  burst_list <- list()
  
  s <- config$burst_s %||% 2.0
  
  for (kw in all_keywords) {
    if (nchar(kw) < 2) next
    
    kw_freq <- kw_freq_by_year[kw_freq_by_year$keyword == kw, ]
    
    if (nrow(kw_freq) == 0) next
    
    # Create frequency vector for all years
    freq_vec <- integer(length(years))
    names(freq_vec) <- years
    freq_vec[as.character(kw_freq$year)] <- kw_freq$frequency
    
    # Detect bursts
    bursts <- kleinberg_burst_detection(freq_vec, years, s = s)
    
    if (length(bursts$bursts) > 0) {
      burst_results[[kw]] <- list(
        keyword = kw,
        frequencies = freq_vec,
        bursts = bursts$bursts,
        burst_intervals = bursts$intervals,
        max_height = bursts$max_height,
        weight = bursts$weight
      )
    }
  }
  
  # Create summary data frame
  burst_summary <- create_burst_summary(burst_results, years)
  
  # Find burst timing categories
  timing_cat <- categorize_burst_timing(burst_results, years)
  
  list(
    bursts = burst_summary,
    keyword_bursts = burst_results,
    years = years,
    year_range = year_range,
    all_keywords = all_keywords,
    timing_categories = timing_cat,
    summary = list(
      n_keywords = length(all_keywords),
      n_bursty_keywords = length(burst_results),
      n_recent_bursts = sum(sapply(burst_results, function(x) {
        if (length(x$bursts) == 0) return(FALSE)
        any(sapply(x$bursts, function(b) b$start_year >= max(years) - 3))
      })),
      max_burst_height = if (length(burst_results) > 0) max(sapply(burst_results, function(x) x$max_height), na.rm = TRUE) else 0,
      parameters = list(s = s)
    ),
    status = "success"
  )
}

#' Extract keyword-year pairs from data frame
#' @keywords internal
extract_keyword_year_pairs <- function(input, kw_col, year_col) {
  kw_year <- data.frame(
    keyword = character(),
    year = integer(),
    doc_id = integer(),
    stringsAsFactors = FALSE
  )
  
  years <- as.integer(input[[year_col]])
  keywords <- as.character(input[[kw_col]])
  doc_ids <- seq_len(nrow(input))
  
  for (i in seq_along(keywords)) {
    if (is.na(keywords[i]) || keywords[i] == "") next
    if (is.na(years[i])) next
    
    # Split keywords
    kw_list <- strsplit(keywords[i], ";")[[1]]
    kw_list <- trimws(kw_list)
    kw_list <- kw_list[kw_list != ""]
    
    for (kw in kw_list) {
      kw_year <- rbind(kw_year, data.frame(
        keyword = tolower(kw),
        year = years[i],
        doc_id = doc_ids[i],
        stringsAsFactors = FALSE
      ))
    }
  }
  
  kw_year
}

#' Compute keyword frequencies by year
#' @keywords internal
compute_keyword_frequencies <- function(kw_year, years) {
  result <- aggregate(
    doc_id ~ keyword + year,
    data = kw_year,
    FUN = function(x) length(unique(x))
  )
  names(result)[3] <- "frequency"
  result
}

#' Kleinberg burst detection algorithm
#' 
#' Implements the 2-state burst detection algorithm.
#' @param frequencies Frequency vector (named by year)
#' @param years Year vector
#' @param s Scaling parameter (default 2.0)
#' @return List with bursts and intervals
#' @keywords internal
kleinberg_burst_detection <- function(frequencies, years, s = 2.0) {
  n <- length(frequencies)
  if (n < 3) {
    return(list(bursts = list(), intervals = list(), max_height = 0, weight = 0))
  }
  
  # Total frequency
  total_freq <- sum(frequencies)
  if (total_freq == 0) {
    return(list(bursts = list(), intervals = list(), max_height = 0, weight = 0))
  }
  
  # Expected frequency per year (baseline)
  expected_freq <- total_freq / n
  
  # Two-state model: state 0 (normal), state 1 (burst)
  # Cost of transitioning from state i to j
  tau <- log(s)
  
  # Cost function for each state at each time
  costs <- matrix(Inf, nrow = n, ncol = 2)
  costs[1, 1] <- 0  # Start in normal state
  costs[1, 2] <- tau  # Cost to start in burst state
  
  # Compute emission costs
  for (t in 1:n) {
    f <- frequencies[t]
    
    # State 0: normal (use expected frequency)
    lambda0 <- max(expected_freq, 1e-10)
    cost0 <- -stats::dpois(f, lambda0, log = TRUE)
    
    # State 1: burst (higher expected frequency)
    lambda1 <- max(expected_freq * s, 1e-10)
    cost1 <- -stats::dpois(f, lambda1, log = TRUE)
    
    if (t == 1) {
      costs[t, 1] <- cost0
      costs[t, 2] <- cost1 + tau
    } else {
      # Transition costs
      costs[t, 1] <- min(
        costs[t-1, 1] + cost0,
        costs[t-1, 2] + tau + cost0
      )
      costs[t, 2] <- min(
        costs[t-1, 1] + tau + cost1,
        costs[t-1, 2] + cost1
      )
    }
  }
  
  # Backtrack to find state sequence
  states <- integer(n)
  states[n] <- which.min(costs[n, ]) - 1
  
  for (t in (n-1):1) {
    if (states[t+1] == 0) {
      opts <- c(costs[t, 1], costs[t, 2] + tau)
    } else {
      opts <- c(costs[t, 1] + tau, costs[t, 2])
    }
    wm <- which.min(opts)
    if (length(wm) == 0) wm <- 1
    states[t] <- wm - 1
  }
  
  # Find burst intervals
  bursts <- list()
  intervals <- list()
  in_burst <- FALSE
  burst_start <- NULL
  
  for (t in 1:n) {
    if (states[t] == 1 && !in_burst) {
      in_burst <- TRUE
      burst_start <- t
    } else if (states[t] == 0 && in_burst) {
      in_burst <- FALSE
      burst_end <- t - 1
      
      # Calculate burst weight
      burst_freq <- sum(frequencies[burst_start:burst_end])
      expected_burst_freq <- expected_freq * (burst_end - burst_start + 1)
      burst_weight <- burst_freq / expected_burst_freq
      
      bursts[[length(bursts) + 1]] <- list(
        start_index = burst_start,
        end_index = burst_end,
        start_year = years[burst_start],
        end_year = years[burst_end],
        duration = burst_end - burst_start + 1,
        total_freq = burst_freq,
        strength = burst_weight,
        height = log(burst_weight)
      )
    }
  }
  
  # Handle case where burst continues to end
  if (in_burst) {
    burst_end <- n
    burst_freq <- sum(frequencies[burst_start:burst_end])
    expected_burst_freq <- expected_freq * (burst_end - burst_start + 1)
    burst_weight <- burst_freq / expected_burst_freq
    
    bursts[[length(bursts) + 1]] <- list(
      start_index = burst_start,
      end_index = burst_end,
      start_year = years[burst_start],
      end_year = years[burst_end],
      duration = burst_end - burst_start + 1,
      total_freq = burst_freq,
      strength = burst_weight,
      height = log(burst_weight)
    )
  }
  
  max_height <- if (length(bursts) > 0) {
    max(sapply(bursts, function(b) b$height), na.rm = TRUE)
  } else {
    0
  }
  
  # Compute interval boundaries
  for (i in seq_along(bursts)) {
    intervals[[i]] <- c(
      years[bursts[[i]]$start_index],
      years[bursts[[i]]$end_index]
    )
  }
  
  list(
    bursts = bursts,
    intervals = intervals,
    states = states,
    max_height = max_height,
    weight = if (length(bursts) > 0) sum(sapply(bursts, function(b) b$strength)) else 0
  )
}

#' Create summary data frame of bursts
#' @keywords internal
create_burst_summary <- function(burst_results, years) {
  if (length(burst_results) == 0) {
    return(data.frame(
      keyword = character(),
      start_year = integer(),
      end_year = integer(),
      duration = integer(),
      strength = numeric(),
      height = numeric(),
      total_freq = integer(),
      stringsAsFactors = FALSE
    ))
  }
  
  summary_list <- lapply(names(burst_results), function(kw) {
    x <- burst_results[[kw]]
    if (length(x$bursts) == 0) return(NULL)
    
    do.call(rbind, lapply(x$bursts, function(b) {
      data.frame(
        keyword = kw,
        start_year = b$start_year,
        end_year = b$end_year,
        duration = b$duration,
        strength = b$strength,
        height = b$height,
        total_freq = b$total_freq,
        stringsAsFactors = FALSE
      )
    }))
  })
  
  summary_df <- do.call(rbind, summary_list[!sapply(summary_list, is.null)])
  
  if (is.null(summary_df) || nrow(summary_df) == 0) {
    return(data.frame(
      keyword = character(),
      start_year = integer(),
      end_year = integer(),
      duration = integer(),
      strength = numeric(),
      height = numeric(),
      total_freq = integer(),
      stringsAsFactors = FALSE
    ))
  }
  
  # Sort by strength
  summary_df <- summary_df[order(-summary_df$strength), ]
  rownames(summary_df) <- NULL
  
  summary_df
}

#' Categorize burst timing
#' @keywords internal
categorize_burst_timing <- function(burst_results, years) {
  recent_threshold <- max(years) - 3
  mid_threshold <- max(years) - 6
  
  categories <- list(
    emerging = character(),
    growing = character(),
    declining = character(),
    steady = character(),
    bursty = character()
  )
  
  for (kw in names(burst_results)) {
    x <- burst_results[[kw]]
    if (length(x$bursts) == 0) next
    
    # Get burst years
    burst_years <- sapply(x$bursts, function(b) b$start_year)
    
    # Check if recent burst
    if (any(burst_years >= recent_threshold)) {
      categories$emerging <- c(categories$emerging, kw)
    } else if (any(burst_years >= mid_threshold)) {
      categories$growing <- c(categories$growing, kw)
    } else {
      categories$declining <- c(categories$declining, kw)
    }
    
    # Check if bursty (multiple bursts)
    if (length(x$bursts) > 1) {
      categories$bursty <- c(categories$bursty, kw)
    }
  }
  
  # Summary
  categories$summary <- list(
    n_emerging = length(categories$emerging),
    n_growing = length(categories$growing),
    n_declining = length(categories$declining),
    n_bursty = length(categories$bursty)
  )
  
  categories
}

#' dpois with small lambda handling
#' @keywords internal
dpois <- function(x, lambda) {
  if (lambda < 1e-10) {
    if (x == 0) return(1) else return(1e-10)
  }
  stats::dpois(x, lambda)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b