# ============================================================================
# m2_compute_changepoint.R - Change-Point Detection
# ============================================================================
# Implements PELT (Pruned Exact Linear Time) algorithm for detecting
# structural changes in time series data.
#
# IEEE Q1 Enhancement: Multiple change-point detection with statistical
# significance testing and automatic segment characterization.

#' Compute change-points in time series using PELT algorithm
#'
#' Identifies structural breaks in publication trends using multiple
#' detection methods: PELT, Binary Segmentation, and cumulative sums.
#'
#' @param data Data frame with Year and Articles columns
#' @param config Configuration list
#' @return List with change-point analysis results
#' @export
compute_m2_changepoint <- function(data, config = biblio_config()) {
  validate_is_data_frame(data)
  
  required_cols <- c("Year", "Articles")
  validation <- validate_required_columns(data, required_cols)
  if (!validation$ok) {
    return(list(
      changepoints = list(),
      segments = data.frame(),
      summary = list(),
      status = paste("error: missing columns:", paste(validation$missing_columns, collapse = ", "))
    ))
  }
  
  if (nrow(data) < 10) {
    return(list(
      changepoints = list(),
      segments = data.frame(),
      summary = list(),
      status = "error: insufficient data points (need at least 10)"
    ))
  }
  
  year <- data$Year
  articles <- data$Articles
  
  pelt_result <- detect_changepoints_pelt(year, articles)
  
  cusum_result <- detect_changepoints_cusum(year, articles)
  
  binseg_result <- detect_changepoints_binseg(year, articles)
  
  consensus_cps <- find_consensus_changepoints(
    pelt_result$changepoints,
    cusum_result$changepoints,
    binseg_result$changepoints,
    tolerance = 2
  )
  
  segments <- characterize_segments(year, articles, consensus_cps$changepoints)
  
  n_cp <- length(consensus_cps$changepoints)
  n_obs <- length(articles)
  
  summary_stats <- list(
    n_changepoints = n_cp,
    changepoint_years = if (n_cp > 0) year[consensus_cps$changepoints] else integer(0),
    n_segments = n_cp + 1,
    n_observations = n_obs,
    method = "PELT + CUSUM + Binary Segmentation (consensus)",
    pelt_changepoints = length(pelt_result$changepoints),
    cusum_changepoints = length(cusum_result$changepoints),
    binseg_changepoints = length(binseg_result$changepoints),
    agreement_rate = consensus_cps$agreement_rate
  )
  
  list(
    changepoints = list(
      consensus = if (n_cp > 0) year[consensus_cps$changepoints] else integer(0),
      pelt = if (length(pelt_result$changepoints) > 0) year[pelt_result$changepoints] else integer(0),
      cusum = if (length(cusum_result$changepoints) > 0) year[cusum_result$changepoints] else integer(0),
      binseg = if (length(binseg_result$changepoints) > 0) year[binseg_result$changepoints] else integer(0)
    ),
    segments = segments,
    pelt_result = pelt_result,
    cusum_result = cusum_result,
    binseg_result = binseg_result,
    summary = summary_stats,
    status = "success"
  )
}

#' PELT Change-Point Detection
#' @keywords internal
detect_changepoints_pelt <- function(year, data, penalty = "BIC") {
  n <- length(data)
  
  calculate_penalty <- function(n, penalty_type = "BIC") {
    switch(penalty_type,
           "BIC" = log(n),
           "AIC" = 2,
           "MBIC" = log(n) + 2 * log(log(n)),
           "Hannan-Quinn" = 2 * log(log(n)),
           log(n))
  }
  
  beta <- calculate_penalty(n, penalty)
  
  cp <- pelt_algorithm(data, beta)
  
  list(
    changepoints = cp,
    cost = pelt_cost(data, cp),
    penalty = beta,
    method = "PELT"
  )
}

#' PELT Algorithm Implementation
#' @keywords internal
pelt_algorithm <- function(data, beta) {
  n <- length(data)
  
  if (n < 3) return(integer(0))
  
  F <- rep(Inf, n + 1)
  F[1] <- -beta
  
  cp_loc <- vector("list", n + 1)
  
  neg_ll <- function(x) {
    if (length(x) <= 1) return(0)
    mu <- mean(x, na.rm = TRUE)
    sigma <- sd(x, na.rm = TRUE)
    if (is.na(sigma) || sigma == 0) sigma <- 1
    sum(dnorm(x, mean = mu, sd = sigma, log = TRUE), na.rm = TRUE)
  }
  
  segment_cost <- function(start, end) {
    -neg_ll(data[start:end])
  }
  
  R <- 1
  
  for (t in 2:(n + 1)) {
    candidates <- R
    
    F[t] <- Inf
    
    for (tau in candidates) {
      cost <- F[tau] + segment_cost(tau, t - 1) + beta
      if (cost < F[t]) {
        F[t] <- cost
        cp_loc[[t]] <- c(cp_loc[[tau]], tau - 1)
      }
    }
    
    if (!is.infinite(F[t])) {
      threshold <- F[t] - beta
      for (s in candidates) {
        if (F[s] > threshold) {
          R <- setdiff(R, s)
        }
      }
    }
    
    R <- c(R, t)
  }
  
  sort(unique(cp_loc[[n + 1]][cp_loc[[n + 1]] > 0]))
}

#' Calculate PELT cost
#' @keywords internal
pelt_cost <- function(data, changepoints) {
  if (length(changepoints) == 0) {
    n <- length(data)
    mu <- mean(data, na.rm = TRUE)
    sigma <- sd(data, na.rm = TRUE)
    if (is.na(sigma) || sigma == 0) return(0)
    -sum(dnorm(data, mean = mu, sd = sigma, log = TRUE))
  } else {
    total_cost <- 0
    start <- 1
    for (cp in sort(changepoints)) {
      segment <- data[start:cp]
      if (length(segment) > 0) {
        mu <- mean(segment, na.rm = TRUE)
        sigma <- sd(segment, na.rm = TRUE)
        if (is.na(sigma) || sigma == 0) sigma <- 1
        total_cost <- total_cost - sum(dnorm(segment, mean = mu, sd = sigma, log = TRUE), na.rm = TRUE)
      }
      start <- cp + 1
    }
    if (start <= length(data)) {
      segment <- data[start:length(data)]
      if (length(segment) > 0) {
        mu <- mean(segment, na.rm = TRUE)
        sigma <- sd(segment, na.rm = TRUE)
        if (is.na(sigma) || sigma == 0) sigma <- 1
        total_cost <- total_cost - sum(dnorm(segment, mean = mu, sd = sigma, log = TRUE), na.rm = TRUE)
      }
    }
    total_cost
  }
}

#' CUSUM Change-Point Detection
#' @keywords internal
detect_changepoints_cusum <- function(year, data) {
  n <- length(data)
  if (n < 3) return(list(changepoints = integer(0), method = "CUSUM"))
  
  mean_data <- mean(data, na.rm = TRUE)
  cusum <- cumsum(data - mean_data)
  
  cusum_abs <- abs(cusum)
  d_n <- max(cusum_abs, na.rm = TRUE)
  
  s0 <- sqrt(sum((data - mean_data)^2, na.rm = TRUE) / n)
  if (s0 == 0) s0 <- 1
  
  threshold <- s0 * sqrt(n) * 1.143
  
  cp_idx <- which(cusum_abs >= d_n * 0.9)[1]
  
  if (is.na(cp_idx) || d_n < threshold) {
    return(list(
      changepoints = integer(0),
      cusum_max = d_n,
      threshold = d_n,
      method = "CUSUM"
    ))
  }
  
  list(
    changepoints = cp_idx,
    cusum_max = d_n,
    threshold = threshold,
    cusum_values = cusum,
    method = "CUSUM"
  )
}

#' Binary Segmentation Change-Point Detection
#' @keywords internal
detect_changepoints_binseg <- function(year, data, min_segment = 5, max_cp = 5) {
  n <- length(data)
  if (n < min_segment * 2) {
    return(list(changepoints = integer(0), method = "Binary Segmentation"))
  }
  
  changepoints <- integer(0)
  
  find_single_cp <- function(start, end) {
    if (end - start < min_segment * 2) return(NULL)
    
    segment <- data[start:end]
    n_seg <- length(segment)
    
    cumulative_var <- sapply((min_segment + 1):(n_seg - min_segment), function(k) {
      left <- segment[1:k]
      right <- segment[(k + 1):n_seg]
      
      n_left <- length(left)
      n_right <- length(right)
      
      mse_left <- mean((left - mean(left))^2, na.rm = TRUE)
      mse_right <- mean((right - mean(right))^2, na.rm = TRUE)
      
      n_left * log(mse_left + 1) + n_right * log(mse_right + 1)
    })
    
    best_k <- which.min(cumulative_var) + min_segment
    
    list(
      index = start + best_k - 1,
      cost_reduction = cumulative_var[best_k - min_segment]
    )
  }
  
  segments_to_check <- list(c(1, n))
  n_cp_found <- 0
  
  while (length(segments_to_check) > 0 && n_cp_found < max_cp) {
    segment <- segments_to_check[[1]]
    segments_to_check <- segments_to_check[-1]
    
    cp_result <- find_single_cp(segment[1], segment[2])
    
    if (!is.null(cp_result) && !is.null(cp_result$index)) {
      changepoints <- c(changepoints, cp_result$index)
      n_cp_found <- n_cp_found + 1
      
      if (cp_result$index - segment[1] >= min_segment * 2) {
        segments_to_check <- c(segments_to_check, list(c(segment[1], cp_result$index)))
      }
      if (segment[2] - cp_result$index >= min_segment * 2) {
        segments_to_check <- c(segments_to_check, list(c(cp_result$index + 1, segment[2])))
      }
    }
  }
  
  list(
    changepoints = sort(unique(changepoints)),
    method = "Binary Segmentation"
  )
}

#' Find consensus change-points
#' @keywords internal
find_consensus_changepoints <- function(pelt_cp, cusum_cp, binseg_cp, tolerance = 2) {
  all_cps <- sort(unique(c(pelt_cp, cusum_cp, binseg_cp)))
  
  if (length(all_cps) == 0) {
    return(list(changepoints = integer(0), agreement_rate = 1))
  }
  
  consensus <- integer(0)
  
  for (cp in all_cps) {
    nearby_pelt <- any(abs(pelt_cp - cp) <= tolerance)
    nearby_cusum <- any(abs(cusum_cp - cp) <= tolerance)
    nearby_binseg <- any(abs(binseg_cp - cp) <= tolerance)
    
    n_methods <- sum(c(nearby_pelt, nearby_cusum, nearby_binseg))
    
    if (n_methods >= 2) {
      consensus <- c(consensus, cp)
    }
  }
  
  consensus <- sort(unique(consensus))
  
  total_detected <- length(unique(c(pelt_cp, cusum_cp, binseg_cp)))
  agreement_rate <- if (total_detected > 0) {
    length(consensus) / total_detected
  } else {
    1
  }
  
  list(
    changepoints = consensus,
    agreement_rate = agreement_rate
  )
}

#' Characterize segments between change-points
#' @keywords internal
characterize_segments <- function(year, data, changepoints) {
  n <- length(data)
  
  if (length(changepoints) == 0) {
    return(data.frame(
      segment = 1,
      start_year = min(year),
      end_year = max(year),
      n_points = n,
      mean = mean(data, na.rm = TRUE),
      sd = sd(data, na.rm = TRUE),
      trend = compute_trend(year, data)$slope,
      growth_rate = compute_trend(year, data)$slope / mean(data, na.rm = TRUE) * 100
    ))
  }
  
  changepoints <- sort(changepoints)
  
  start_indices <- c(1, changepoints + 1)
  end_indices <- c(changepoints, n)
  
  segments <- lapply(seq_along(start_indices), function(i) {
    start_idx <- start_indices[i]
    end_idx <- end_indices[i]
    
    segment_years <- year[start_idx:end_idx]
    segment_data <- data[start_idx:end_idx]
    
    trend_result <- compute_trend(segment_years, segment_data)
    
    data.frame(
      segment = i,
      start_year = min(segment_years),
      end_year = max(segment_years),
      n_points = length(segment_data),
      mean = mean(segment_data, na.rm = TRUE),
      sd = sd(segment_data, na.rm = TRUE),
      trend = trend_result$slope,
      growth_rate = if (mean(segment_data, na.rm = TRUE) > 0) {
        trend_result$slope / mean(segment_data, na.rm = TRUE) * 100
      } else {
        NA_real_
      }
    )
  })
  
  do.call(rbind, segments)
}

#' Compute linear trend
#' @keywords internal
compute_trend <- function(x, y) {
  if (length(x) < 2) {
    return(list(slope = NA_real_, intercept = NA_real_, r2 = NA_real_))
  }
  
  model <- lm(y ~ x, na.action = na.exclude)
  
  list(
    slope = coef(model)[2],
    intercept = coef(model)[1],
    r2 = summary(model)$r.squared
  )
}