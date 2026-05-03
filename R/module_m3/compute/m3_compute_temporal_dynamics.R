# ============================================================================
# m3_compute_temporal_dynamics.R - Temporal Dynamics Analysis
# ============================================================================
# NELSOP, emergence patterns, rank mobility, share evolution, transition matrices

#' Compute temporal dynamics for country production
#'
#' @param country_year_data Data frame with country, year/columns
#' @param config Configuration list
#' @return List with temporal dynamics analysis
#' @export
m3_compute_temporal_dynamics <- function(country_year_data, config = biblio_config()) {
  # Handle column name variations
  if (!all(c("country", "year", "production") %in% names(country_year_data))) {
    # Try alternative column names
    if ("PY" %in% names(country_year_data) && !("year" %in% names(country_year_data))) {
      country_year_data$year <- country_year_data$PY
    }
    if ("article_count" %in% names(country_year_data) && !("production" %in% names(country_year_data))) {
      country_year_data$production <- country_year_data$article_count
    }
  }
  
  required_cols <- c("country", "year", "production")
  if (!all(required_cols %in% names(country_year_data))) {
    return(list(status = "error: missing required columns (need country, year, production or PY/article_count)"))
  }
  
  years <- sort(unique(country_year_data$year))
  window_n <- m3_temporal_window_size(years)

  results <- list()
  
  # 1. Rank mobility analysis
  results$rank_mobility <- compute_rank_mobility(country_year_data, years = years, window_n = window_n)
  
  # 2. Share evolution
  results$share_evolution <- compute_share_evolution(country_year_data, years = years, window_n = window_n)
  
  # 3. Emergence patterns (new countries)
  results$emergence <- compute_emergence_patterns(country_year_data)
  
  # 4. Transition matrices (Markov chain)
  results$transitions <- compute_transition_matrices(country_year_data)
  
  # 5. NELSOP patterns
  results$nelsop <- compute_nelsop_patterns(country_year_data)
  results$meta <- list(
    window_n = window_n,
    first_window = head(years, window_n),
    last_window = tail(years, window_n)
  )
  
  results$status <- "success"
  results
}

#' Compute rank mobility analysis
#' @keywords internal
compute_rank_mobility <- function(country_year_data, years = sort(unique(country_year_data$year)),
                                  window_n = m3_temporal_window_size(years)) {
  if (length(years) < 3) {
    return(list(status = "error: insufficient years"))
  }
  
  # Compute ranks per year
  rank_matrices <- list()
  
  for (year in years) {
    year_data <- country_year_data[country_year_data$year == year, ]
    year_data <- year_data[order(-year_data$production), ]
    year_data$rank <- seq_len(nrow(year_data))
    rank_matrices[[as.character(year)]] <- year_data[, c("country", "rank", "production")]
  }
  
  # Rank volatility (average absolute rank change)
  volatility <- compute_rank_volatility(rank_matrices)
  
  # Rank persistence (probability of staying in same rank group)
  persistence <- compute_rank_persistence(rank_matrices)
  
  # Kendall's tau rank correlation over time
  rank_correlations <- compute_rank_correlations(rank_matrices)
  
  # Rank changes
  rank_changes <- compute_rank_changes(country_year_data, years = years, window_n = window_n)
  
  list(
    volatility = volatility,
    persistence = persistence,
    rank_correlations = rank_correlations,
    rank_changes = rank_changes,
    n_years = length(years),
    window_n = window_n,
    first_window = head(years, window_n),
    last_window = tail(years, window_n),
    status = "success"
  )
}

#' Compute rank volatility
#' @keywords internal
compute_rank_volatility <- function(rank_matrices) {
  year_names <- names(rank_matrices)
  n_years <- length(year_names)
  
  volatility <- list()
  
  for (i in 2:n_years) {
    year_prev <- rank_matrices[[year_names[i - 1]]]
    year_curr <- rank_matrices[[year_names[i]]]
    
    # Match countries
    common_countries <- intersect(year_prev$country, year_curr$country)
    
    if (length(common_countries) > 0) {
      rank_prev <- year_prev$rank[match(common_countries, year_prev$country)]
      rank_curr <- year_curr$rank[match(common_countries, year_curr$country)]
      
      abs_change <- abs(rank_curr - rank_prev)
      
      volatility[[paste0(year_names[i - 1], "_to_", year_names[i])]] <- list(
        mean_change = mean(abs_change),
        max_change = max(abs_change),
        median_change = median(abs_change),
        n_countries = length(common_countries)
      )
    }
  }
  
  if (length(volatility) == 0) {
    return(list(
      by_period = list(),
      overall = NA_real_,
      interpretation = "Insufficient overlap to estimate mobility"
    ))
  }

  # Overall volatility
  mean_volatility <- mean(vapply(volatility, function(x) x$mean_change %||% NA_real_, numeric(1)), na.rm = TRUE)
  if (!is.finite(mean_volatility)) {
    return(list(
      by_period = volatility,
      overall = NA_real_,
      interpretation = "Insufficient overlap to estimate mobility"
    ))
  }
  
  list(
    by_period = volatility,
    overall = mean_volatility,
    interpretation = if (mean_volatility > 3) "High mobility" else if (mean_volatility > 1) "Moderate mobility" else "Low mobility"
  )
}

#' Compute rank persistence
#' @keywords internal
compute_rank_persistence <- function(rank_matrices) {
  year_names <- names(rank_matrices)
  n_years <- length(year_names)
  
  if (n_years < 2) return(list(status = "insufficient years"))
  
  # Define rank groups (top 10%, middle, bottom)
  persistence <- list()
  
  for (i in 2:n_years) {
    year_prev <- rank_matrices[[year_names[i - 1]]]
    year_curr <- rank_matrices[[year_names[i]]]
    
    n_prev <- nrow(year_prev)
    
    # Classify into groups
    year_prev$group <- cut(year_prev$rank,
                           breaks = c(0, n_prev * 0.2, n_prev * 0.8, n_prev),
                           labels = c("top", "middle", "bottom"))
    
    year_curr$group <- cut(year_curr$rank,
                           breaks = c(0, nrow(year_curr) * 0.2, nrow(year_curr) * 0.8, nrow(year_curr)),
                           labels = c("top", "middle", "bottom"))
    
    # Match
    common <- intersect(year_prev$country, year_curr$country)
    
    if (length(common) > 0) {
      groups_prev <- year_prev$group[match(common, year_prev$country)]
      groups_curr <- year_curr$group[match(common, year_curr$country)]
      
      persistence_rate <- mean(groups_prev == groups_curr, na.rm = TRUE)
      
      # Movement between groups
      transitions <- table(groups_prev, groups_curr)
      
      persistence[[paste0(year_names[i - 1], "_to_", year_names[i])]] <- list(
        persistence_rate = persistence_rate,
        transitions = transitions
      )
    }
  }
  
  persistence
}

#' Compute rank correlations
#' @keywords internal
compute_rank_correlations <- function(rank_matrices) {
  year_names <- names(rank_matrices)
  n_years <- length(year_names)
  
  correlations <- numeric(n_years - 1)
  lags <- numeric(n_years - 1)
  
  for (i in 2:n_years) {
    year_prev <- rank_matrices[[year_names[i - 1]]]
    year_curr <- rank_matrices[[year_names[i]]]
    
    common <- intersect(year_prev$country, year_curr$country)
    
    if (length(common) >= 5) {
      rank_prev <- year_prev$rank[match(common, year_prev$country)]
      rank_curr <- year_curr$rank[match(common, year_curr$country)]
      
      correlations[i - 1] <- cor(rank_prev, rank_curr, method = "kendall")
      lags[i - 1] <- as.numeric(year_names[i]) - as.numeric(year_names[i - 1])
    }
  }
  
  list(
    correlations = correlations,
    mean_correlation = mean(correlations, na.rm = TRUE),
    lags = lags
  )
}

#' Compute rank changes
#' @keywords internal
compute_rank_changes <- function(country_year_data, years = sort(unique(country_year_data$year)),
                                 window_n = m3_temporal_window_size(years)) {
  if (length(years) == 0) {
    return(list(status = "no country-year data"))
  }

  first_window <- head(years, window_n)
  last_window <- tail(years, window_n)

  first_avg <- m3_average_window_production(country_year_data, first_window)
  last_avg <- m3_average_window_production(country_year_data, last_window)
  all_countries <- union(first_avg$country, last_avg$country)

  if (length(all_countries) < 2) {
    return(list(status = "insufficient countries across windows"))
  }

  changes <- data.frame(
    country = all_countries,
    first_window_output = first_avg$avg_production[match(all_countries, first_avg$country)],
    last_window_output = last_avg$avg_production[match(all_countries, last_avg$country)],
    stringsAsFactors = FALSE
  )

  changes$first_window_output[is.na(changes$first_window_output)] <- 0
  changes$last_window_output[is.na(changes$last_window_output)] <- 0
  changes <- changes[
    changes$first_window_output > 0 | changes$last_window_output > 0,
    ,
    drop = FALSE
  ]

  changes$rank_first <- rank(-changes$first_window_output, ties.method = "min")
  changes$rank_last <- rank(-changes$last_window_output, ties.method = "min")
  changes$rank_change <- changes$rank_first - changes$rank_last
  changes$direction <- ifelse(
    changes$rank_change > 0, "improved",
    ifelse(changes$rank_change < 0, "declined", "stable")
  )

  most_improved <- head(changes[order(-changes$rank_change, -changes$last_window_output), ], 10)
  most_declined <- head(changes[order(changes$rank_change, -changes$first_window_output), ], 10)

  list(
    changes = changes,
    most_improved = most_improved,
    most_declined = most_declined,
    n_improved = sum(changes$rank_change > 0),
    n_declined = sum(changes$rank_change < 0),
    n_stable = sum(changes$rank_change == 0),
    first_window = first_window,
    last_window = last_window,
    window_n = window_n
  )
}

#' Compute share evolution over time
#' @keywords internal
compute_share_evolution <- function(country_year_data, years = sort(unique(country_year_data$year)),
                                    window_n = m3_temporal_window_size(years)) {
  countries <- sort(unique(as.character(country_year_data$country)))
  if (length(years) == 0 || length(countries) == 0) {
    return(list(
      share_matrix = matrix(numeric(0), nrow = 0, ncol = 0),
      share_trends = data.frame(),
      n_years = 0L,
      status = "error: no country-year data"
    ))
  }
  
  # Compute annual shares
  share_matrix <- matrix(NA_real_, length(countries), length(years))
  rownames(share_matrix) <- countries
  colnames(share_matrix) <- as.character(years)
  
  for (i in seq_along(years)) {
    year_data <- country_year_data[country_year_data$year == years[i], ]
    total <- sum(year_data$production, na.rm = TRUE)
    if (!is.finite(total) || total <= 0) {
      next
    }
    
    for (country in unique(as.character(year_data$country))) {
      share_matrix[country, i] <- sum(year_data$production[year_data$country == country], na.rm = TRUE) / total
    }
  }
  
  share_matrix[is.na(share_matrix)] <- 0

  first_window <- head(years, window_n)
  last_window <- tail(years, window_n)
  first_idx <- match(as.character(first_window), colnames(share_matrix))
  last_idx <- match(as.character(last_window), colnames(share_matrix))
  first_idx <- first_idx[!is.na(first_idx)]
  last_idx <- last_idx[!is.na(last_idx)]

  if (length(first_idx) == 0 || length(last_idx) == 0) {
    return(list(
      share_matrix = share_matrix,
      share_trends = data.frame(),
      n_years = length(years),
      status = "success"
    ))
  }

  first_share <- rowMeans(share_matrix[, first_idx, drop = FALSE], na.rm = TRUE)
  last_share <- rowMeans(share_matrix[, last_idx, drop = FALSE], na.rm = TRUE)

  share_trends <- data.frame(
    country = countries,
    first_share = first_share * 100,
    last_share = last_share * 100,
    change = (last_share - first_share) * 100,
    stringsAsFactors = FALSE
  )
  share_trends <- share_trends[order(abs(share_trends$change), decreasing = TRUE), , drop = FALSE]
  share_trends$trend <- ifelse(
    share_trends$change > 0.5, "gaining",
    ifelse(share_trends$change < -0.5, "losing", "stable")
  )
  
  list(
    share_matrix = share_matrix,
    share_trends = share_trends,
    n_years = length(years),
    first_window = first_window,
    last_window = last_window,
    window_n = window_n,
    status = "success"
  )
}

#' Compute emergence patterns
#' @keywords internal
compute_emergence_patterns <- function(country_year_data) {
  years <- sort(unique(country_year_data$year))
  
  if (length(years) < 3) {
    return(list(status = "insufficient years"))
  }
  
  # Find when each country first appears
  first_appearance <- aggregate(year ~ country, data = country_year_data, FUN = min)
  names(first_appearance) <- c("country", "first_year")
  
  # Count new countries per year
  emergence_counts <- table(first_appearance$first_year)
  
  # Identify recent emergers
  recent_threshold <- years[length(years) - 2]
  recent_emergers <- first_appearance$country[first_appearance$first_year >= recent_threshold]
  
  # Growth trajectory of recent emergers
  if (length(recent_emergers) > 0) {
    emergent_growth <- lapply(recent_emergers, function(country) {
      country_data <- country_year_data[country_year_data$country == country, ]
      
      if (nrow(country_data) >= 2) {
        fit <- lm(production ~ year, data = country_data)
        list(
          country = country,
          growth_rate = coef(fit)[2],
          n_years = nrow(country_data),
          total_production = sum(country_data$production)
        )
      }
    })
    
    emergent_growth <- Filter(Negate(is.null), emergent_growth)
  } else {
    emergent_growth <- list()
  }
  
  list(
    first_appearance = first_appearance,
    emergence_counts = emergence_counts,
    recent_emergers = recent_emergers,
    emergent_growth = emergent_growth,
    total_countries = length(unique(country_year_data$country)),
    status = "success"
  )
}

#' Compute transition matrices (Markov chain)
#' @keywords internal
compute_transition_matrices <- function(country_year_data) {
  years <- sort(unique(country_year_data$year))
  
  if (length(years) < 3) {
    return(list(status = "insufficient years"))
  }
  
  # Classify countries into quartiles per year
  n_states <- 4
  state_labels <- paste0("Q", 1:n_states)
  
  transition_matrix <- matrix(0, n_states, n_states)
  rownames(transition_matrix) <- state_labels
  colnames(transition_matrix) <- state_labels
  
  for (i in 2:length(years)) {
    prev_year <- country_year_data[country_year_data$year == years[i - 1], ]
    curr_year <- country_year_data[country_year_data$year == years[i], ]
    
    # Classify into quartiles
    prev_year$state <- cut(rank(-prev_year$production),
                           breaks = c(0, nrow(prev_year) * 0.25, nrow(prev_year) * 0.5,
                                     nrow(prev_year) * 0.75, nrow(prev_year)),
                           labels = state_labels, include.lowest = TRUE)
    
    curr_year$state <- cut(rank(-curr_year$production),
                           breaks = c(0, nrow(curr_year) * 0.25, nrow(curr_year) * 0.5,
                                     nrow(curr_year) * 0.75, nrow(curr_year)),
                           labels = state_labels, include.lowest = TRUE)
    
    # Count transitions
    common <- intersect(prev_year$country, curr_year$country)
    
    for (country in common) {
      state_prev <- prev_year$state[prev_year$country == country]
      state_curr <- curr_year$state[curr_year$country == country]
      
      if (!is.na(state_prev) && !is.na(state_curr)) {
        idx_prev <- which(state_labels == state_prev)
        idx_curr <- which(state_labels == state_curr)
        transition_matrix[idx_prev, idx_curr] <- transition_matrix[idx_prev, idx_curr] + 1
      }
    }
  }
  
  # Convert to probabilities
  row_sums <- rowSums(transition_matrix)
  row_sums[row_sums == 0] <- 1
  transition_prob <- transition_matrix / row_sums
  
  # Stationary distribution
  stationary <- tryCatch({
    eigen_result <- eigen(t(transition_prob))
    stationary_vec <- Re(eigen_result$vectors[, which.min(abs(eigen_result$values - 1))])
    stationary_vec / sum(stationary_vec)
  }, error = function(e) rep(1/n_states, n_states))
  
  list(
    transition_counts = transition_matrix,
    transition_probabilities = transition_prob,
    stationary_distribution = stationary,
    state_labels = state_labels,
    status = "success"
  )
}

#' Compute NELSOP patterns (Network Evolution and Leadership of Scientific Output)
#' @keywords internal
compute_nelsop_patterns <- function(country_year_data) {
  years <- sort(unique(country_year_data$year))
  
  if (length(years) < 5) {
    return(list(status = "insufficient years"))
  }
  
  # Leadership index (share of top 5 countries)
  leadership_index <- numeric(length(years))
  
  for (i in seq_along(years)) {
    year_data <- country_year_data[country_year_data$year == years[i], ]
    year_data <- year_data[order(-year_data$production), ]
    
    total <- sum(year_data$production)
    top5_share <- sum(year_data$production[1:min(5, nrow(year_data))]) / total
    
    leadership_index[i] <- top5_share
  }
  
  # Trend in concentration
  concentration_fit <- lm(leadership_index ~ years)
  concentration_trend <- coef(concentration_fit)[2]
  concentration_p <- summary(concentration_fit)$coefficients[2, 4]
  
  # Network evolution (simplified)
  # Measure connectivity over time
  network_density <- compute_network_density_over_time(country_year_data)
  
  # Specialization index
  specialization <- compute_specialization_index(country_year_data)
  
  list(
    leadership_index = leadership_index,
    concentration_trend = concentration_trend,
    concentration_trend_p = concentration_p,
    interpretation = if (concentration_trend < -0.001) {
      "Decentralizing: leadership is spreading to more countries"
    } else if (concentration_trend > 0.001) {
      "Concentrating: leaders are maintaining dominance"
    } else {
      "Stable: no significant change in concentration"
    },
    network_density = network_density,
    specialization = specialization,
    years = years,
    status = "success"
  )
}

#' Compute network density over time
#' @keywords internal
compute_network_density_over_time <- function(country_year_data) {
  years <- sort(unique(country_year_data$year))
  
  density <- numeric(length(years))
  
  for (i in seq_along(years)) {
    year_data <- country_year_data[country_year_data$year == years[i], ]
    n_countries <- nrow(year_data)
    
    # Density as number of countries relative to max
    density[i] <- n_countries
  }
  
  # Normalize
  density <- density / max(density)
  
  density
}

#' Compute specialization index
#' @keywords internal
compute_specialization_index <- function(country_year_data) {
  # Simple specialization: how concentrated is production
  countries <- unique(country_year_data$country)
  
  specialization <- data.frame(
    country = countries,
    total_production = sapply(countries, function(c) {
      sum(country_year_data$production[country_year_data$country == c])
    }),
    years_present = sapply(countries, function(c) {
      length(unique(country_year_data$year[country_year_data$country == c]))
    }),
    stringsAsFactors = FALSE
  )
  
  specialization$production_per_year <- specialization$total_production / specialization$years_present
  
  specialization[order(-specialization$production_per_year), ]
}

#' Choose a stable comparison window for temporal summaries
#' @keywords internal
m3_temporal_window_size <- function(years) {
  n_years <- length(unique(years))
  if (n_years <= 6) return(max(1L, floor(n_years / 2)))
  min(5L, max(3L, floor(n_years / 4)))
}

#' Average production across a year window
#' @keywords internal
m3_average_window_production <- function(country_year_data, years_window) {
  country_year_data %>%
    dplyr::filter(year %in% years_window) %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(
      avg_production = mean(production, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(avg_production))
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
