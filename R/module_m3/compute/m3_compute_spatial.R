# ============================================================================
# m3_compute_spatial.R - Spatial Statistics for Country Analysis
# ============================================================================
# Moran's I, Geary's C, Local Indicators of Spatial Association (LISA),
# Getis-Ord Gi* hot spot analysis, and distance-based collaboration analysis

#' Compute Moran's I spatial autocorrelation
#'
#' @param country_data Data frame with country, value columns
#' @param distance_matrix Optional pre-computed distance matrix
#' @return List with Moran's I statistic and significance test
#' @export
m3_compute_morans_i <- function(country_data, distance_matrix = NULL) {
  if (!is.data.frame(country_data) || !all(c("country", "value") %in% names(country_data))) {
    return(list(
      statistic = NA,
      p_value = NA,
      interpretation = "error: invalid input",
      status = "error"
    ))
  }
  
  values <- country_data$value
  n <- length(values)
  
  if (n < 4) {
    return(list(
      statistic = NA,
      p_value = NA,
      interpretation = "insufficient countries",
      status = "error"
    ))
  }
  
  # Use distance matrix or create inverse distance weights
  if (is.null(distance_matrix)) {
    # Create binary contiguity weights (simplified)
    # In practice, would use actual geographic distances
    W <- matrix(1, n, n) - diag(1, n)
  } else {
    W <- distance_matrix
    # Inverse distance weighting
    W <- 1 / (W + 0.001)  # Add small constant to avoid division by zero
  }
  
  # Row-standardize weights
  W <- W / rowSums(W)
  W[is.na(W)] <- 0
  
  # Moran's I calculation
  mean_val <- mean(values, na.rm = TRUE)
  dev <- values - mean_val
  
  numerator <- sum(W * outer(dev, dev), na.rm = TRUE)
  denominator <- sum(dev^2, na.rm = TRUE)
  
  moran_i <- (n / sum(W, na.rm = TRUE)) * (numerator / denominator)
  
  # Expected value under null
  expected_i <- -1 / (n - 1)
  
  # Variance (simplified)
  s1 <- 0.5 * sum((W + t(W))^2, na.rm = TRUE)
  s2 <- sum((rowSums(W) + colSums(W))^2, na.rm = TRUE)
  
  var_i <- (n^2 * s1 - n * s2 + 2 * s1) / (n^2 * (n - 1)^2 * sum(dev^2)^2) * sum(dev^2)^2
  
  # Simplified variance
  var_i <- (n^2 - 1) / (n^2 * (n - 1)^2) - expected_i^2
  
  # Z-score
  z_score <- (moran_i - expected_i) / sqrt(var_i)
  
  # P-value (two-tailed)
  p_value <- 2 * (1 - pnorm(abs(z_score)))
  
  # Interpretation
  if (moran_i > 0.3 && p_value < 0.05) {
    interpretation <- "Strong positive spatial autocorrelation: similar countries cluster"
  } else if (moran_i > 0 && p_value < 0.05) {
    interpretation <- "Weak positive spatial autocorrelation: some clustering"
  } else if (moran_i < -0.3 && p_value < 0.05) {
    interpretation <- "Strong negative spatial autocorrelation: dissimilar countries cluster"
  } else if (moran_i < 0 && p_value < 0.05) {
    interpretation <- "Weak negative spatial autocorrelation: some dispersal"
  } else {
    interpretation <- "No significant spatial autocorrelation: random distribution"
  }
  
  list(
    statistic = moran_i,
    expected = expected_i,
    variance = var_i,
    z_score = z_score,
    p_value = p_value,
    significant = p_value < 0.05,
    interpretation = interpretation,
    n_countries = n,
    status = "success"
  )
}

#' Compute Geary's C spatial autocorrelation
#'
#' @param country_data Data frame with country, value columns
#' @param distance_matrix Optional pre-computed distance matrix
#' @return List with Geary's C statistic and significance test
#' @export
m3_compute_gearys_c <- function(country_data, distance_matrix = NULL) {
  if (!is.data.frame(country_data) || !all(c("country", "value") %in% names(country_data))) {
    return(list(
      statistic = NA,
      p_value = NA,
      status = "error"
    ))
  }
  
  values <- country_data$value
  n <- length(values)
  
  if (n < 4) {
    return(list(
      statistic = NA,
      p_value = NA,
      status = "error: insufficient countries"
    ))
  }
  
  # Create weights matrix
  if (is.null(distance_matrix)) {
    W <- matrix(1, n, n) - diag(1, n)
  } else {
    W <- distance_matrix
    W <- 1 / (W + 0.001)
  }
  
  W <- W / sum(W)
  
  mean_val <- mean(values, na.rm = TRUE)
  
  # Geary's C calculation
  numerator <- sum(W * outer(values, values, function(x, y) (x - y)^2), na.rm = TRUE)
  denominator <- sum((values - mean_val)^2, na.rm = TRUE)
  
  geary_c <- (n - 1) / (2 * sum(W, na.rm = TRUE)) * numerator / denominator
  
  # Expected under null
  expected_c <- 1
  
  # Variance (simplified)
  var_c <- 1 / (2 * (n - 1))
  
  # Z-score
  z_score <- (geary_c - expected_c) / sqrt(var_c)
  p_value <- 2 * (1 - pnorm(abs(z_score)))
  
  # Interpretation
  if (geary_c < 0.5 && p_value < 0.05) {
    interpretation <- "Strong positive spatial autocorrelation"
  } else if (geary_c < 1 && p_value < 0.05) {
    interpretation <- "Weak positive spatial autocorrelation"
  } else if (geary_c > 1.5 && p_value < 0.05) {
    interpretation <- "Strong negative spatial autocorrelation"
  } else if (geary_c > 1 && p_value < 0.05) {
    interpretation <- "Weak negative spatial autocorrelation"
  } else {
    interpretation <- "No significant spatial autocorrelation"
  }
  
  list(
    statistic = geary_c,
    expected = expected_c,
    variance = var_c,
    z_score = z_score,
    p_value = p_value,
    significant = p_value < 0.05,
    interpretation = interpretation,
    n_countries = n,
    status = "success"
  )
}

#' Compute Getis-Ord Gi* hot spot analysis
#'
#' @param country_data Data frame with country, value, lat, lon columns
#' @param distance_matrix Optional pre-computed distance matrix
#' @return List with Gi* statistics and hot/cold spots
#' @export
m3_compute_getis_ord <- function(country_data, distance_matrix = NULL) {
  required_cols <- c("country", "value")
  if (!is.data.frame(country_data) || !all(required_cols %in% names(country_data))) {
    return(list(
      gi_values = data.frame(),
      hot_spots = character(0),
      cold_spots = character(0),
      status = "error: invalid input"
    ))
  }
  
  values <- country_data$value
  n <- length(values)
  
  if (n < 4) {
    return(list(
      gi_values = data.frame(),
      hot_spots = character(0),
      cold_spots = character(0),
      status = "error: insufficient countries"
    ))
  }
  
  # Create weights matrix
  if (is.null(distance_matrix)) {
    W <- matrix(1, n, n) - diag(1, n)
  } else {
    W <- distance_matrix
    W <- 1 / (W + 0.001)
  }
  
  # Row-standardize
  row_sums <- rowSums(W)
  row_sums[row_sums == 0] <- 1
  W <- W / row_sums
  
  mean_val <- mean(values, na.rm = TRUE)
  sd_val <- sd(values, na.rm = TRUE)
  
  # Gi* for each location
  gi_values <- numeric(n)
  z_scores <- numeric(n)
  p_values <- numeric(n)
  
  for (i in 1:n) {
    # Weighted sum of neighbors
    weighted_sum <- sum(W[i, ] * values, na.rm = TRUE)
    
    # Gi* statistic (includes self)
    W_sum_i <- sum(W[i, ])
    
    # Gi* formula
    gi_values[i] <- (weighted_sum - mean_val * W_sum_i) / 
                   (sd_val * sqrt((n * W_sum_i - W_sum_i^2) / (n - 1)))
    
    # Z-score approximation
    z_scores[i] <- gi_values[i]
    p_values[i] <- 2 * (1 - pnorm(abs(z_scores[i])))
  }
  
  # Identify hot and cold spots
  hot_spots <- country_data$country[z_scores > 1.96 & p_values < 0.05]
  cold_spots <- country_data$country[z_scores < -1.96 & p_values < 0.05]
  
  # Confidence levels
  confidence <- ifelse(abs(z_scores) > 2.58, "99%",
                      ifelse(abs(z_scores) > 1.96, "95%",
                      ifelse(abs(z_scores) > 1.65, "90%", "Not significant")))
  
  results <- data.frame(
    country = country_data$country,
    value = values,
    gi_star = gi_values,
    z_score = z_scores,
    p_value = p_values,
    confidence = confidence,
    type = ifelse(z_scores > 1.96, "Hot spot",
                 ifelse(z_scores < -1.96, "Cold spot", "Not significant")),
    stringsAsFactors = FALSE
  )
  
  list(
    gi_values = results,
    hot_spots = hot_spots,
    cold_spots = cold_spots,
    n_hot_spots = length(hot_spots),
    n_cold_spots = length(cold_spots),
    global_gi = (sum(values) - n * mean_val) / (sd_val * sqrt(n - 1)),
    interpretation = sprintf("Found %d hot spots and %d cold spots at p<0.05", 
                           length(hot_spots), length(cold_spots)),
    status = "success"
  )
}

#' Compute Local Indicators of Spatial Association (LISA)
#'
#' @param country_data Data frame with country, value columns
#' @param distance_matrix Optional pre-computed distance matrix
#' @return List with LISA statistics and cluster types
#' @export
m3_compute_lisa <- function(country_data, distance_matrix = NULL) {
  required_cols <- c("country", "value")
  if (!is.data.frame(country_data) || !all(required_cols %in% names(country_data))) {
    return(list(
      lisa_values = data.frame(),
      clusters = data.frame(),
      status = "error: invalid input"
    ))
  }
  
  values <- country_data$value
  n <- length(values)
  countries <- country_data$country
  
  if (n < 4) {
    return(list(
      lisa_values = data.frame(),
      clusters = data.frame(),
      status = "error: insufficient countries"
    ))
  }
  
  # Create spatial weights
  if (is.null(distance_matrix)) {
    W <- matrix(1, n, n) - diag(1, n)
  } else {
    W <- distance_matrix
    W <- 1 / (W + 0.001)
  }
  
  # Row-standardize
  row_sums <- rowSums(W)
  row_sums[row_sums == 0] <- 1
  W <- W / row_sums
  
  mean_val <- mean(values, na.rm = TRUE)
  sd_val <- sd(values, na.rm = TRUE)
  
  # Standardized values
  z <- (values - mean_val) / sd_val
  
  # Local Moran's I for each location
  local_i <- numeric(n)
  p_values <- numeric(n)
  
  for (i in 1:n) {
    # Lagged value
    z_lag <- sum(W[i, ] * z)
    
    # Local Moran's I
    local_i[i] <- z[i] * z_lag
    
    # Permutation test for significance
    n_perm <- 499
    perm_i <- numeric(n_perm)
    for (p in 1:n_perm) {
      z_perm <- sample(z)
      z_lag_perm <- sum(W[i, ] * z_perm)
      perm_i[p] <- z[i] * z_lag_perm
    }
    
    p_values[i] <- (sum(perm_i >= local_i[i]) + 1) / (n_perm + 1)
  }
  
  # Determine cluster types
  # HH: High-High (high value surrounded by high values)
  # LL: Low-Low (low value surrounded by low values)
  # HL: High-Low (high value surrounded by low values) - outlier
  # LH: Low-High (low value surrounded by high values) - outlier
  
  cluster_types <- character(n)
  for (i in 1:n) {
    if (p_values[i] >= 0.05) {
      cluster_types[i] <- "Not significant"
    } else if (z[i] > 0 && local_i[i] > 0) {
      cluster_types[i] <- "HH"  # High-High cluster
    } else if (z[i] < 0 && local_i[i] > 0) {
      cluster_types[i] <- "LL"  # Low-Low cluster
    } else if (z[i] > 0 && local_i[i] < 0) {
      cluster_types[i] <- "HL"  # High-Low outlier
    } else {
      cluster_types[i] <- "LH"  # Low-High outlier
    }
  }
  
  results <- data.frame(
    country = countries,
    value = values,
    z_score = z,
    local_i = local_i,
    p_value = p_values,
    cluster_type = cluster_types,
    significant = p_values < 0.05,
    stringsAsFactors = FALSE
  )
  
  # Summary by cluster type
  cluster_summary <- data.frame(
    type = c("HH", "LL", "HL", "LH", "Not significant"),
    count = c(
      sum(cluster_types == "HH"),
      sum(cluster_types == "LL"),
      sum(cluster_types == "HL"),
      sum(cluster_types == "LH"),
      sum(cluster_types == "Not significant")
    ),
    countries = c(
      paste(countries[cluster_types == "HH"], collapse = ", "),
      paste(countries[cluster_types == "LL"], collapse = ", "),
      paste(countries[cluster_types == "HL"], collapse = ", "),
      paste(countries[cluster_types == "LH"], collapse = ", "),
      paste(countries[cluster_types == "Not significant"], collapse = ", ")
    ),
    stringsAsFactors = FALSE
  )
  
  list(
    lisa_values = results,
    clusters = cluster_summary,
    hh_clusters = countries[cluster_types == "HH"],
    ll_clusters = countries[cluster_types == "LL"],
    hl_outliers = countries[cluster_types == "HL"],
    lh_outliers = countries[cluster_types == "LH"],
    n_significant = sum(p_values < 0.05),
    interpretation = sprintf("%d countries in significant clusters (%d HH, %d LL, %d outliers)",
                           sum(p_values < 0.05),
                           sum(cluster_types == "HH"),
                           sum(cluster_types == "LL"),
                           sum(cluster_types %in% c("HL", "LH"))),
    status = "success"
  )
}

#' Compute gravity model for international collaboration
#'
#' @param collab_matrix Collaboration matrix (countries x countries)
#' @param distance_matrix Geographic distance matrix
#' @param country_attr Country attributes (GDP, population, etc.)
#' @return List with gravity model estimates
#' @export
m3_compute_gravity_model <- function(collab_matrix, distance_matrix, country_attr = NULL) {
  if (!is.matrix(collab_matrix) || !is.matrix(distance_matrix)) {
    return(list(
      coefficients = data.frame(),
      R_squared = NA,
      status = "error: invalid input"
    ))
  }
  
  n <- nrow(collab_matrix)
  
  if (n != nrow(distance_matrix)) {
    return(list(
      coefficients = data.frame(),
      R_squared = NA,
      status = "error: matrix dimension mismatch"
    ))
  }
  
  # Convert to vectors for regression
  # Collaboration_ij = A * (GDP_i^a * GDP_j^b) / (Distance_ij^d)
  
  # Log-linearize: log(Collab_ij) = log(A) + a*log(GDP_i) + b*log(GDP_j) - d*log(Distance_ij)
  
  # Get upper triangle (to avoid duplicates)
  collab_vec <- collab_matrix[upper.tri(collab_matrix)]
  dist_vec <- distance_matrix[upper.tri(distance_matrix)]
  
  # Remove zeros for log transformation
  valid_idx <- collab_vec > 0 & dist_vec > 0
  
  if (sum(valid_idx) < 10) {
    return(list(
      coefficients = data.frame(),
      R_squared = NA,
      status = "error: insufficient positive values"
    ))
  }
  
  collab_vec <- collab_vec[valid_idx]
  dist_vec <- dist_vec[valid_idx]
  
  # Simple gravity model: log(Collab_ij) = a - d*log(Distance_ij)
  log_collab <- log(collab_vec)
  log_dist <- log(dist_vec)
  
  fit <- lm(log_collab ~ log_dist)
  
  # Extract coefficients
  intercept <- coef(fit)[1]
  distance_coef <- coef(fit)[2]
  
  # Distance decay effect
  distance_effect <- -distance_coef
  
  # If country attributes provided, enhanced model
  if (!is.null(country_attr) && "production" %in% names(country_attr)) {
    # Add production attributes
    # This would require matching country indices
  }
  
  # Goodness of fit
  r_squared <- summary(fit)$r.squared
  
  # Interpretation
  interpretation <- if (distance_effect > 2) {
    "Strong distance effect: collaboration decreases sharply with distance"
  } else if (distance_effect > 1) {
    "Moderate distance effect: some distance decay in collaboration"
  } else {
    "Weak distance effect: distance plays minor role in collaboration"
  }
  
  coefficients_df <- data.frame(
    term = c("Intercept", "log(Distance)"),
    estimate = c(intercept, distance_coef),
    std_error = summary(fit)$coefficients[, 2],
    t_value = summary(fit)$coefficients[, 3],
    p_value = summary(fit)$coefficients[, 4]
  )
  
  list(
    coefficients = coefficients_df,
    distance_decay = distance_effect,
    R_squared = r_squared,
    AIC = AIC(fit),
    BIC = BIC(fit),
    interpretation = interpretation,
    near_effect = exp(intercept),  # Collaboration at unit distance
    status = "success"
  )
}

#' Create distance matrix from country coordinates
#'
#' @param countries Vector of country names
#' @param coords Data frame with country, lat, lon columns (optional)
#' @return Distance matrix in kilometers
#' @export
m3_create_distance_matrix <- function(countries, coords = NULL) {
  n <- length(countries)
  
  if (is.null(coords)) {
    # Would need rnaturalearth to get coordinates
    # Return placeholder matrix
    dist_matrix <- matrix(0, n, n)
    rownames(dist_matrix) <- colnames(dist_matrix) <- countries
    return(list(
      distance_matrix = dist_matrix,
      status = "error: coordinates not available - install rnaturalearth"
    ))
  }
  
  # Haversine distance calculation
  dist_matrix <- matrix(0, n, n)
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        lat1 <- coords$lat[coords$country == countries[i]]
        lon1 <- coords$lon[coords$country == countries[i]]
        lat2 <- coords$lat[coords$country == countries[j]]
        lon2 <- coords$lon[coords$country == countries[j]]
        
        if (length(lat1) == 1 && length(lat2) == 1) {
          dist_matrix[i, j] <- haversine_distance(lat1, lon1, lat2, lon2)
        }
      }
    }
  }
  
  rownames(dist_matrix) <- colnames(dist_matrix) <- countries
  
  list(
    distance_matrix = dist_matrix,
    status = "success"
  )
}

#' Haversine distance calculation
#' @keywords internal
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  # Convert to radians
  lat1 <- lat1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon1 <- lon1 * pi / 180
  lon2 <- lon2 * pi / 180
  
  # Haversine formula
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  
  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  c <- 2 * asin(sqrt(a))
  
  # Earth radius in km
  R <- 6371
  
  R * c
}

`%||%` <- function(a, b) if (!is.null(a)) a else b