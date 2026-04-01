# ============================================================================
# m1_compute_citation_analysis.R - Advanced Citation Analysis
# ============================================================================
# Citation distribution fitting, half-life, decay, self-citation, velocity

#' Compute advanced citation analysis
#'
#' @param input Bibliographic data frame
#' @param config Configuration list
#' @return List with citation metrics
#' @export
compute_m1_citation_analysis <- function(input, config = biblio_config()) {
  tc_col <- "TC"
  if (!tc_col %in% names(input)) {
    tc_col <- grep("^TC|citation|times.*cited", names(input), ignore.case = TRUE, value = TRUE)[1]
  }
  
  if (is.null(tc_col) || !tc_col %in% names(input)) {
    return(list(status = "error: no citation column found"))
  }
  
  citations <- as.numeric(input[[tc_col]])
  citations <- citations[!is.na(citations)]
  
  if (length(citations) < 10) {
    return(list(status = "error: insufficient citation data"))
  }
  
  # Distribution fitting
  dist_fit <- fit_citation_distributions(citations)
  
  # Citation age analysis
  age_analysis <- compute_citation_age(input, tc_col)
  
  # Self-citation analysis
  self_citation <- compute_self_citations(input)
  
  # Citation velocity
  velocity <- compute_citation_velocity(input, tc_col)
  
  # Summary statistics
  summary_stats <- list(
    total_citations = sum(citations),
    mean_citations = mean(citations),
    median_citations = median(citations),
    sd_citations = sd(citations),
    max_citations = max(citations),
    n_zero_citations = sum(citations == 0),
    pct_zero_citations = mean(citations == 0) * 100,
    gini_coefficient = safe_gini(citations),
    h_index = compute_h_index(citations),
    g_index = compute_g_index(citations),
    i10_index = sum(citations >= 10)
  )
  
  list(
    summary = summary_stats,
    distribution_fit = dist_fit,
    age_analysis = age_analysis,
    self_citation = self_citation,
    velocity = velocity,
    citations = citations,
    status = "success"
  )
}

#' Fit distributions to citation counts
#' @keywords internal
fit_citation_distributions <- function(citations) {
  citations <- citations[citations >= 0]
  citations <- citations[citations > 0]  # For log-based fits
  
  if (length(citations) < 10) {
    return(list(status = "insufficient data"))
  }
  
  results <- list()
  
  # 1. Power law (Pareto) fit
  power_law <- tryCatch({
    fit_power_law(citations)
  }, error = function(e) list(status = "error", alpha = NA, xmin = NA))
  results$power_law <- power_law
  
  # 2. Log-normal fit
  log_normal <- tryCatch({
    fit_log_normal(citations)
  }, error = function(e) list(status = "error", meanlog = NA, sdlog = NA))
  results$log_normal <- log_normal
  
  # 3. Exponential fit
  exponential <- tryCatch({
    fit_exponential(citations)
  }, error = function(e) list(status = "error", lambda = NA))
  results$exponential <- exponential
  
  # 4. Weibull fit
  weibull <- tryCatch({
    fit_weibull(citations)
  }, error = function(e) list(status = "error", shape = NA, scale = NA))
  results$weibull <- weibull
  
  # Compare distributions using AIC
  aic_values <- c(
    power_law = if (!is.na(power_law$AIC)) power_law$AIC else Inf,
    log_normal = if (!is.na(log_normal$AIC)) log_normal$AIC else Inf,
    exponential = if (!is.na(exponential$AIC)) exponential$AIC else Inf,
    weibull = if (!is.na(weibull$AIC)) weibull$AIC else Inf
  )
  
  best_dist <- names(which.min(aic_values))
  
  # KS test for goodness of fit
  ks_tests <- list(
    power_law = tryCatch(ks.test(citations, function(x) ppareto(x, power_law$xmin, power_law$alpha)), error = function(e) NULL),
    log_normal = tryCatch(ks.test(log(citations), "pnorm", log_normal$meanlog, log_normal$sdlog), error = function(e) NULL)
  )
  
  results$comparison <- data.frame(
    distribution = names(aic_values),
    AIC = aic_values,
    delta_AIC = aic_values - min(aic_values),
    stringsAsFactors = FALSE
  )
  results$best_fit <- best_dist
  results$ks_tests <- ks_tests
  results$status <- "success"
  
  results
}

#' Fit power law distribution
#' @keywords internal
fit_power_law <- function(x) {
  x <- sort(x)
  n <- length(x)
  
  # Estimate xmin using Kolmogorov-Smirnov method
  xmin_values <- unique(x[x > 0])
  xmin_values <- xmin_values[xmin_values >= min(x)]
  
  best_ks <- Inf
  best_xmin <- min(x)
  best_alpha <- 1
  
  for (xmin in head(xmin_values, 100)) {
    x_tail <- x[x >= xmin]
    n_tail <- length(x_tail)
    
    if (n_tail < 10) next
    
    # MLE for alpha
    alpha <- 1 + n_tail / sum(log(x_tail / xmin))
    
    # KS statistic
    ks_stat <- tryCatch({
      cdf_empirical <- cumsum(table(x_tail)) / n_tail
      cdf_theoretical <- 1 - (xmin / sort(x_tail))^(alpha - 1)
      max(abs(cdf_empirical - cdf_theoretical))
    }, error = function(e) Inf)
    
    if (ks_stat < best_ks) {
      best_ks <- ks_stat
      best_xmin <- xmin
      best_alpha <- alpha
    }
  }
  
  # Calculate log-likelihood
  x_tail <- x[x >= best_xmin]
  n_tail <- length(x_tail)
  log_lik <- n_tail * log(best_alpha - 1) - n_tail * log(best_xmin) - (best_alpha - 1) * sum(log(x_tail / best_xmin))
  
  # AIC
  k <- 2  # parameters: alpha, xmin
  AIC <- -2 * log_lik + 2 * k
  
  list(
    alpha = best_alpha,
    xmin = best_xmin,
    ks_statistic = best_ks,
    log_likelihood = log_lik,
    AIC = AIC,
    n_tail = n_tail,
    status = "success"
  )
}

#' Fit log-normal distribution
#' @keywords internal
fit_log_normal <- function(x) {
  x <- x[x > 0]
  log_x <- log(x)
  
  n <- length(x)
  if (n < 2) {
    return(list(meanlog = NA, sdlog = NA, log_likelihood = NA, AIC = NA, status = "error: insufficient data"))
  }
  
  mean_log <- mean(log_x)
  sd_log <- sd(log_x)
  
  # Log-likelihood for log-normal distribution
  # Using MLE estimates: meanlog = mean(log_x), sdlog = sd(log_x)
  # log-likelihood = -n*log(sdlog) - n/2*log(2*pi) - sum(log(x)) - sum((log(x)-meanlog)^2)/(2*sdlog^2)
  log_lik <- -n * log(sd_log) - n/2 * log(2 * pi) - sum(log_x) - sum((log_x - mean_log)^2) / (2 * sd_log^2)
  
  # AIC
  k <- 2  # parameters: meanlog, sdlog
  AIC <- -2 * log_lik + 2 * k
  
  list(
    meanlog = mean_log,
    sdlog = sd_log,
    log_likelihood = log_lik,
    AIC = AIC,
    status = "success"
  )
}

#' Fit exponential distribution
#' @keywords internal
fit_exponential <- function(x) {
  x <- x[x > 0]
  
  lambda <- 1 / mean(x)
  
  n <- length(x)
  log_lik <- n * log(lambda) - lambda * sum(x)
  
  k <- 1  # parameter: lambda
  AIC <- -2 * log_lik + 2 * k
  
  list(
    lambda = lambda,
    rate = lambda,
    mean = 1 / lambda,
    log_likelihood = log_lik,
    AIC = AIC,
    status = "success"
  )
}

#' Fit Weibull distribution
#' @keywords internal
fit_weibull <- function(x) {
  x <- x[x > 0]
  n <- length(x)
  
  # Method of moments for initial estimates
  mean_x <- mean(x)
  sd_x <- sd(x)
  
  # Approximate shape (k) parameter
  shape_approx <- (sd_x / mean_x)^(-1.086)
  
  # Optimize
  neg_log_lik <- function(shape) {
    scale <- mean_x / gamma(1 + 1/shape)
    -sum(dweibull(x, shape = shape, scale = scale, log = TRUE))
  }
  
  opt <- tryCatch({
    optimize(neg_log_lik, interval = c(0.1, 10))
  }, error = function(e) list(minimum = shape_approx, objective = NA))
  
  shape <- opt$minimum
  scale <- mean_x / gamma(1 + 1/shape)
  
  # AIC
  log_lik <- sum(dweibull(x, shape = shape, scale = scale, log = TRUE))
  AIC <- -2 * log_lik + 2 * 2  # 2 parameters
  
  list(
    shape = shape,
    scale = scale,
    log_likelihood = log_lik,
    AIC = AIC,
    status = "success"
  )
}

#' Compute citation age analysis
#' @keywords internal
compute_citation_age <- function(input, tc_col) {
  py_col <- "PY"
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  if (!py_col %in% names(input)) {
    return(list(
      half_life = NA,
      decay_rate = NA,
      age_distribution = data.frame(),
      status = "no year data"
    ))
  }
  
  years <- as.numeric(input[[py_col]])
  citations <- as.numeric(input[[tc_col]])
  
  # Calculate citation age
  age <- current_year - years
  age <- age[!is.na(age) & !is.na(citations)]
  citations <- citations[!is.na(age) & !is.na(citations)]
  
  if (length(age) < 5) {
    return(list(status = "insufficient data"))
  }
  
  # Age distribution
  age_dist <- aggregate(citations ~ age, FUN = sum)
  names(age_dist) <- c("age", "total_citations")
  age_dist$mean_citations <- aggregate(citations ~ age, FUN = mean)$citations
  age_dist$n_papers <- aggregate(citations ~ age, FUN = length)$citations
  
  # Citation half-life (age at which half of citations received)
  cumsum_cit <- cumsum(age_dist$total_citations[order(age_dist$age)])
  total_cit <- sum(age_dist$total_citations)
  half_life_age <- min(age_dist$age[order(age_dist$age)][cumsum_cit >= total_cit / 2])
  
  # Decay rate (annual percentage decrease in citations)
  if (nrow(age_dist) > 2) {
    age_ordered <- age_dist[order(age_dist$age), ]
    age_ordered$citations_per_paper <- age_ordered$total_citations / age_ordered$n_papers
    
    # Fit exponential decay
    fit <- tryCatch({
      lm(log(citations_per_paper + 1) ~ age, data = age_ordered)
    }, error = function(e) NULL)
    
    decay_rate <- if (!is.null(fit)) {
      -coef(fit)[2]  # Negative of slope
    } else {
      NA
    }
  } else {
    decay_rate <- NA
  }
  
  # Price Index (proportion of citations from recent years)
  recent_citations <- sum(age_dist$total_citations[age_dist$age <= 5])
  price_index <- recent_citations / total_cit
  
  list(
    half_life = half_life_age,
    decay_rate = decay_rate,
    price_index = price_index,
    age_distribution = age_dist,
    median_age = median(age),
    mean_age = weighted.mean(age_dist$age, age_dist$n_papers),
    status = "success"
  )
}

#' Compute self-citation analysis
#' @keywords internal
compute_self_citations <- function(input) {
  au_col <- "AU"
  cr_col <- "CR"
  
  if (!au_col %in% names(input) || !cr_col %in% names(input)) {
    return(list(
      self_citation_rate = NA,
      n_self_citations = 0,
      status = "missing author or reference column"
    ))
  }
  
  authors_list <- as.character(input[[au_col]])
  refs_list <- as.character(input[[cr_col]])
  
  n_self_citations <- 0
  n_total_refs <- 0
  
  for (i in seq_along(authors_list)) {
    if (is.na(authors_list[i]) || is.na(refs_list[i])) next
    
    # Extract author last names
    authors <- strsplit(authors_list[i], ";")[[1]]
    author_names <- trimws(sapply(authors, function(a) {
      parts <- strsplit(a, ",")[[1]]
      if (length(parts) > 0) parts[1] else ""
    }))
    author_names <- author_names[author_names != ""]
    author_names <- toupper(author_names)
    
    # Extract reference authors
    refs <- strsplit(refs_list[i], ";")[[1]]
    n_total_refs <- n_total_refs + length(refs)
    
    for (ref in refs) {
      # Check if any author appears in reference
      ref_upper <- toupper(ref)
      for (author in author_names) {
        if (nchar(author) > 2 && grepl(author, ref_upper, fixed = TRUE)) {
          n_self_citations <- n_self_citations + 1
          break
        }
      }
    }
  }
  
  self_citation_rate <- if (n_total_refs > 0) {
    n_self_citations / n_total_refs
  } else {
    NA
  }
  
  list(
    self_citation_rate = self_citation_rate,
    n_self_citations = n_self_citations,
    n_total_references = n_total_refs,
    status = "success"
  )
}

#' Compute citation velocity (time to first citation)
#' @keywords internal
compute_citation_velocity <- function(input, tc_col) {
  py_col <- "PY"
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  if (!py_col %in% names(input)) {
    return(list(
      median_time_to_first_citation = NA,
      mean_velocity = NA,
      status = "no year data"
    ))
  }
  
  years <- as.numeric(input[[py_col]])
  citations <- as.numeric(input[[tc_col]])
  
  # Articles with at least one citation
  cited_mask <- !is.na(citations) & citations > 0 & !is.na(years)
  cited_citations <- citations[cited_mask]
  cited_years <- years[cited_mask]
  
  if (length(cited_years) < 5) {
    return(list(status = "insufficient cited articles"))
  }
  
  # Age of cited articles when cited
  # Assumption: First citation likely within first 1-2 years for most
  # Use heuristics based on citation pattern
  
  age_at_first_cite <- pmax(1, current_year - cited_years)
  
  # Articles with high citations likely cited early
  # Estimate time to first citation based on citation count
  # Higher citations = earlier first citation
  
  # Estimate using citation ratio
  avg_annual_citations <- cited_citations / age_at_first_cite
  
  # Velocity: citations per year
  velocities <- data.frame(
    year = cited_years,
    citations = cited_citations,
    age = age_at_first_cite,
    annual_velocity = avg_annual_citations
  )
  
  # Summary statistics
  median_velocity <- median(velocities$annual_velocity, na.rm = TRUE)
  mean_velocity <- mean(velocities$annual_velocity, na.rm = TRUE)
  
  # Immediacy index (citations in current year / articles in current year)
  current_articles <- sum(years == current_year, na.rm = TRUE)
  current_citations <- sum(citations[years == current_year], na.rm = TRUE)
  immediacy_index <- if (current_articles > 0) {
    current_citations / current_articles
  } else {
    NA
  }
  
  # Velocity by year
  velocity_by_year <- aggregate(
    annual_velocity ~ year,
    data = velocities,
    FUN = median
  )
  names(velocity_by_year) <- c("year", "median_velocity")
  
  # Trend in velocity
  if (nrow(velocity_by_year) > 2) {
    velocity_fit <- lm(median_velocity ~ year, data = velocity_by_year)
    velocity_trend <- coef(velocity_fit)[2]
    velocity_trend_p <- summary(velocity_fit)$coefficients[2, 4]
  } else {
    velocity_trend <- NA
    velocity_trend_p <- NA
  }
  
  list(
    median_velocity = median_velocity,
    mean_velocity = mean_velocity,
    immediacy_index = immediacy_index,
    velocity_by_year = velocity_by_year,
    velocity_trend = velocity_trend,
    velocity_trend_p = velocity_trend_p,
    percentile_90_velocity = quantile(velocities$annual_velocity, 0.90),
    distribution = velocities,
    status = "success"
  )
}

#' Compute h-index
#' @keywords internal
compute_h_index <- function(citations) {
  citations <- sort(citations, decreasing = TRUE)
  h <- 0
  for (i in seq_along(citations)) {
    if (citations[i] >= i) {
      h <- i
    } else {
      break
    }
  }
  h
}

#' Compute g-index
#' @keywords internal
compute_g_index <- function(citations) {
  citations <- sort(citations, decreasing = TRUE)
  g <- 0
  cumsum_cit <- cumsum(citations)
  
  for (i in seq_along(citations)) {
    if (cumsum_cit[i] >= i^2) {
      g <- i
    } else {
      break
    }
  }
  g
}

#' Weighted mean with safe division
#' @keywords internal
weighted.mean <- function(x, w, na.rm = TRUE) {
  if (na.rm) {
    valid <- !is.na(x) & !is.na(w)
    x <- x[valid]
    w <- w[valid]
  }
  
  if (length(x) == 0 || sum(w) == 0) return(NA)
  
  sum(x * w) / sum(w)
}

#' Pareto CDF
#' @keywords internal
ppareto <- function(x, xmin, alpha) {
  ifelse(x < xmin, 0, 1 - (xmin / x)^(alpha - 1))
}

`%||%` <- function(a, b) if (!is.null(a)) a else b