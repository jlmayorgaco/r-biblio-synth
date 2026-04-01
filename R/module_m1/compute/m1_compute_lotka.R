# ============================================================================
# m1_compute_lotka.R - Lotka's Law Analysis
# ============================================================================
# Lotka's Law: The number of authors making n contributions is approximately
# 1/n^2 of those making one contribution.
# P(n) = C / n^alpha, where C ~ 0.6079 and alpha ~ 2 (classical)
# 
# IEEE Q1 Enhancement: Fit power-law distribution and test goodness-of-fit
# using Kolmogorov-Smirnov test and maximum likelihood estimation.

#' Compute Lotka's Law analysis for author productivity
#'
#' Analyzes the distribution of author productivity and tests conformity
#' to Lotka's inverse square law using maximum likelihood estimation.
#'
#' @param input Bibliographic data frame
#' @param config Configuration list
#' @return List with Lotka analysis results
#' @export
compute_m1_lotka <- function(input, config = biblio_config()) {
  validate_is_data_frame(input)
  
  authors_col <- "AU"
  if (!authors_col %in% names(input)) {
    authors_col <- "AF"
  }
  if (!authors_col %in% names(input)) {
    return(list(
      lotka = list(
        alpha = NA_real_,
        C = NA_real_,
        gof_ks = NA_real_,
        gof_pvalue = NA_real_,
        is_lotka = NA,
        n_authors = 0,
        frequencies = data.frame()
      ),
      status = "error: no author column found"
    ))
  }
  
  authors <- input[[authors_col]]
  if (is.null(authors) || all(is.na(authors))) {
    return(list(
      lotka = list(
        alpha = NA_real_,
        C = NA_real_,
        gof_ks = NA_real_,
        gof_pvalue = NA_real_,
        is_lotka = NA,
        n_authors = 0,
        frequencies = data.frame()
      ),
      status = "error: no authors found"
    ))
  }
  
  author_counts <- compute_author_article_counts(input, authors_col)
  if (is.null(author_counts) || nrow(author_counts) == 0) {
    return(list(
      lotka = list(
        alpha = NA_real_,
        C = NA_real_,
        gof_ks = NA_real_,
        gof_pvalue = NA_real_,
        is_lotka = NA,
        n_authors = 0,
        frequencies = data.frame()
      ),
      status = "error: could not compute author counts"
    ))
  }
  
  freq_table <- compute_lotka_frequency_table(author_counts)
  if (is.null(freq_table) || nrow(freq_table) == 0) {
    return(list(
      lotka = list(
        alpha = NA_real_,
        C = NA_real_,
        gof_ks = NA_real_,
        gof_pvalue = NA_real_,
        is_lotka = NA,
        n_authors = nrow(author_counts),
        frequencies = data.frame()
      ),
      status = "error: could not compute frequency table"
    ))
  }
  
  lotka_params <- fit_lotka_power_law(freq_table)
  
  gof_result <- test_lotka_goodness_of_fit(freq_table, lotka_params$alpha)
  
  # Classical Lotka's Law: C = 1/zeta(2) ≈ 6/π² ≈ 0.6079
  # For alpha=2, C = 1 / sum_{n=1}^{inf} (1/n²)
  classical_alpha <- 2
  classical_C <- 6 / pi^2  # Exact value: ≈ 0.6079
  alpha_diff <- abs(lotka_params$alpha - classical_alpha)
  is_lotka <- alpha_diff < 0.5
  
  lotka_result <- list(
    alpha = lotka_params$alpha,
    C = lotka_params$C,
    C_classical = classical_C,
    alpha_classical = classical_alpha,
    alpha_diff = alpha_diff,
    gof_ks = gof_result$ks_stat,
    gof_pvalue = gof_result$pvalue,
    is_lotka = is_lotka,
    interpretation = interpret_lotka_result(lotka_params$alpha, is_lotka),
    n_authors = nrow(author_counts),
    n_articles = sum(author_counts$n_articles),
    frequencies = freq_table
  )
  
  list(
    lotka = lotka_result,
    status = "success"
  )
}

#' Compute article counts per author
#' @keywords internal
compute_author_article_counts <- function(input, authors_col) {
  authors_list <- strsplit(as.character(input[[authors_col]]), ";")
  authors_list <- lapply(authors_list, trimws)
  authors_list <- lapply(authors_list, function(x) x[x != ""])
  
  author_counts <- table(unlist(authors_list))
  
  data.frame(
    author = names(author_counts),
    n_articles = as.integer(author_counts),
    stringsAsFactors = FALSE
  )
}

#' Compute Lotka frequency table
#' @keywords internal
compute_lotka_frequency_table <- function(author_counts) {
  freq <- table(author_counts$n_articles)
  
  n_max <- max(as.integer(names(freq)))
  n_seq <- 1:n_max
  n_authors <- as.integer(freq[as.character(n_seq)])
  n_authors[is.na(n_authors)] <- 0L
  
  data.frame(
    n_articles = n_seq,
    n_authors = n_authors,
    proportion = n_authors / sum(n_authors),
    stringsAsFactors = FALSE
  )
}

#' Fit Lotka power-law using maximum likelihood estimation
#' @keywords internal
fit_lotka_power_law <- function(freq_table) {
  n <- freq_table$n_articles
  f <- freq_table$n_authors
  
  valid_idx <- f > 0 & n > 0
  n <- n[valid_idx]
  f <- f[valid_idx]
  
  if (length(n) < 2) {
    return(list(alpha = 2, C = 0.6079, method = "fallback"))
  }
  
  log_likelihood <- function(alpha) {
    if (alpha <= 1) return(-Inf)
    N <- sum(f)
    -N * log(sum(n^(-alpha))) - alpha * sum(f * log(n))
  }
  
  opt_result <- tryCatch({
    optimize(log_likelihood, interval = c(1.01, 5), maximum = TRUE)
  }, error = function(e) {
    list(maximum = 2, objective = 0)
  })
  
  alpha_hat <- opt_result$maximum
  if (length(alpha_hat) > 1) alpha_hat <- alpha_hat[1]
  
  C <- 1 / sum((1:1000)^(-alpha_hat))
  
  list(
    alpha = alpha_hat,
    C = C,
    method = "MLE"
  )
}

#' Test goodness of fit for Lotka's law
#' @keywords internal
test_lotka_goodness_of_fit <- function(freq_table, alpha) {
  observed <- freq_table$n_authors
  total <- sum(observed)
  
  n_max <- max(freq_table$n_articles)
  n_seq <- 1:n_max
  expected_prop <- n_seq^(-alpha) / sum(n_seq^(-alpha))
  expected <- expected_prop * total
  
  n <- length(observed)
  if (n > length(expected)) {
    expected <- c(expected, rep(0, n - length(expected)))
  } else if (length(expected) > n) {
    expected <- expected[1:n]
  }
  
  expected[expected < 5] <- 5
  
  observed_prop <- observed / total
  
  ks_stat <- max(abs(cumsum(observed_prop) - cumsum(expected_prop)))
  if (length(ks_stat) > 1) ks_stat <- ks_stat[1]
  
  pvalue <- tryCatch({
    1 - .C("pkolmogorov2x", p = as.double(0.5))$p
  }, error = function(e) {
    exp(-2 * ks_stat^2 * total)
  })
  if (length(pvalue) > 1) pvalue <- pvalue[1]
  
  list(
    ks_stat = ks_stat,
    pvalue = max(0, min(1, pvalue))
  )
}

#' Interpret Lotka result
#' @keywords internal
interpret_lotka_result <- function(alpha, is_lotka) {
  if (length(alpha) > 1) alpha <- alpha[1]
  if (is.na(alpha)) {
    return("Unable to estimate power-law exponent.")
  }
  
  if (is_lotka) {
    sprintf(
      "Author productivity follows Lotka's Law (alpha = %.2f, close to classical 2.0).",
      alpha
    )
  } else if (alpha > 2) {
    sprintf(
      "Productivity is MORE concentrated than Lotka's Law predicts (alpha = %.2f > 2.0). Fewer authors dominate the field.",
      alpha
    )
  } else {
    sprintf(
      "Productivity is LESS concentrated than Lotka's Law predicts (alpha = %.2f < 2.0). More authors contribute multiple papers.",
      alpha
    )
  }
}