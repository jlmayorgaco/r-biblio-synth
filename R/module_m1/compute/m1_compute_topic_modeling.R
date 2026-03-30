# ============================================================================
# m1_compute_topic_modeling.R - Topic Modeling with LDA
# ============================================================================
# Latent Dirichlet Allocation and topic analysis for bibliometric data

#' Compute LDA topic model
#'
#' @param input Bibliographic data frame with KW/DE/ID columns
#' @param config Configuration list
#' @return List with topics, document-topic distributions, and metrics
#' @export
compute_m1_topic_modeling <- function(input, config = biblio_config()) {
  kw_cols <- c("KW", "DE", "ID", "Keywords", "AB")
  kw_col <- NULL
  for (col in kw_cols) {
    if (col %in% names(input)) {
      kw_col <- col
      break
    }
  }
  
  if (is.null(kw_col)) {
    return(list(
      status = "error: no keyword column found",
      topics = data.frame(),
      n_topics = 0
    ))
  }
  
  # Extract keywords
  keywords_list <- extract_keywords(input[[kw_col]])
  
  if (length(keywords_list) < 10) {
    return(list(
      status = "error: insufficient documents",
      topics = data.frame(),
      n_topics = 0
    ))
  }
  
  # Determine optimal number of topics
  n_topics <- config$n_topics %||% determine_optimal_topics(keywords_list)
  n_topics <- min(n_topics, length(keywords_list) %/% 5)
  n_topics <- max(n_topics, 2)
  
  # Create document-term matrix
  dtm <- create_dtm(keywords_list)
  
  # Fit LDA model
  lda_result <- fit_lda_model(dtm, n_topics)
  
  if (lda_result$status == "error") {
    return(lda_result)
  }
  
  # Extract topics
  topics_result <- extract_topics(lda_result, dtm)
  topics_df <- topics_result$topics
  
  # Document-topic distribution
  doc_topics <- get_document_topics(lda_result, nrow(dtm))
  
  # Topic metrics
  topic_metrics <- compute_topic_metrics(lda_result, topics_df, doc_topics)
  
  # Topic evolution over time
  py_col <- if ("PY" %in% names(input)) input$PY else if ("Year" %in% names(input)) input$Year else NULL
  topic_evolution <- compute_topic_evolution(doc_topics, py_col, keywords_list)
  
  # Topic co-occurrence
  topic_cooccurrence <- compute_topic_cooccurrence(doc_topics)
  
  list(
    status = "success",
    n_topics = n_topics,
    topics = topics_df,
    document_topics = doc_topics,
    topic_metrics = topic_metrics,
    topic_evolution = topic_evolution,
    topic_cooccurrence = topic_cooccurrence,
    coherence = lda_result$coherence,
    perplexity = lda_result$perplexity,
    method = "LDA",
    keywords_used = kw_col
  )
}

#' Extract keywords from column
#' @keywords internal
extract_keywords <- function(keyword_column) {
  keywords_list <- lapply(as.character(keyword_column), function(x) {
    if (is.na(x) || x == "") return(character(0))
    
    # Split by semicolon or comma
    kw <- strsplit(x, "[;,]")[[1]]
    kw <- trimws(kw)
    kw <- tolower(kw)
    kw <- gsub("[^a-z0-9 ]", "", kw)
    kw <- kw[kw != "" & nchar(kw) > 2]
    
    kw
  })
  
  names(keywords_list) <- seq_along(keywords_list)
  keywords_list
}

#' Create document-term matrix
#' @keywords internal
create_dtm <- function(keywords_list) {
  # Get all unique terms
  all_terms <- unique(unlist(keywords_list))
  n_docs <- length(keywords_list)
  n_terms <- length(all_terms)
  
  # Filter terms that appear in at least 2 documents
  term_freq <- table(unlist(keywords_list))
  all_terms <- names(term_freq[term_freq >= 2])
  
  if (length(all_terms) == 0) {
    return(NULL)
  }
  
  # Create matrix
  dtm <- matrix(0, nrow = n_docs, ncol = length(all_terms))
  colnames(dtm) <- all_terms
  
  for (i in seq_along(keywords_list)) {
    doc_terms <- keywords_list[[i]]
    doc_terms <- doc_terms[doc_terms %in% all_terms]
    if (length(doc_terms) > 0) {
      term_counts <- table(doc_terms)
      dtm[i, names(term_counts)] <- as.numeric(term_counts)
    }
  }
  
  # Ensure no entirely empty rows to prevent LDA failure
  row_sums <- rowSums(dtm)
  if (any(row_sums == 0)) {
    # Add a tiny value to prevent row sum = 0
    dtm[row_sums == 0, 1] <- 1e-10
  }
  
  dtm
}

#' Fit LDA model
#' @keywords internal
fit_lda_model <- function(dtm, n_topics) {
  if (is.null(dtm) || nrow(dtm) < 5) {
    return(list(
      status = "error: insufficient data",
      topics = data.frame()
    ))
  }
  
  # Try topicmodels package first
  lda_result <- tryCatch({
    if (requireNamespace("topicmodels", quietly = TRUE)) {
      lda_model <- topicmodels::LDA(dtm, k = n_topics, method = "Gibbs", 
                                     control = list(seed = 12345, burnin = 1000, iter = 2000))
      
      # Get posterior
      posterior <- topicmodels::posterior(lda_model)
      
      list(
        status = "success",
        model = lda_model,
        beta = posterior$terms,        # Topic-term distribution
        gamma = posterior$topics,       # Document-topic distribution
        n_topics = n_topics,
        coherence = calculate_coherence(posterior$terms, dtm),
        perplexity = tryCatch(topicmodels::perplexity(lda_model, dtm), error = function(e) NA)
      )
    } else {
      # Fallback: simple implementation
      fit_simple_lda(dtm, n_topics)
    }
  }, error = function(e) {
    fit_simple_lda(dtm, n_topics)
  })
  
  lda_result
}

#' Simple LDA implementation (fallback)
#' @keywords internal
fit_simple_lda <- function(dtm, n_topics) {
  n_docs <- nrow(dtm)
  n_terms <- ncol(dtm)
  
  # Initialize
  set.seed(12345)
  beta <- matrix(runif(n_topics * n_terms), n_topics, n_terms)
  beta <- beta / rowSums(beta)
  
  gamma <- matrix(runif(n_docs * n_topics), n_docs, n_topics)
  gamma <- gamma / rowSums(gamma)
  
  # EM iterations
  for (iter in 1:50) {
    # E-step: update gamma
    for (d in 1:n_docs) {
      for (k in 1:n_topics) {
        gamma[d, k] <- prod(beta[k, ] ^ dtm[d, ])
      }
      # Ensure scalar values and handle NA
      row_sum <- sum(gamma[d, ], na.rm = TRUE)
      if (!is.na(row_sum) && row_sum > 0) {
        gamma[d, ] <- gamma[d, ] / row_sum
      }
    }
    
    # M-step: update beta
    for (k in 1:n_topics) {
      for (v in 1:n_terms) {
        beta[k, v] <- sum(gamma[, k] * dtm[, v]) + 0.01
      }
      beta[k, ] <- beta[k, ] / sum(beta[k, ])
    }
  }
  
  # Calculate coherence
  coherence <- calculate_coherence(beta, dtm)
  
  # Calculate perplexity
  perplexity <- calculate_perplexity(beta, gamma, dtm)
  
  list(
    status = "success",
    model = NULL,
    beta = beta,
    gamma = gamma,
    n_topics = n_topics,
    coherence = coherence,
    perplexity = perplexity
  )
}

#' Calculate topic coherence
#' @keywords internal
calculate_coherence <- function(beta, dtm) {
  n_topics <- nrow(beta)
  coherence_scores <- numeric(n_topics)
  
  # Get top words per topic
  for (k in 1:n_topics) {
    top_words <- order(beta[k, ], decreasing = TRUE)[1:10]
    
    # Calculate U-mass coherence
    coherence_sum <- 0
    for (i in 2:min(10, length(top_words))) {
      for (j in 1:(i - 1)) {
        # Co-occurrence
        cooccur <- sum(dtm[, top_words[i]] > 0 & dtm[, top_words[j]] > 0, na.rm = TRUE)
        occur_j <- sum(dtm[, top_words[j]] > 0, na.rm = TRUE)
        
        if (!is.na(occur_j) && occur_j > 0) {
          coherence_sum <- coherence_sum + log((cooccur + 1) / occur_j)
        }
      }
    }
    coherence_scores[k] <- coherence_sum
  }
  
  mean(coherence_scores, na.rm = TRUE)
}

#' Calculate perplexity
#' @keywords internal
calculate_perplexity <- function(beta, gamma, dtm) {
  n_docs <- nrow(dtm)
  n_terms <- ncol(dtm)
  
  # Reconstruct document probabilities
  reconstructed <- gamma %*% beta
  
  # Calculate log-likelihood
  loglik <- sum(dtm * log(reconstructed + 1e-10), na.rm = TRUE)
  
  # Perplexity
  perplexity <- exp(-loglik / sum(dtm))
  
  perplexity
}

#' Determine optimal number of topics
#' @keywords internal
determine_optimal_topics <- function(keywords_list, max_topics = 20) {
  n_docs <- length(keywords_list)
  
  # Heuristic: sqrt of documents, but between 5 and 20
  optimal <- round(sqrt(n_docs))
  optimal <- min(optimal, max_topics)
  optimal <- max(optimal, 5)
  
  # For small datasets
  if (n_docs < 50) {
    optimal <- min(optimal, 5)
  }
  
  optimal
}

#' Extract topics from LDA result
#' @keywords internal
extract_topics <- function(lda_result, dtm, n_top_words = 10) {
  n_topics <- lda_result$n_topics
  beta <- lda_result$beta
  terms <- colnames(beta)
  
  if (is.null(terms)) {
    terms <- paste0("term", 1:ncol(beta))
  }
  
  topics_list <- list()
  
  for (k in 1:n_topics) {
    top_idx <- order(beta[k, ], decreasing = TRUE)[1:n_top_words]
    top_words <- terms[top_idx]
    top_probs <- beta[k, top_idx]
    
    # Topic label (first few words)
    label <- paste(top_words[1:min(3, length(top_words))], collapse = "_")
    
    topics_list[[k]] <- list(
      topic_id = k,
      label = label,
      top_words = top_words,
      probabilities = top_probs,
      weight = mean(lda_result$gamma[, k])
    )
  }
  
  # Convert to data frame
  if (length(topics_list) == 0) {
    return(list(topics = data.frame(), topics_list = list()))
  }
  
  topics_df <- do.call(rbind, lapply(topics_list, function(t) {
    data.frame(
      topic_id = t$topic_id,
      label = t$label,
      top_words = paste(t$top_words, collapse = ", "),
      top_probabilities = paste(round(t$probabilities, 4), collapse = ", "),
      weight = t$weight,
      stringsAsFactors = FALSE
    )
  }))
  
  if (nrow(topics_df) > 0) {
    topics_df <- topics_df[order(-topics_df$weight), ]
  }
  
  list(
    topics = topics_df,
    topics_list = topics_list
  )
}

#' Get document-topic distribution
#' @keywords internal
get_document_topics <- function(lda_result, n_docs) {
  gamma <- lda_result$gamma
  
  # Dominant topic for each document
  dominant_topic <- apply(gamma, 1, function(x) {
    if (all(is.na(x)) || length(x) == 0) return(NA_integer_)
    which.max(x)
  })
  dominant_prob <- apply(gamma, 1, function(x) {
    if (all(is.na(x)) || length(x) == 0) return(NA_real_)
    max(x, na.rm = TRUE)
  })
  
  data.frame(
    doc_id = 1:n_docs,
    dominant_topic = dominant_topic,
    dominant_prob = dominant_prob,
    stringsAsFactors = FALSE
  )
}

#' Compute topic metrics
#' @keywords internal
compute_topic_metrics <- function(lda_result, topics_df, doc_topics) {
  gamma <- lda_result$gamma
  n_topics <- nrow(topics_df)
  
  if (n_topics == 0 || is.null(gamma) || nrow(gamma) == 0) {
    return(data.frame(
      topic_id = integer(0),
      prevalence = numeric(0),
      exclusivity = numeric(0),
      entropy = numeric(0),
      doc_count = integer(0)
    ))
  }
  
  # Topic prevalence
  prevalence <- colMeans(gamma, na.rm = TRUE)
  
  # Topic exclusivity (how distinct each topic is)
  exclusivity <- numeric(n_topics)
  for (k in 1:n_topics) {
    # Find terms most associated with this topic
    top_terms <- order(lda_result$beta[k, ], decreasing = TRUE)[1:min(50, ncol(lda_result$beta))]
    
    # How exclusive are these terms to this topic?
    col_sums <- colSums(lda_result$beta, na.rm = TRUE)
    if (length(top_terms) > 0 && all(!is.na(col_sums[top_terms])) && all(col_sums[top_terms] > 0)) {
      exclusivity[k] <- mean(lda_result$beta[k, top_terms] / col_sums[top_terms], na.rm = TRUE)
    } else {
      exclusivity[k] <- 0
    }
  }
  # Topic entropy
  entropy <- apply(gamma, 2, function(x) {
    probs <- x[x > 0]
    if (length(probs) == 0) return(0)
    -sum(probs * log(probs))
  })
  
  # Document count per topic
  dominant_topic <- doc_topics$dominant_topic
  if (is.null(dominant_topic) || length(dominant_topic) == 0) {
    doc_count_per_topic <- rep(0L, n_topics)
  } else {
    doc_count_per_topic <- tabulate(dominant_topic[!is.na(dominant_topic)], nbins = n_topics)
  }
  
  data.frame(
    topic_id = 1:n_topics,
    prevalence = round(prevalence, 4),
    exclusivity = round(exclusivity, 4),
    entropy = round(entropy, 4),
    doc_count = doc_count_per_topic,
    stringsAsFactors = FALSE
  )
}

#' Compute topic evolution over time
#' @keywords internal
compute_topic_evolution <- function(doc_topics, years, keywords_list) {
  if (is.null(years) || length(years) != nrow(doc_topics)) {
    return(data.frame())
  }
  
  years <- as.numeric(years)
  valid_years <- !is.na(years)
  
  if (sum(valid_years) < 5) {
    return(data.frame())
  }
  
  years_valid <- years[valid_years]
  doc_topics_valid <- doc_topics[valid_years, ]
  
  # Group by year
  year_range <- range(years_valid)
  years_seq <- year_range[1]:year_range[2]
  
  n_topics <- max(doc_topics_valid$dominant_topic, na.rm = TRUE)
  if (is.infinite(n_topics) || n_topics < 1) {
    return(data.frame())
  }
  topic_prevalence <- matrix(0, length(years_seq), n_topics)
  rownames(topic_prevalence) <- years_seq
  
  for (y in years_seq) {
    year_docs <- doc_topics_valid$dominant_topic[years_valid == y]
    year_docs <- year_docs[!is.na(year_docs)]  # Remove NAs
    if (length(year_docs) > 0) {
      topic_prevalence[as.character(y), ] <- tabulate(year_docs, nbins = n_topics) / length(year_docs)
    }
  }
  
  # Find emerging topics
  first_half <- years_seq[1]:floor(mean(years_seq))
  second_half <- ceiling(mean(years_seq)):years_seq[length(years_seq)]
  
  early_prev <- colMeans(topic_prevalence[as.character(first_half), , drop = FALSE], na.rm = TRUE)
  late_prev <- colMeans(topic_prevalence[as.character(second_half), , drop = FALSE], na.rm = TRUE)
  
  topic_change <- late_prev - early_prev
  
  emerging <- which(topic_change > 0.05)
  declining <- which(topic_change < -0.05)
  
  list(
    prevalence_matrix = topic_prevalence,
    years = years_seq,
    early_prevalence = early_prev,
    late_prevalence = late_prev,
    change = topic_change,
    emerging_topics = emerging,
    declining_topics = declining
  )
}

#' Compute topic co-occurrence
#' @keywords internal
compute_topic_cooccurrence <- function(gamma, threshold = 0.2) {
  n_topics <- ncol(gamma)
  
  # Binary co-occurrence (topic > threshold)
  binary <- gamma > threshold
  
  # Co-occurrence matrix
  cooccur <- t(binary) %*% binary
  
  # Normalize
  cooccur_norm <- cooccur / diag(cooccur)
  
  rownames(cooccur_norm) <- colnames(cooccur_norm) <- paste0("Topic", 1:n_topics)
  
  list(
    cooccurrence = cooccur,
    cooccurrence_normalized = cooccur_norm,
    n_pairs = sum(cooccur[upper.tri(cooccur)]> 0)
  )
}

#' Compute NMF topic model (alternative)
#'
#' @param input Bibliographic data frame
#' @param config Configuration list
#' @return List with topics
#' @export
compute_m1_nmf_topics <- function(input, config = biblio_config()) {
  # Similar to LDA but uses NMF
  kw_cols <- c("KW", "DE", "ID", "Keywords")
  kw_col <- NULL
  for (col in kw_cols) {
    if (col %in% names(input)) {
      kw_col <- col
      break
    }
  }
  
  if (is.null(kw_col)) {
    return(list(status = "error: no keywords"))
  }
  
  keywords_list <- extract_keywords(input[[kw_col]])
  dtm <- create_dtm(keywords_list)
  
  if (is.null(dtm)) return(list(status = "error: DTM creation failed"))
  
  n_topics <- config$n_topics %||% determine_optimal_topics(keywords_list)
  
  #Fit NMF
  nmf_result <- tryCatch({
    if (requireNamespace("NMF", quietly = TRUE)) {
      NMF::nmf(dtm, n_topics, seed = 12345)
    } else {
      fit_simple_nmf(dtm, n_topics)
    }
  }, error = function(e) {
    fit_simple_nmf(dtm, n_topics)
  })
  
  nmf_result
}

#' Simple NMF implementation
#' @keywords internal
fit_simple_nmf <- function(dtm, n_topics, max_iter = 100) {
  n_docs <- nrow(dtm)
  n_terms <- ncol(dtm)
  
  set.seed(12345)
  W <- matrix(runif(n_docs * n_topics), n_docs, n_topics)
  H <- matrix(runif(n_topics * n_terms), n_topics, n_terms)
  
  # Multiplicative update
  for (iter in 1:max_iter) {
    # Update H
    H <- H * (t(W) %*% dtm) / (t(W) %*% W %*% H + 1e-10)
    
    # Update W
    W <- W * (dtm %*% t(H)) / (W %*% H %*% t(H) + 1e-10)
  }
  
  list(
    status = "success",
    W = W,  # Document-topic
    H = H,  # Topic-term
    n_topics = n_topics,
    coherence = calculate_coherence(H, dtm)
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b