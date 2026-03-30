# ============================================================================
# m1_render_topic_modeling.R - Topic Modeling Visualization
# ============================================================================

#' Render topic modeling results
#'
#' @param data Output from compute_m1_topic_modeling
#' @param config Configuration list
#' @return List with topic plots
#' @export
render_m1_topic_modeling <- function(data, config = biblio_config()) {
  if (is.null(data) || data$status == "error") {
    return(list(plots = list(), status = data$status %||% "error"))
  }
  
  plots <- list()
  
  if (!is.null(data$topics) && nrow(data$topics) > 0) {
    plots$topic_words <- create_topic_word_plot(data, config)
    plots$topic_distribution <- create_topic_distribution_plot(data, config)
  }
  
  if (!is.null(data$coherence)) {
    plots$coherence <- create_coherence_plot(data, config)
  }
  
  if (!is.null(data$perplexity)) {
    plots$perplexity <- create_perplexity_plot(data, config)
  }
  
  list(
    plots = plots,
    status = "success"
  )
}

#' Create topic word plot
#' @keywords internal
create_topic_word_plot <- function(data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }
  
  topics <- data$topics
  if (is.null(topics) || nrow(topics) == 0) return(NULL)
  
  n_top_words <- config$n_top_words %||% 10
  
  ggplot2::ggplot(topics, ggplot2::aes(x = reorder(term, beta), y = beta, fill = topic)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::facet_wrap(~ topic, scales = "free_y", ncol = 2) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Top Words per Topic",
      x = "Word",
      y = "Beta (Probability)"
    ) +
    ieee_theme()
}

#' Create topic distribution plot
#' @keywords internal
create_topic_distribution_plot <- function(data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }
  
  doc_topics <- data$document_topics
  if (is.null(doc_topics) || nrow(doc_topics) == 0) return(NULL)
  
  ggplot2::ggplot(doc_topics, ggplot2::aes(x = topic, y = gamma)) +
    ggplot2::geom_boxplot(fill = "#2166AC", alpha = 0.7) +
    ggplot2::labs(
      title = "Topic Distribution Across Documents",
      x = "Topic",
      y = "Gamma (Document-Topic Probability)"
    ) +
    ieee_theme()
}

#' Create coherence plot
#' @keywords internal
create_coherence_plot <- function(data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }
  
  coherence <- data$coherence
  if (is.null(coherence) || length(coherence) == 0) return(NULL)
  
  coherence_df <- data.frame(
    n_topics = names(coherence),
    coherence = unlist(coherence),
    stringsAsFactors = FALSE
  )
  
  ggplot2::ggplot(coherence_df, ggplot2::aes(x = as.integer(n_topics), y = coherence)) +
    ggplot2::geom_line(color = "#2166AC", size = 1) +
    ggplot2::geom_point(color = "#2166AC", size = 3) +
    ggplot2::labs(
      title = "Topic Model Coherence by Number of Topics",
      x = "Number of Topics",
      y = "Coherence Score"
    ) +
    ieee_theme()
}

#' Create perplexity plot
#' @keywords internal
create_perplexity_plot <- function(data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }
  
  perplexity <- data$perplexity
  if (is.null(perplexity) || length(perplexity) == 0) return(NULL)
  
  perplexity_df <- data.frame(
    n_topics = names(perplexity),
    perplexity = unlist(perplexity),
    stringsAsFactors = FALSE
  )
  
  ggplot2::ggplot(perplexity_df, ggplot2::aes(x = as.integer(n_topics), y = perplexity)) +
    ggplot2::geom_line(color = "#B2182B", size = 1) +
    ggplot2::geom_point(color = "#B2182B", size = 3) +
    ggplot2::labs(
      title = "Topic Model Perplexity by Number of Topics",
      x = "Number of Topics",
      y = "Perplexity"
    ) +
    ieee_theme()
}

#' Build topic modeling table
#' @export
build_m1_topic_modeling_table <- function(data, config = biblio_config()) {
  if (is.null(data) || data$status == "error") {
    return(data.frame(
      topic = character(),
      top_words = character(),
      n_documents = integer(),
      stringsAsFactors = FALSE
    ))
  }
  
  topics <- data$topics
  if (is.null(topics) || nrow(topics) == 0) {
    return(data.frame(
      topic = character(),
      top_words = character(),
      n_documents = integer(),
      stringsAsFactors = FALSE
    ))
  }
  
  topics
}

`%||%` <- function(a, b) if (!is.null(a)) a else b