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
  if (is.null(data) || startsWith(data$status %||% "error", "error")) {
    return(list(plots = list(), status = data$status %||% "error"))
  }

  plots <- list()

  if (!is.null(data$topic_terms_long) && nrow(data$topic_terms_long) > 0) {
    plots$topic_words <- create_topic_word_plot(data, config)
  }

  if (!is.null(data$document_topic_weights) && nrow(data$document_topic_weights) > 0) {
    plots$topic_distribution <- create_topic_distribution_plot(data, config)
  }

  if (!is.null(data$topic_evolution) && is.list(data$topic_evolution) &&
      !is.null(data$topic_evolution$prevalence_matrix) &&
      length(data$topic_evolution$prevalence_matrix) > 0) {
    plots$topic_evolution <- create_topic_evolution_plot(data, config)
  }

  if (!is.null(data$coherence) || !is.null(data$perplexity)) {
    plots$model_quality <- create_topic_quality_plot(data, config)
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

  terms <- data$topic_terms_long
  if (is.null(terms) || nrow(terms) == 0) return(NULL)

  n_top_words <- config$n_top_words %||% 10
  terms <- terms[terms$rank <= n_top_words, , drop = FALSE]
  terms$facet_term <- paste0("T", terms$topic_id, ": ", terms$term)
  terms$facet_term <- stats::reorder(terms$facet_term, terms$probability)

  ggplot2::ggplot(terms, ggplot2::aes(x = facet_term, y = probability, fill = factor(topic_id))) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::facet_wrap(~ topic_label, scales = "free_y") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Top Terms per Topic",
      x = "Term",
      y = "Topic-Term Probability"
    ) +
    ieee_theme()
}

#' Create topic distribution plot
#' @keywords internal
create_topic_distribution_plot <- function(data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }

  doc_topics <- data$document_topic_weights
  if (is.null(doc_topics) || nrow(doc_topics) == 0) return(NULL)

  ggplot2::ggplot(doc_topics, ggplot2::aes(x = factor(topic_id), y = probability)) +
    ggplot2::geom_boxplot(fill = "#2166AC", alpha = 0.75) +
    ggplot2::labs(
      title = "Document-Topic Weight Distribution",
      x = "Topic",
      y = "Document-Topic Probability"
    ) +
    ieee_theme()
}

#' Create topic evolution plot
#' @keywords internal
create_topic_evolution_plot <- function(data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }

  evolution <- data$topic_evolution
  matrix_data <- evolution$prevalence_matrix
  if (is.null(matrix_data) || length(matrix_data) == 0) return(NULL)

  evolution_df <- as.data.frame(as.table(matrix_data), stringsAsFactors = FALSE)
  names(evolution_df) <- c("year", "topic_id", "prevalence")
  evolution_df$year <- as.numeric(as.character(evolution_df$year))
  evolution_df$topic_id <- as.integer(gsub("[^0-9]", "", as.character(evolution_df$topic_id)))
  evolution_df$topic <- paste0("Topic ", evolution_df$topic_id)

  ggplot2::ggplot(evolution_df, ggplot2::aes(x = year, y = prevalence, color = topic)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::labs(
      title = "Topic Prevalence Over Time",
      x = "Year",
      y = "Prevalence",
      color = "Topic"
    ) +
    ggplot2::theme(legend.position = "bottom") +
    ieee_theme()
}

#' Create topic quality plot
#' @keywords internal
create_topic_quality_plot <- function(data, config) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }

  quality_df <- data.frame(
    metric = c("Coherence", "Perplexity"),
    value = c(as.numeric(data$coherence %||% NA_real_), as.numeric(data$perplexity %||% NA_real_)),
    stringsAsFactors = FALSE
  )
  quality_df <- quality_df[!is.na(quality_df$value), , drop = FALSE]
  if (nrow(quality_df) == 0) return(NULL)

  ggplot2::ggplot(quality_df, ggplot2::aes(x = metric, y = value, fill = metric)) +
    ggplot2::geom_col(show.legend = FALSE, alpha = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = round(value, 3)), vjust = -0.4, size = 3) +
    ggplot2::labs(
      title = "Topic Model Quality Metrics",
      x = NULL,
      y = "Value"
    ) +
    ieee_theme()
}

#' Build topic modeling table
#' @export
build_m1_topic_modeling_table <- function(data, config = biblio_config()) {
  empty <- list(
    status = "stub",
    table = tibble::tibble(),
    summary = list()
  )

  if (is.null(data) || startsWith(data$status %||% "error", "error")) {
    return(empty)
  }

  topics <- tibble::as_tibble(data$topics %||% data.frame())
  metrics <- tibble::as_tibble(data$topic_metrics %||% data.frame())
  if (nrow(topics) == 0) {
    return(empty)
  }

  if (nrow(metrics) > 0 && "topic_id" %in% names(topics) && "topic_id" %in% names(metrics)) {
    topics <- dplyr::left_join(topics, metrics, by = "topic_id")
  }

  if ("doc_count" %in% names(topics)) {
    topics <- topics[order(-topics$doc_count, -topics$weight), , drop = FALSE]
  } else if ("weight" %in% names(topics)) {
    topics <- topics[order(-topics$weight), , drop = FALSE]
  }

  list(
    status = "success",
    table = topics,
    summary = list(
      n_topics = data$n_topics %||% nrow(topics),
      coherence = data$coherence %||% NA_real_,
      perplexity = data$perplexity %||% NA_real_,
      keyword_source = data$keywords_used %||% NA_character_
    )
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
