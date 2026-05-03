# ============================================================================
# m1_render_keyword_burst.R - Render Keyword Burst Detection Results
# ============================================================================

#' Render keyword burst detection plots
#'
#' @param data Output from compute_m1_keyword_burst
#' @param config Configuration list
#' @return List with plots
#' @export
render_m1_keyword_burst <- function(data, config = biblio_config()) {
  if (is.null(data) || data$status != "success") {
    return(list(plots = list(), status = "error: invalid data"))
  }
  
  plots <- list()
  
  # 1. Timeline of all bursts
  if (nrow(data$bursts) > 0) {
    plots$burst_timeline <- create_burst_timeline_plot(data, config)
  }
  
  # 2. Burst intensity heatmap
  if (length(data$keyword_bursts) > 0) {
    plots$burst_heatmap <- create_burst_heatmap(data, config)
  }
  
  # 3. Top bursty keywords
  if (nrow(data$bursts) > 0) {
    plots$top_bursts <- create_top_bursts_plot(data, config)
  }
  
  # 4. Burst timing categories
  if (!is.null(data$timing_categories)) {
    plots$timing_categories <- create_timing_categories_plot(data, config)
  }
  
  # 5. Emerging vs declining keywords
  if (length(data$keyword_bursts) > 0) {
    plots$emerging_keywords <- create_emerging_keywords_plot(data, config)
  }
  
  # 6. Burst frequency over time
  if (nrow(data$bursts) > 0) {
    plots$burst_frequency <- create_burst_frequency_plot(data, config)
  }
  
  list(
    plots = plots,
    status = "success"
  )
}

#' Create burst timeline plot
#' @keywords internal
create_burst_timeline_plot <- function(data, config) {
  burst_df <- data$bursts
  
  # Take top 20 by strength
  top_bursts <- head(burst_df[order(-burst_df$strength), ], 20)
  
  if (nrow(top_bursts) == 0) return(NULL)
  
  # Create interval data
  top_bursts$keyword <- factor(top_bursts$keyword, levels = unique(top_bursts$keyword))
  
  p <- ggplot2::ggplot(top_bursts) +
    ggplot2::geom_segment(
      ggplot2::aes(x = start_year, xend = end_year, y = keyword, yend = keyword,
                   color = strength, linewidth = height),
      arrow = ggplot2::arrow(length = ggplot2::unit(0.1, "inches"), type = "closed")
    ) +
    ggplot2::scale_color_gradient(low = "#FED876", high = "#FB7E06", name = "Strength") +
    ggplot2::scale_linewidth_continuous(range = c(1, 3), name = "Height") +
    ggplot2::scale_x_continuous(name = "Year", expand = ggplot2::expansion(mult = c(0.05, 0.1))) +
    ieee_theme() +
    ggplot2::labs(
      title = "Keyword Burst Timeline",
      subtitle = "Showing top 20 bursty keywords over time"
    )
  
  p
}

#' Create burst heatmap
#' @keywords internal
create_burst_heatmap_plot <- function(data, config) {
  create_burst_heatmap(data, config)
}

create_burst_heatmap <- function(data, config) {
  years <- data$years
  keywords <- names(data$keyword_bursts)
  
  if (length(keywords) == 0) return(NULL)
  
  # Limit to top keywords
  top_n <- config$top_n_keywords %||% 30
  
  burst_heights <- sapply(keywords, function(kw) {
    data$keyword_bursts[[kw]]$max_height
  })
  top_keywords <- names(sort(burst_heights, decreasing = TRUE))[1:min(top_n, length(keywords))]
  
  # Create matrix
  heatmap_matrix <- matrix(0, 
                           nrow = length(top_keywords), 
                           ncol = length(years),
                           dimnames = list(top_keywords, years))
  
  for (kw in top_keywords) {
    if (!(kw %in% names(data$keyword_bursts))) next
    x <- data$keyword_bursts[[kw]]
    
    # Fill with burst states
    for (t in seq_along(years)) {
      if (!is.null(x$states) && length(x$states) >= t) {
        heatmap_matrix[kw, t] <- x$states[t]  # 0 = normal, 1 = burst
      }
    }
  }
  
  # Melt for ggplot
  heatmap_df <- reshape2::melt(heatmap_matrix)
  names(heatmap_df) <- c("keyword", "year", "state")
  
  heatmap_df$keyword <- factor(heatmap_df$keyword, levels = rev(top_keywords))
  
  p <- ggplot2::ggplot(heatmap_df, ggplot2::aes(x = year, y = keyword, fill = factor(state))) +
    ggplot2::geom_tile(color = "white", linewidth = 0.1) +
    ggplot2::scale_fill_manual(
      values = c("0" = "#F7FBFF", "1" = "#084594"),
      name = "State",
      labels = c("Normal", "Burst")
    ) +
    ggplot2::scale_x_continuous(name = "Year") +
    ggplot2::scale_y_discrete(name = NULL) +
    ieee_theme() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 6),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    ) +
    ggplot2::labs(
      title = "Keyword Burst Heatmap",
      subtitle = sprintf("Top %d bursty keywords", length(top_keywords))
    )
  
  p
}

#' Create top bursts bar plot
#' @keywords internal
create_top_bursts_plot <- function(data, config) {
  burst_df <- data$bursts
  
  top_n <- min(20, nrow(burst_df))
  top_bursts <- head(burst_df[order(-burst_df$strength), ], top_n)
  
  top_bursts$label <- sprintf("%s (%d-%d)", top_bursts$keyword, 
                              top_bursts$start_year, top_bursts$end_year)
  top_bursts$label <- factor(top_bursts$label, levels = rev(top_bursts$label))
  
  p <- ggplot2::ggplot(top_bursts, ggplot2::aes(x = label, y = strength)) +
    ggplot2::geom_col(fill = "#0072BD", color = "black", linewidth = 0.2, width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f", strength)), hjust = -0.1, size = 2.5) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(name = "Burst Strength", expand = ggplot2::expansion(mult = c(0, 0.2))) +
    ieee_theme_bar() +
    ggplot2::labs(
      title = "Top Keyword Bursts",
      subtitle = "Strength = frequency ratio during burst",
      x = NULL
    )
  
  p
}

#' Create timing categories plot
#' @keywords internal
create_timing_categories_plot <- function(data, config) {
  timing <- data$timing_categories
  
  if (is.null(timing) || is.null(timing$summary)) return(NULL)
  
  df <- data.frame(
    category = c("Emerging", "Growing", "Declining", "Bursty"),
    count = c(
      timing$summary$n_emerging,
      timing$summary$n_growing,
      timing$summary$n_declining,
      timing$summary$n_bursty
    )
  )
  
  df$category <- factor(df$category, levels = c("Emerging", "Growing", "Declining", "Bursty"))
  
  colors <- c("Emerging" = "#0072BD", "Growing" = "#77AC30", 
              "Declining" = "#A2142F", "Bursty" = "#7E2F8E")
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = category, y = count, fill = category)) +
    ggplot2::geom_col(color = "black", linewidth = 0.2, width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = count), vjust = -0.3, size = 3) +
    ggplot2::scale_fill_manual(values = colors, guide = "none") +
    ggplot2::scale_y_continuous(name = "Number of Keywords", expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ieee_theme_bar() +
    ggplot2::labs(
      title = "Keyword Burst Timing Categories",
      subtitle = sprintf("Total bursty keywords: %d", 
                        timing$summary$n_emerging + timing$summary$n_growing + timing$summary$n_declining),
      x = NULL
    )
  
  p
}

#' Create emerging keywords plot
#' @keywords internal
create_emerging_keywords_plot <- function(data, config) {
  timing <- data$timing_categories
  
  if (is.null(timing) || length(timing$emerging) == 0) return(NULL)
  
  emerging_kw <- timing$emerging
  
  # Get frequencies for emerging keywords
  years <- data$years
  top_n <- min(10, length(emerging_kw))
  
  freq_data <- do.call(rbind, lapply(emerging_kw[1:top_n], function(kw) {
    if (!(kw %in% names(data$keyword_bursts))) return(NULL)
    x <- data$keyword_bursts[[kw]]
    data.frame(
      keyword = kw,
      year = years,
      frequency = as.numeric(x$frequencies),
      stringsAsFactors = FALSE
    )
  }))
  
  if (is.null(freq_data) || nrow(freq_data) == 0) return(NULL)
  
  freq_data$keyword <- factor(freq_data$keyword, levels = emerging_kw[1:top_n])
  
  p <- ggplot2::ggplot(freq_data, ggplot2::aes(x = year, y = frequency, color = keyword)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::scale_x_continuous(name = "Year") +
    ggplot2::scale_y_continuous(name = "Frequency", expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ggplot2::scale_color_discrete(name = "Keyword") +
    ieee_theme_timeseries() +
    ggplot2::labs(
      title = "Emerging Keywords",
      subtitle = "Keywords with recent bursts (last 3 years)"
    ) +
    ggplot2::theme(legend.position = "bottom")
  
  p
}

#' Create burst frequency over time plot
#' @keywords internal
create_burst_frequency_plot <- function(data, config) {
  years <- data$years
  
  # Count bursts per year
  burst_counts <- integer(length(years))
  names(burst_counts) <- years
  
  for (kw in names(data$keyword_bursts)) {
    x <- data$keyword_bursts[[kw]]
    if (length(x$bursts) == 0) next
    
    for (b in x$bursts) {
      for (y in b$start_year:b$end_year) {
        y_str <- as.character(y)
        if (y_str %in% names(burst_counts)) {
          burst_counts[y_str] <- burst_counts[y_str] + 1
        }
      }
    }
  }
  
  burst_df <- data.frame(
    year = years,
    count = as.numeric(burst_counts)
  )
  
  p <- ggplot2::ggplot(burst_df, ggplot2::aes(x = year, y = count)) +
    ggplot2::geom_col(fill = "#0072BD", color = "black", linewidth = 0.2, width = 0.7) +
    ggplot2::geom_smooth(method = "loess", se = FALSE, color = "#D95319", linetype = "dashed", linewidth = 0.5) +
    ggplot2::scale_x_continuous(name = "Year") +
    ggplot2::scale_y_continuous(name = "Number of Bursts", expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ieee_theme_timeseries() +
    ggplot2::labs(
      title = "Burst Activity Over Time",
      subtitle = "Count of active keyword bursts per year"
    )
  
  p
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
