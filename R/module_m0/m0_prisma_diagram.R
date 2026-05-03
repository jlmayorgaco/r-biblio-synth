# ============================================================================
# m0_prisma_diagram.R - PRISMA 2020 flow diagram rendering with ggplot2
# ============================================================================

#' Render PRISMA 2020 flow diagram
#'
#' Creates a PRISMA-inspired flow diagram using ggplot2 so it exports reliably
#' to PNG, SVG, and PDF in headless environments.
#'
#' @param prisma_data A list with PRISMA flow counts.
#' @param config Configuration list.
#' @return A ggplot object.
#' @export
m0_render_prisma_diagram <- function(prisma_data, config = biblio_config()) {
  config <- merge_biblio_config(config)

  id <- prisma_data$identification %||% list()
  sc <- prisma_data$screening %||% list()
  el <- prisma_data$eligibility %||% list()
  inc <- prisma_data$included %||% list()
  qa <- prisma_data$quality %||% list()

  db_records <- as.integer(id$records_database %||% 0)
  other_records <- as.integer(id$records_other %||% 0)
  duplicates <- as.integer(id$duplicates_removed %||% 0)
  screened <- as.integer(sc$records_screened %||% 0)
  excluded_screening <- as.integer(sc$excluded_screening %||% 0)
  fulltext <- as.integer(el$fulltext_assessed %||% 0)
  excluded_fulltext <- as.integer(el$excluded_fulltext %||% 0)
  included <- as.integer(inc$studies_included %||% 0)

  type_counts <- m0_filter_named_counts(inc$by_type)
  type_summary <- if (length(type_counts) > 0) {
    type_names <- names(type_counts)
    keep_n <- min(4, length(type_names))
    shown <- vapply(
      type_names[seq_len(keep_n)],
      function(tp) sprintf("%s (n = %s)", tp, type_counts[[tp]]),
      character(1)
    )
    if (length(type_names) > keep_n) {
      shown <- c(shown, sprintf("+ %s more types", length(type_names) - keep_n))
    }
    paste(shown, collapse = " | ")
  } else {
    NULL
  }

  reason_counts <- m0_filter_named_counts(el$excluded_reasons)
  reason_summary <- if (length(reason_counts) > 0) {
    paste(
      vapply(
        names(reason_counts),
        function(reason) sprintf("%s: %s", reason, reason_counts[[reason]]),
        character(1)
      ),
      collapse = "\n"
    )
  } else {
    "No exclusion reasons recorded."
  }

  quality_summary <- NULL
  if (!is.null(qa$tool) && nzchar(qa$tool %||% "")) {
    quality_summary <- paste(
      sprintf("Tool: %s", qa$tool),
      sprintf("Low risk: %s", qa$low_risk %||% 0),
      sprintf("High risk: %s", qa$high_risk %||% 0),
      sprintf("Unclear: %s", qa$unclear %||% 0),
      sep = "\n"
    )
  }

  box_data <- data.frame(
    id = c(
      "db", "other", "dedup", "screened",
      "excluded_screening", "fulltext",
      "excluded_fulltext", "included"
    ),
    xmin = c(0.8, 5.6, 5.6, 0.8, 5.6, 0.8, 5.6, 0.8),
    xmax = c(4.2, 9.0, 9.0, 4.2, 9.0, 4.2, 9.0, 4.2),
    ymin = c(10.6, 10.6, 8.9, 7.3, 7.3, 5.0, 5.0, 2.6),
    ymax = c(11.8, 11.8, 10.1, 8.5, 8.5, 6.2, 6.2, 3.8),
    fill = c(
      "#D9E8F5", "#D9E8F5", "#E6E6E6", "#E6DDF8",
      "#F9D5D3", "#D9F0E4", "#F9D5D3", "#FCE3BF"
    ),
    stringsAsFactors = FALSE
  )

  box_data$label <- c(
    sprintf("Records identified\nfrom databases\n(n = %s)", db_records),
    sprintf("Records identified\nfrom other sources\n(n = %s)", other_records),
    sprintf("Duplicates removed\n(n = %s)", duplicates),
    sprintf("Records screened\n(n = %s)", screened),
    sprintf("Records excluded at\n title/abstract stage\n(n = %s)", excluded_screening),
    sprintf("Full-text reports assessed\nfor eligibility\n(n = %s)", fulltext),
    sprintf("Reports excluded after\nfull-text review\n(n = %s)\n\n%s", excluded_fulltext, reason_summary),
    sprintf("Studies included in synthesis\n(n = %s)", included)
  )

  if (!is.null(quality_summary)) {
    box_data <- rbind(
      box_data,
      data.frame(
        id = "quality",
        xmin = 5.6,
        xmax = 9.0,
        ymin = 2.6,
        ymax = 3.8,
        fill = "#DCE8FA",
        label = paste("Quality assessment", quality_summary, sep = "\n\n"),
        stringsAsFactors = FALSE
      )
    )
  }

  stage_data <- data.frame(
    x = c(0.2, 0.2, 0.2, 0.2),
    y = c(11.2, 8.0, 5.6, 3.2),
    label = c("Identification", "Screening", "Eligibility", "Included"),
    stringsAsFactors = FALSE
  )

  arrow_data <- data.frame(
    x = c(2.5, 7.3, 2.5, 2.5, 4.2, 2.5, 4.2, 2.5),
    y = c(10.6, 10.6, 8.9, 7.3, 7.9, 5.0, 5.6, 2.6),
    xend = c(2.5, 7.3, 2.5, 2.5, 5.6, 2.5, 5.6, if (!is.null(quality_summary)) 5.6 else 2.5),
    yend = c(10.1, 10.1, 8.5, 6.2, 7.9, 3.8, 5.6, if (!is.null(quality_summary)) 3.2 else 3.8),
    stringsAsFactors = FALSE
  )

  exclusion_arrows <- data.frame(
    x = c(4.2, 4.2),
    y = c(7.9, 5.6),
    xend = c(5.6, 5.6),
    yend = c(7.9, 5.6),
    stringsAsFactors = FALSE
  )

  title_text <- "PRISMA 2020 Flow Diagram"
  subtitle_text <- if (!is.null(prisma_data$title) && nzchar(prisma_data$title)) {
    prisma_data$title
  } else {
    "Study identification, screening, eligibility, and inclusion summary"
  }
  footer_text <- if (!is.null(type_summary)) paste("Included types:", type_summary) else NULL

  ggplot2::ggplot() +
    ggplot2::geom_curve(
      data = arrow_data,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      curvature = 0,
      arrow = grid::arrow(length = grid::unit(0.16, "inches"), type = "closed"),
      linewidth = 0.45,
      color = "#5E5E5E"
    ) +
    ggplot2::geom_curve(
      data = exclusion_arrows,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      curvature = 0,
      arrow = grid::arrow(length = grid::unit(0.16, "inches"), type = "closed"),
      linewidth = 0.45,
      color = "#8A4F4D"
    ) +
    ggplot2::geom_rect(
      data = box_data,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
      color = "#4C4C4C",
      linewidth = 0.45
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::geom_text(
      data = transform(box_data, x = (xmin + xmax) / 2, y = (ymin + ymax) / 2),
      ggplot2::aes(x = x, y = y, label = label),
      size = 2.9,
      family = "sans",
      lineheight = 0.95
    ) +
    ggplot2::geom_text(
      data = stage_data,
      ggplot2::aes(x = x, y = y, label = label),
      hjust = 0,
      size = 3.6,
      fontface = "bold",
      color = "#3C3C3C"
    ) +
    {
      if (!is.null(footer_text)) {
        ggplot2::annotate("text", x = 4.9, y = 2.15, label = footer_text, size = 2.5, color = "#4C4C4C")
      }
    } +
    ggplot2::coord_cartesian(xlim = c(0, 9.4), ylim = c(1.8, 12.2), clip = "off") +
    ggplot2::labs(
      title = title_text,
      subtitle = subtitle_text
    ) +
    ggplot2::theme_void(base_family = "sans") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5, color = "#4C4C4C"),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin = ggplot2::margin(12, 16, 12, 16)
    )
}
