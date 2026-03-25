# ============================================================================
# m1_render_citations.R - Render citations plots
# ============================================================================

#' Render M1 citations plots
#'
#' @param result The compute result for citations.
#' @param config A configuration list.
#' @return A list with \code{status}, \code{plots}, \code{tables}.
#' @export
render_m1_citations <- function(result, config = biblio_config()) {
  if (!inherits(result, "list") || !"top_cited_documents" %in% names(result)) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  top_cited <- result$top_cited_documents
  if (nrow(top_cited) == 0) {
    return(list(status = "stub", plots = list(), tables = list()))
  }

  palette <- get_biblio_palette("main")

  # Bar plot
  bar_plot <- ggplot2::ggplot(top_cited, ggplot2::aes(x = reorder(label, value), y = value)) +
    ggplot2::geom_bar(stat = "identity", fill = palette[1], color = "black", linewidth = 0.3) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Top Most Cited Papers",
      x = "Paper",
      y = "Total Citations"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
      axis.text = ggplot2::element_text(size = 10)
    )

  plots <- list(bar = bar_plot)

  # Citations per year plot
  if ("citations_per_year" %in% names(result) && nrow(result$citations_per_year) > 0) {
    cpy <- result$citations_per_year
    cpy_plot <- ggplot2::ggplot(cpy, ggplot2::aes(x = reorder(label, value), y = value)) +
      ggplot2::geom_bar(stat = "identity", fill = palette[2], color = "black", linewidth = 0.3) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Top Papers by Citations per Year",
        x = "Paper",
        y = "Citations per Year"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5)
      )
    plots$citations_per_year <- cpy_plot

    # Dual bar+line plot (legacy F4d feature)
    dual_plot <- m1_render_citations_dual_plot(top_cited, cpy, palette)
    if (!is.null(dual_plot)) {
      plots$dual_bar_line <- dual_plot
    }
  }

  list(
    status = "success",
    plots  = plots,
    tables = list()
  )
}

#' Generate dual bar+line plot for citations
#'
#' Creates a bar plot with TC and overlays TCperYear as a line on secondary axis.
#'
#' @param top_cited Data frame with rank, label, value columns.
#' @param cpy Data frame with rank, label, value columns for citations per year.
#' @param palette Color palette.
#' @return A ggplot object or NULL.
m1_render_citations_dual_plot <- function(top_cited, cpy, palette) {
  # Merge data
  merged <- merge(top_cited, cpy, by = "label", suffixes = c("_tc", "_cpy"))
  if (nrow(merged) == 0) return(NULL)

  ggplot2::ggplot(merged, ggplot2::aes(x = reorder(label, value_tc))) +
    ggplot2::geom_bar(ggplot2::aes(y = value_tc), stat = "identity",
                      fill = palette[1], color = "black", linewidth = 0.3) +
    ggplot2::geom_line(ggplot2::aes(y = value_cpy * max(value_tc) / max(value_cpy), group = 1),
                       color = palette[2], linewidth = 1.2) +
    ggplot2::geom_point(ggplot2::aes(y = value_cpy * max(value_tc) / max(value_cpy)),
                        color = palette[2], size = 3) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      name = "Total Citations",
      sec.axis = ggplot2::sec_axis(~ . * max(merged$value_cpy) / max(merged$value_tc),
                                   name = "Citations per Year")
    ) +
    ggplot2::labs(
      title = "Top Cited Papers with TC per Year",
      x = "Paper"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
      axis.text = ggplot2::element_text(size = 10)
    )
}
