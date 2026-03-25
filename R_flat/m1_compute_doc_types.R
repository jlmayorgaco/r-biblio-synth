# ============================================================================
# m1_compute_doc_types.R - Document types metric for M1
# ============================================================================

#' Compute M1 document types
#'
#' @param input A data frame of bibliographic data.
#' @param config A configuration list.
#' @return A list with \code{doc_type_table}, \code{doc_type_counts}, \code{status}.
#' @export
compute_m1_doc_types <- function(input, config = biblio_config()) {
  if (!is.data.frame(input) || nrow(input) == 0) {
    return(list(
      doc_type_table  = m1_empty_rank_table(),
      doc_type_counts = list(),
      status          = "error"
    ))
  }

  # Count document types
  if ("DT" %in% names(input)) {
    dt_counts <- table(input$DT, useNA = "no")
    dt_df <- data.frame(
      type  = names(dt_counts),
      count = as.integer(dt_counts),
      stringsAsFactors = FALSE
    )
  } else {
    dt_df <- data.frame(type = "Unknown", count = nrow(input), stringsAsFactors = FALSE)
  }

  # Calculate percentages
  total <- sum(dt_df$count)
  dt_df$percentage <- m1_compute_percentage(dt_df$count, total)

  # Sort by count descending
  dt_df <- dt_df[order(-dt_df$count), ]

  # Apply label mapping
  label_map <- get_label_mapping()
  dt_df$label <- ifelse(dt_df$type %in% names(label_map),
                        label_map[dt_df$type], dt_df$type)

  # Build doc_type_table
  doc_type_table <- tibble::tibble(
    rank       = seq_len(nrow(dt_df)),
    label      = dt_df$label,
    value      = dt_df$count,
    percentage = dt_df$percentage
  )

  # Build doc_type_counts list
  doc_type_counts <- stats::setNames(
    as.list(dt_df$count),
    dt_df$type
  )

  list(
    doc_type_table  = doc_type_table,
    doc_type_counts = doc_type_counts,
    doc_type_percentages = stats::setNames(
      as.list(dt_df$percentage),
      dt_df$label
    ),
    status          = "success"
  )
}
