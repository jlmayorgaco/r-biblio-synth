# ============================================================================
# m1_compute_doc_types.R - Document types metric for M1
# ============================================================================

#' Compute M1 document types
#' @param input A data frame of bibliographic data.
#' @param config A configuration list.
#' @export
compute_m1_doc_types <- function(input, config = biblio_config()) {
  if (!is.data.frame(input) || nrow(input) == 0) {
    return(list(
      doc_type_table  = m1_empty_rank_table(),
      doc_type_counts = list(),
      status          = "error"
    ))
  }

  # Try bibliometrix first, fallback to manual
  dt_counts <- tryCatch({
    cached <- get_cached_biblio_analysis(input)
    res <- cached$res
    s <- summary(res, pause = FALSE, verbose = FALSE)
    m1_extract_document_types(s$MainInformationDF)
  }, error = function(e) {
    # Fallback: count DT column directly
    if ("DT" %in% names(input)) {
      as.list(table(input$DT, useNA = "no"))
    } else {
      list(article = nrow(input))
    }
  })

  # Build table
  dt_df <- data.frame(
    type  = names(dt_counts),
    count = as.integer(unlist(dt_counts)),
    stringsAsFactors = FALSE
  )
  dt_df <- dt_df[dt_df$count > 0, ]

  total <- sum(dt_df$count)
  dt_df$percentage <- m1_compute_percentage(dt_df$count, total)
  dt_df <- dt_df[order(-dt_df$count), ]

  label_map <- get_label_mapping()
  dt_df$label <- ifelse(dt_df$type %in% names(label_map), label_map[dt_df$type], dt_df$type)

  doc_type_table <- tibble::tibble(
    rank       = seq_len(nrow(dt_df)),
    label      = dt_df$label,
    value      = dt_df$count,
    percentage = dt_df$percentage
  )

  list(
    doc_type_table  = doc_type_table,
    doc_type_counts = dt_counts,
    doc_type_percentages = stats::setNames(as.list(dt_df$percentage), dt_df$label),
    status          = "success"
  )
}
