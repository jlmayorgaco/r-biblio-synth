# ============================================================================
# m1_table_semantic_advanced.R - Tables for M1 semantic advanced layer
# ============================================================================

build_m1_semantic_advanced_table <- function(result, config = biblio_config()) {
  if (!is.list(result)) {
    return(list(status = "stub", table = tibble::tibble(), tables = list()))
  }
  tables <- list(
    synonym_dictionary = result$synonym_dictionary %||% tibble::tibble(),
    coword_nodes = result$co_word_network$nodes %||% tibble::tibble(),
    coword_edges = result$co_word_network$edges %||% tibble::tibble(),
    thematic_map = result$thematic_map %||% tibble::tibble(),
    thematic_evolution = result$thematic_evolution %||% tibble::tibble(),
    topic_stability = result$topic_stability %||% tibble::tibble()
  )
  main <- dplyr::bind_rows(lapply(names(tables), function(nm) {
    tbl <- tables[[nm]]
    tibble::tibble(
      table_name = nm,
      rows = if (is.data.frame(tbl)) nrow(tbl) else 0L,
      status = if (is.data.frame(tbl) && nrow(tbl) > 0) "available" else "empty"
    )
  }))
  list(status = result$status %||% "success", reason = result$reason %||% NA_character_, table = main, tables = tables)
}
