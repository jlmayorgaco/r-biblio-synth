# ============================================================================
# m4_tables.R - M4 Institutional Analysis Tables
# ============================================================================

#' Build M4 production table
#' @export
build_m4_production_table <- function(data, config = biblio_config()) {
  if (is.null(data) || data$status != "success") {
    return(data.frame())
  }
  
  data$institutions %>%
    dplyr::select(
      Rank = rank,
      Institution = institution_canonical,
      Sector = sector,
      Papers = n_papers,
      Authors = n_authors,
      Countries = n_countries,
      Top_Country = top_country,
      Percentage = pct_of_total
    ) %>%
    dplyr::mutate(Percentage = round(Percentage, 2))
}

#' Build M4 collaboration table
#' @export
build_m4_collaboration_table <- function(data, config = biblio_config()) {
  if (is.null(data) || data$status != "success") {
    return(data.frame())
  }
  
  data$institution_stats %>%
    dplyr::select(
      Institution = institution,
      Collaborators = n_collaborators,
      Total_Collaborations = total_collaborations
    ) %>%
    dplyr::arrange(dplyr::desc(Total_Collaborations))
}

#' Build M4 ranking table
#' @export
build_m4_ranking_table <- function(data, config = biblio_config()) {
  if (is.null(data) || data$status != "success") {
    return(data.frame())
  }
  
  data$rankings$composite %>%
    dplyr::select(
      Composite_Rank = composite_rank,
      Institution = institution,
      Paper_Rank = paper_rank,
      Author_Rank = author_rank,
      Geography_Rank = geography_rank,
      Avg_Rank = avg_rank
    ) %>%
    dplyr::arrange(Composite_Rank)
}