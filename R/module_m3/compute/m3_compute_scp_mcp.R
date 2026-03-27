# ============================================================================
# m3_compute_scp_mcp.R - SCP/MCP metrics for M3
# ============================================================================

#' Compute SCP (Single Country Publications) and MCP (Multiple Country Publications) metrics
#'
#' @param prepared_data Output from \code{prepare_m3_country_data}
#' @param config A configuration list (see \code{biblio_config})
#' @return A list containing SCP/MCP metrics
#' @export
m3_compute_scp_mcp <- function(prepared_data, config = biblio_config()) {
  # Extract the country-doc level data
  country_doc_level <- prepared_data$country_doc_level
  
  if (nrow(country_doc_level) == 0) {
    return(list(
      scp_mcp = tibble::tibble(),
      scp_mcp_summary = list(
        total_scp = 0,
        total_mcp = 0,
        mcp_ratio = NA_real_
      ),
      status = "error: no country-doc level data"
    ))
  }
  
  # We need to compute, for each document, the number of distinct countries.
  # Then, for each document with exactly one country, assign SCP to that country.
  # For each document with more than one country, assign MCP to each country in that document.
  
  # Step 1: Compute the number of countries per document
  doc_country_counts <- country_doc_level %>%
    dplyr::group_by(doc_id) %>%
    dplyr::summarise(
      n_countries = dplyr::n_distinct(country),
      .groups = "drop"
    )
  
  # Step 2: Identify SCP documents (n_countries == 1) and get the country
  scp_documents <- doc_country_counts %>%
    dplyr::filter(n_countries == 1) %>%
    dplyr::inner_join(country_doc_level, by = "doc_id") %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(
      scp = dplyr::n(),
      .groups = "drop"
    )
  
  # Step 3: Identify MCP documents (n_countries > 1) and get the countries
  mcp_documents <- doc_country_counts %>%
    dplyr::filter(n_countries > 1) %>%
    dplyr::inner_join(country_doc_level, by = "doc_id") %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(
      mcp = dplyr::n(),
      .groups = "drop"
    )
  
  # Step 4: Combine SCP and MCP counts by country
  # We start with the country summary (which has article_count) to ensure we have all countries.
  country_summary <- prepared_data$country_summary %>%
    dplyr::select(country, article_count)
  
  # Left join SCP and MCP counts
  scp_mcp_df <- country_summary %>%
    dplyr::left_join(scp_documents, by = "country") %>%
    dplyr::left_join(mcp_documents, by = "country") %>%
    dplyr::mutate(
      scp = dplyr::coalesce(scp, 0),
      mcp = dplyr::coalesce(mcp, 0)
    ) %>%
    # Verify that article_count equals scp + mcp (should, by definition)
    dplyr::mutate(
      # We can add a check if we want, but for now we just compute the ratio.
      mcp_ratio = ifelse((scp + mcp) > 0, round(mcp / (scp + mcp) * 100, 1), NA_real_)
    ) %>%
    dplyr::arrange(desc(scp + mcp)) %>%
    dplyr::transmute(
      country = country,
      article_count = article_count,
      scp = scp,
      mcp = mcp,
      mcp_ratio = mcp_ratio
    )
  
  # Top N countries by SCP
  top_n <- config$top_n_countries
  top_countries_by_scp <- scp_mcp_df %>%
    dplyr::arrange(desc(scp)) %>%
    dplyr::slice(1:min(top_n, nrow(.))) %>%
    dplyr::transmute(
      rank = dplyr::row_number(),
      label = country,
      value = scp
    )
  
  # Top N countries by MCP
  top_countries_by_mcp <- scp_mcp_df %>%
    dplyr::arrange(desc(mcp)) %>%
    dplyr::slice(1:min(top_n, nrow(.))) %>%
    dplyr::transmute(
      rank = dplyr::row_number(),
      label = country,
      value = mcp
    )
  
  # Top N countries by MCP ratio (only those with at least one article, which is all)
  top_countries_by_mcp_ratio <- scp_mcp_df %>%
    dplyr::filter(!is.na(mcp_ratio)) %>%
    dplyr::arrange(desc(mcp_ratio)) %>%
    dplyr::slice(1:min(top_n, nrow(.))) %>%
    dplyr::transmute(
      rank = dplyr::row_number(),
      label = country,
      value = mcp_ratio
    )
  
  # Summary statistics
  total_scp <- sum(scp_mcp_df$scp)
  total_mcp <- sum(scp_mcp_df$mcp)
  overall_mcp_ratio <- ifelse((total_scp + total_mcp) > 0, round(total_mcp / (total_scp + total_mcp) * 100, 1), NA_real_)
  
  scp_mcp_summary <- list(
    total_scp = total_scp,
    total_mcp = total_mcp,
    mcp_ratio = overall_mcp_ratio
  )
  
  return(list(
    scp_mcp = scp_mcp_df,
    top_countries_by_scp = top_countries_by_scp,
    top_countries_by_mcp = top_countries_by_mcp,
    top_countries_by_mcp_ratio = top_countries_by_mcp_ratio,
    scp_mcp_summary = scp_mcp_summary,
    status = "success"
  ))
}