# ============================================================================
# m1_compute_author_indices.R - Author Impact Indices
# ============================================================================
# Computes h-index, g-index, m-index (m-quotient) for authors.
#
# h-index: An author has h-index n if n of their papers have at least n citations
# g-index: An author has g-index n if the top g papers have at least g^2 citations
# m-index (m-quotient): h-index / years since first publication
#
# IEEE Q1 Enhancement: Includes 10-index, hI-index (normalized), and comprehensive
# citation statistics for robust bibliometric analysis.

#' Compute author impact indices (h-index, g-index, m-index)
#'
#' @param input Bibliographic data frame
#' @param config Configuration list
#' @return List with author indices
#' @export
compute_m1_author_indices <- function(input, config = biblio_config()) {
  validate_is_data_frame(input)
  
  tc_col <- "TC"
  py_col <- "PY"
  au_col <- "AU"
  if (!au_col %in% names(input)) au_col <- "AF"
  
  required_cols <- c(tc_col, au_col)
  missing <- setdiff(required_cols, names(input))
  if (length(missing) > 0) {
    return(list(
      indices = data.frame(),
      summary = list(),
      status = paste("error: missing columns:", paste(missing, collapse = ", "))
    ))
  }
  
  author_papers <- extract_author_papers(input, au_col, tc_col, py_col)
  
  if (is.null(author_papers) || nrow(author_papers) == 0) {
    return(list(
      indices = data.frame(),
      summary = list(),
      status = "error: could not extract author papers"
    ))
  }
  
  indices_df <- compute_all_author_indices(author_papers)
  
  if (nrow(indices_df) == 0) {
    return(list(
      indices = data.frame(),
      summary = list(),
      status = "error: no authors with valid data"
    ))
  }
  
  top_n <- config$top_n_authors %||% 10
  
  indices_df <- indices_df[order(-indices_df$h_index), ]
  
  summary_stats <- list(
    total_authors = nrow(indices_df),
    h_index_mean = mean(indices_df$h_index, na.rm = TRUE),
    h_index_median = median(indices_df$h_index, na.rm = TRUE),
    h_index_max = max(indices_df$h_index, na.rm = TRUE),
    h_index_90th = quantile(indices_df$h_index, 0.9, na.rm = TRUE),
    g_index_mean = mean(indices_df$g_index, na.rm = TRUE),
    g_index_median = median(indices_df$g_index, na.rm = TRUE),
    m_quotient_mean = mean(indices_df$m_quotient, na.rm = TRUE),
    highly_productive = sum(indices_df$n_papers >= 10),
    h_authors = sum(indices_df$h_index >= 10)
  )
  
  top_h <- head(indices_df[order(-indices_df$h_index), ], top_n)
  top_g <- head(indices_df[order(-indices_df$g_index), ], top_n)
  top_m <- head(indices_df[order(-indices_df$m_quotient), ], top_n)
  
  list(
    indices = indices_df,
    top_h_index = top_h,
    top_g_index = top_g,
    top_m_quotient = top_m,
    summary = summary_stats,
    status = "success"
  )
}

#' Extract author papers with citations
#' @keywords internal
extract_author_papers <- function(input, au_col, tc_col, py_col) {
  authors_list <- strsplit(as.character(input[[au_col]]), ";")
  authors_list <- lapply(authors_list, trimws)
  authors_list <- lapply(authors_list, function(x) x[x != ""])
  
  citations <- as.numeric(input[[tc_col]])
  citations[is.na(citations)] <- 0
  
  years <- if (py_col %in% names(input)) {
    as.numeric(input[[py_col]])
  } else {
    NA
  }
  years[is.na(years)] <- NA
  
  result_list <- lapply(seq_along(authors_list), function(i) {
    authors <- authors_list[[i]]
    if (length(authors) == 0) return(NULL)
    
    data.frame(
      author = authors,
      citations = citations[i],
      year = years[i],
      stringsAsFactors = FALSE
    )
  })
  
  result <- do.call(rbind, result_list[!sapply(result_list, is.null)])
  
  if (is.null(result) || nrow(result) == 0) {
    return(NULL)
  }
  
  result$citations[is.na(result$citations)] <- 0
  
  result
}

#' Compute all author indices
#' @keywords internal
compute_all_author_indices <- function(author_papers) {
  # Compute author summary statistics using dplyr-like approach
  author_stats <- lapply(unique(author_papers$author), function(au) {
    papers <- author_papers[author_papers$author == au, ]
    year_min <- if ("year" %in% names(papers) && any(is.finite(papers$year))) {
      min(papers$year, na.rm = TRUE)
    } else {
      NA_real_
    }
    data.frame(
      author = au,
      citations_sum = sum(papers$citations, na.rm = TRUE),
      citations_count = length(papers$citations),
      year_min = year_min,
      stringsAsFactors = FALSE
    )
  })
  author_summary <- do.call(rbind, author_stats)
  
  authors <- unique(author_papers$author)
  
  indices_list <- lapply(authors, function(au) {
    papers <- author_papers[author_papers$author == au, ]
    c_vec <- sort(papers$citations, decreasing = TRUE)
    
    h_idx <- compute_h_index(c_vec)
    g_idx <- compute_g_index(c_vec)
    
    n_papers <- length(c_vec)
    total_citations <- sum(c_vec, na.rm = TRUE)
    
    first_year <- if ("year" %in% names(papers) && any(!is.na(papers$year))) {
      min(papers$year, na.rm = TRUE)
    } else {
      NA
    }
    
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    m_q <- if (!is.na(first_year) && current_year > first_year) {
      h_idx / (current_year - first_year)
    } else {
      NA
    }
    
    i10 <- sum(c_vec >= 10)
    
    hI_norm <- if (n_papers > 0) {
      h_idx / sqrt(n_papers)
    } else {
      NA
    }
    
    data.frame(
      author = au,
      n_papers = n_papers,
      total_citations = total_citations,
      citations_per_paper = if (n_papers > 0) total_citations / n_papers else 0,
      h_index = h_idx,
      g_index = g_idx,
      m_quotient = m_q,
      i10_index = i10,
      hI_index = hI_norm,
      first_year = first_year,
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, indices_list)
}

#' Compute h-index
#' @keywords internal
compute_h_index <- function(citations_sorted_desc) {
  citations_sorted_desc <- citations_sorted_desc[!is.na(citations_sorted_desc)]
  if (length(citations_sorted_desc) == 0) return(0)
  
  citations_sorted_desc <- sort(citations_sorted_desc, decreasing = TRUE)
  
  h <- 0
  for (i in seq_along(citations_sorted_desc)) {
    if (citations_sorted_desc[i] >= i) {
      h <- i
    } else {
      break
    }
  }
  
  h
}

#' Compute g-index
#' @keywords internal
compute_g_index <- function(citations_sorted_desc) {
  citations_sorted_desc <- citations_sorted_desc[!is.na(citations_sorted_desc)]
  if (length(citations_sorted_desc) == 0) return(0)
  
  citations_sorted_desc <- sort(citations_sorted_desc, decreasing = TRUE)
  
  g <- 0
  cumsum_citations <- 0
  
  for (i in seq_along(citations_sorted_desc)) {
    cumsum_citations <- cumsum_citations + citations_sorted_desc[i]
    if (cumsum_citations >= i^2) {
      g <- i
    } else {
      break
    }
  }
  
  g
}
