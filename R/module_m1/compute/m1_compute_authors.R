# ============================================================================
# m1_compute_authors.R - Authors metric for M1
# ============================================================================

#' @export
compute_m1_authors <- function(input, config = biblio_config()) {
  if (!is.data.frame(input) || nrow(input) == 0) {
    return(list(top_authors = m1_empty_rank_table(), author_productivity = list(), author_gini = NA_real_, status = "error"))
  }

  top_n <- config$top_n_authors

  # Try bibliometrix first, fallback to manual
  author_data <- tryCatch({
    res <- bibliometrix::biblioAnalysis(input, sep = ";")
    s <- summary(res, pause = FALSE, verbose = FALSE)
    mp <- s$MostProdAuthors
    colnames(mp) <- make.unique(colnames(mp))
    art_col <- if ("Articles" %in% colnames(mp)) "Articles" else colnames(mp)[2]
    mp[[art_col]] <- suppressWarnings(as.numeric(mp[[art_col]]))
    mp <- mp[!is.na(mp[[art_col]]), ]
    mp <- mp[order(-mp[[art_col]]), ]
    mp
  }, error = function(e) {
    # Manual: count authors
    authors <- m1_extract_authors(input)
    au_list <- lapply(input$AU, function(x) trimws(unlist(strsplit(x, ";"))))
    au_counts <- table(unlist(au_list))
    data.frame(Authors = names(au_counts), Articles = as.integer(au_counts), stringsAsFactors = FALSE)
  })

  top_authors <- tibble::tibble(
    rank  = seq_len(min(top_n, nrow(author_data))),
    label = author_data$Authors[1:min(top_n, nrow(author_data))],
    value = author_data$Articles[1:min(top_n, nrow(author_data))]
  )

  # Gini
  lorenz <- m1_compute_lorenz(top_authors$value)
  author_gini <- m1_compute_gini(lorenz$cumulative_x, lorenz$cumulative_y)

  list(top_authors = top_authors, author_productivity = author_data, author_gini = author_gini, status = "success")
}
