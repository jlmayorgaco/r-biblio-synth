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
  # Returns a data frame with columns: Authors (char) and Articles (numeric)
  author_data <- tryCatch({
    res <- bibliometrix::biblioAnalysis(input, sep = ";")
    s <- summary(res, pause = FALSE, verbose = FALSE)
    mp <- s$MostProdAuthors
    colnames(mp) <- make.unique(colnames(mp))
    # Detect articles column (2nd column or "Articles" / "Freq")
    art_col <- if ("Articles" %in% colnames(mp)) "Articles" else
               if ("Freq"     %in% colnames(mp)) "Freq"     else colnames(mp)[2]
    mp[[art_col]] <- suppressWarnings(as.numeric(mp[[art_col]]))
    mp <- mp[!is.na(mp[[art_col]]), ]
    mp <- mp[order(-mp[[art_col]]), ]
    # Detect author column (1st column or "Authors" / "AU")
    au_col <- if ("Authors" %in% colnames(mp)) "Authors" else
              if ("AU"      %in% colnames(mp)) "AU"      else colnames(mp)[1]
    data.frame(Authors = mp[[au_col]], Articles = mp[[art_col]],
               stringsAsFactors = FALSE)
  }, error = function(e) {
    # Manual: count authors from AU column
    if (!"AU" %in% names(input)) {
      return(data.frame(Authors = character(0), Articles = integer(0),
                        stringsAsFactors = FALSE))
    }
    au_list <- lapply(input$AU, function(x) trimws(unlist(strsplit(x, ";"))))
    au_counts <- sort(table(unlist(au_list)), decreasing = TRUE)
    data.frame(Authors = names(au_counts), Articles = as.integer(au_counts),
               stringsAsFactors = FALSE)
  })

  top_n_actual <- min(top_n, nrow(author_data))
  top_authors <- tibble::tibble(
    rank  = seq_len(top_n_actual),
    label = author_data$Authors[seq_len(top_n_actual)],
    value = author_data$Articles[seq_len(top_n_actual)]
  )

  # Gini (FIXED: correct column names)
  lorenz <- m1_compute_lorenz(top_authors$value)
  author_gini <- m1_compute_gini(lorenz$cumulative_entities, lorenz$cumulative_values)

  list(top_authors = top_authors, author_productivity = author_data, author_gini = author_gini, status = "success")
}
