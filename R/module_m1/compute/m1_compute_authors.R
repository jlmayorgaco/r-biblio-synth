# ============================================================================
# m1_compute_authors.R - Authors metric for M1
# ============================================================================

#' @export
compute_m1_authors <- function(input, config = biblio_config()) {
  if (!is.data.frame(input) || nrow(input) == 0) {
    return(list(top_authors = m1_empty_rank_table(), author_productivity = list(), author_gini = NA_real_, status = "error"))
  }

  top_n <- config$top_n_authors

  author_data <- if (identical(config$counting_mode, "fractional")) {
    m1_compute_authors_manual(input, counting_mode = "fractional")
  } else {
    tryCatch({
      cached <- get_cached_biblio_analysis(input); res <- cached$res
      s <- summary(res, pause = FALSE, verbose = FALSE)
      mp <- s$MostProdAuthors
      colnames(mp) <- make.unique(colnames(mp))
      art_col <- if ("Articles" %in% colnames(mp)) "Articles" else
        if ("Freq" %in% colnames(mp)) "Freq" else colnames(mp)[2]
      mp[[art_col]] <- suppressWarnings(as.numeric(mp[[art_col]]))
      mp <- mp[!is.na(mp[[art_col]]), ]
      mp <- mp[order(-mp[[art_col]]), ]
      au_col <- if ("Authors" %in% colnames(mp)) "Authors" else
        if ("AU" %in% colnames(mp)) "AU" else colnames(mp)[1]
      data.frame(
        Authors = mp[[au_col]],
        Articles = mp[[art_col]],
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      m1_compute_authors_manual(input, counting_mode = "full")
    })
  }

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

#' Manual author aggregation with counting-mode support
#' @keywords internal
m1_compute_authors_manual <- function(input, counting_mode = "full") {
  if (!"AU" %in% names(input)) {
    return(data.frame(Authors = character(0), Articles = numeric(0), stringsAsFactors = FALSE))
  }

  rows <- vector("list", nrow(input))
  for (i in seq_len(nrow(input))) {
    authors <- trimws(unlist(strsplit(as.character(input$AU[i]), ";", fixed = TRUE)))
    authors <- authors[nzchar(authors)]
    if (length(authors) == 0) next
    weight <- if (identical(counting_mode, "fractional")) 1 / length(authors) else 1
    rows[[i]] <- data.frame(
      Authors = authors,
      Articles = rep(weight, length(authors)),
      stringsAsFactors = FALSE
    )
  }

  author_df <- dplyr::bind_rows(rows)
  if (nrow(author_df) == 0) {
    return(data.frame(Authors = character(0), Articles = numeric(0), stringsAsFactors = FALSE))
  }

  author_df %>%
    dplyr::group_by(Authors) %>%
    dplyr::summarise(Articles = sum(Articles, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(desc(Articles)) %>%
    as.data.frame(stringsAsFactors = FALSE)
}
