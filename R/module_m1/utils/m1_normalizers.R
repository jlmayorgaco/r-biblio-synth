# ============================================================================
# m1_normalizers.R - Normalization helpers for M1
# ============================================================================

#' Normalize text (trim, collapse whitespace, lowercase)
#'
#' @param x Character vector.
#' @return Normalized character vector.
#' @export
m1_normalize_text <- function(x) {
  x <- trimws(x)
  x <- gsub("\\s+", " ", x)
  tolower(x)
}

#' Normalize a delimited field
#'
#' @param x Character vector.
#' @param delim Delimiter to split on.
#' @return Character vector with normalized values.
#' @export
m1_normalize_delimited_field <- function(x, delim = ";") {
  parts <- unlist(strsplit(x, delim, fixed = TRUE))
  unique(trimws(parts[!is.na(parts) & parts != ""]))
}

#' Normalize country names to match map dataset
#'
#' @param x Character vector of country names.
#' @return Character vector with normalized country names.
#' @export
m1_normalize_country_names <- function(x) {
  x <- as.character(x)
  x <- trimws(gsub("\\s+", " ", x))

  normalize_one <- function(value) {
    if (is.na(value) || !nzchar(value)) {
      return(NA_character_)
    }

    key <- toupper(trimws(value))
    aliases <- c(
      "USA" = "UNITED STATES",
      "US" = "UNITED STATES",
      "U.S.A." = "UNITED STATES",
      "U.S." = "UNITED STATES",
      "UK" = "UNITED KINGDOM",
      "U.K." = "UNITED KINGDOM",
      "KOREA" = "SOUTH KOREA",
      "REPUBLIC OF KOREA" = "SOUTH KOREA",
      "PEOPLES R CHINA" = "CHINA",
      "PR CHINA" = "CHINA",
      "P R CHINA" = "CHINA",
      "UAE" = "UNITED ARAB EMIRATES",
      "RUSSIAN FEDERATION" = "RUSSIA",
      "VIET NAM" = "VIETNAM",
      "IRAN, ISLAMIC REPUBLIC OF" = "IRAN",
      "TURKIYE" = "TURKEY"
    )

    if (key %in% names(aliases)) {
      key <- aliases[[key]]
    }

    title_case <- function(text) {
      parts <- unlist(strsplit(tolower(text), "\\s+"))
      parts <- ifelse(
        parts %in% c("and", "of", "the", "for"),
        parts,
        paste0(toupper(substr(parts, 1, 1)), substring(parts, 2))
      )
      paste(parts, collapse = " ")
    }

    title_case(key)
  }

  vapply(x, normalize_one, character(1))
}

#' Convert a phrase to readable title case
#'
#' @param x Character vector.
#' @return Title-cased character vector.
#' @export
m1_title_case_phrase <- function(x) {
  x <- as.character(x)
  vapply(x, function(value) {
    if (is.na(value) || !nzchar(trimws(value))) {
      return(NA_character_)
    }
    words <- unlist(strsplit(tolower(trimws(value)), "\\s+"))
    stop_words <- c("and", "or", "of", "the", "for", "in", "on", "to", "with", "via")
    words <- ifelse(
      words %in% stop_words,
      words,
      paste0(toupper(substr(words, 1, 1)), substring(words, 2))
    )
    paste(words, collapse = " ")
  }, character(1))
}

#' Normalize keyword or topic phrases
#'
#' @param x Character vector.
#' @return Normalized phrase labels in readable title case.
#' @export
m1_normalize_keyword_phrase <- function(x) {
  x <- as.character(x)
  normalized <- vapply(x, function(value) {
    if (is.na(value) || !nzchar(trimws(value))) {
      return(NA_character_)
    }
    value <- gsub("[_/]+", " ", value)
    value <- gsub("-", " ", value)
    value <- gsub("[^[:alnum:]\\s]", " ", value)
    value <- gsub("\\s+", " ", trimws(value))
    if (!nzchar(value)) {
      return(NA_character_)
    }
    tolower(value)
  }, character(1))

  m1_title_case_phrase(normalized)
}

#' Build a readable citation label for reports and tables
#'
#' @param title Article title.
#' @param source Source or journal name.
#' @param year Publication year.
#' @param doi DOI.
#' @param max_chars Maximum length for the title fragment.
#' @return Character vector of display labels.
#' @export
m1_build_citation_display_label <- function(title, source = NA_character_, year = NA, doi = NA_character_, max_chars = 80L) {
  title <- as.character(title)
  source <- as.character(source)
  doi <- as.character(doi)
  year <- as.character(year)

  vapply(seq_along(title), function(i) {
    ti <- if (!is.na(title[i])) trimws(title[i]) else ""
    so <- if (!is.na(source[i])) trimws(source[i]) else ""
    yr <- if (!is.na(year[i])) trimws(year[i]) else ""
    di <- if (!is.na(doi[i])) trimws(doi[i]) else ""

    if (!nzchar(ti)) {
      ti <- if (nzchar(di)) di else paste("Document", i)
    }
    if (nchar(ti) > max_chars) {
      ti <- paste0(substr(ti, 1, max_chars - 3L), "...")
    }

    meta <- c(
      if (nzchar(yr)) yr else NULL,
      if (nzchar(so)) so else NULL
    )
    meta_text <- if (length(meta) > 0) paste(meta, collapse = ", ") else NULL
    paste(c(ti, meta_text), collapse = " | ")
  }, character(1))
}

#' Build readable topic labels from top words
#'
#' @param top_words Character vector of top topic words or phrases.
#' @return Single readable topic label.
#' @export
m1_make_topic_label <- function(top_words) {
  top_words <- as.character(top_words %||% character())
  top_words <- top_words[!is.na(top_words) & nzchar(trimws(top_words))]
  if (length(top_words) == 0) {
    return("Topic")
  }

  generic_terms <- c(
    "study", "studies", "analysis", "review", "reviews", "method", "methods",
    "approach", "approaches", "paper", "papers", "article", "articles",
    "model", "models", "based", "using", "application", "applications"
  )

  clean_terms <- vapply(top_words, function(term) {
    term <- m1_normalize_keyword_phrase(term)
    words <- unlist(strsplit(tolower(term), "\\s+"))
    words <- words[!words %in% generic_terms]
    if (length(words) == 0) {
      return(NA_character_)
    }
    m1_title_case_phrase(paste(words, collapse = " "))
  }, character(1))

  clean_terms <- unique(clean_terms[!is.na(clean_terms) & nzchar(clean_terms)])
  if (length(clean_terms) == 0) {
    clean_terms <- unique(m1_normalize_keyword_phrase(top_words))
    clean_terms <- clean_terms[!is.na(clean_terms) & nzchar(clean_terms)]
  }
  if (length(clean_terms) == 0) {
    return("Topic")
  }

  paste(utils::head(clean_terms, 3), collapse = " / ")
}
