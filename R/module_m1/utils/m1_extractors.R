# ============================================================================
# m1_extractors.R - Extraction helpers for M1
# ============================================================================

#' Extract main information from bibliometrix summary
#'
#' @param summary_df A data frame from bibliometrix summary$MainInformationDF.
#' @return A list of main indicators.
#' @export
m1_extract_main_information <- function(summary_df) {
  get_val <- function(desc) {
    idx <- which(summary_df$Description == desc)
    if (length(idx) == 0) return(NA_character_)
    as.character(summary_df$Results[idx[1]])
  }

  list(
    timespan                      = get_val("Timespan"),
    sources                       = get_val("Sources (Journals, Books, etc)"),
    documents                     = get_val("Documents"),
    annual_growth_rate             = get_val("Annual Growth Rate %"),
    document_average_age           = get_val("Document Average Age"),
    avg_citations_per_doc          = get_val("Average citations per doc"),
    avg_citations_per_year_per_doc = get_val("Average citations per year per doc"),
    references                    = get_val("References"),
    authors                       = get_val("Authors"),
    authors_per_doc               = get_val("Authors per Document"),
    co_authors_per_doc            = get_val("Co-Authors per Documents"),
    international_collaborations  = get_val("International co-authorships %"),
    single_author_documents       = get_val("Single-authored documents"),
    multi_author_documents        = get_val("Multi-authored documents")
  )
}

#' Extract document types from bibliometrix summary
#'
#' @param summary_df A data frame from bibliometrix summary$MainInformationDF.
#' @return A named list of document type counts.
#' @export
m1_extract_document_types <- function(summary_df) {
  get_val <- function(desc) {
    idx <- which(summary_df$Description == desc)
    if (length(idx) == 0) return(0L)
    as.integer(summary_df$Results[idx[1]])
  }

  list(
    article                  = get_val("article"),
    article_article          = get_val("article article"),
    article_conference_paper = get_val("article conference paper"),
    article_review           = get_val("article review"),
    conference_paper         = get_val("conference paper"),
    review                   = get_val("review")
  )
}

#' Extract unique author names
#'
#' @param input A data frame with an AU column.
#' @param delim Delimiter between authors.
#' @return Character vector of unique authors.
#' @export
m1_extract_authors <- function(input, delim = ";") {
  au <- m1_get_authors_col(input)
  if (length(au) == 0) return(character())
  unique(trimws(unlist(strsplit(au, delim, fixed = TRUE))))
}

#' Extract unique sources
#'
#' @param input A data frame with an SO column.
#' @return Character vector of unique sources.
#' @export
m1_extract_sources <- function(input) {
  so <- m1_get_source_col(input)
  if (length(so) == 0) return(character())
  unique(trimws(so[!is.na(so)]))
}

#' Extract keywords
#'
#' @param input A data frame with a DE or ID column.
#' @param column Column name to use.
#' @param delim Delimiter between keywords.
#' @return Character vector of keywords.
#' @export
m1_extract_keywords <- function(input, column = "DE", delim = ";") {
  if (!column %in% names(input)) return(character())
  kw <- input[[column]]
  kw <- kw[!is.na(kw) & kw != ""]
  unique(trimws(unlist(strsplit(kw, delim, fixed = TRUE))))
}
