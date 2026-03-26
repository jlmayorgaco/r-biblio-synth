# ============================================================================
# module_m1/compute/m1_compute_country_trends.R - Country trend analysis
# ============================================================================

#' @export
compute_m1_country_trends <- function(bib_data, config = biblio_config()) {
  if (!is.data.frame(bib_data)) {
    return(list(status = "error", trends = data.frame()))
  }

  # Get country data from bibliometrix
  res <- tryCatch(bibliometrix::biblioAnalysis(bib_data, sep = ";"), error = function(e) NULL)
  if (is.null(res)) return(list(status = "error", trends = data.frame()))

  s <- tryCatch(summary(res, pause = FALSE, verbose = FALSE), error = function(e) NULL)
  if (is.null(s)) return(list(status = "error", trends = data.frame()))

  mp <- s$MostProdCountries
  tc <- s$TCperCountries

  if (is.null(mp) || nrow(mp) == 0) {
    return(list(status = "error", trends = data.frame()))
  }

  colnames(mp) <- make.unique(colnames(mp))
  art_col <- if ("Articles" %in% colnames(mp)) "Articles" else colnames(mp)[2]
  mp[[art_col]] <- suppressWarnings(as.numeric(mp[[art_col]]))
  mp <- mp[!is.na(mp[[art_col]]), ]

  # Build country data
  country_data <- data.frame(
    country = mp$Country,
    articles = mp[[art_col]],
    stringsAsFactors = FALSE
  )

  # Add citations if available
  if (!is.null(tc) && nrow(tc) > 0) {
    colnames(tc) <- make.unique(colnames(tc))
    cit_col <- if ("Total Citations" %in% colnames(tc)) "Total Citations" else colnames(tc)[2]
    tc[[cit_col]] <- suppressWarnings(as.numeric(tc[[cit_col]]))
    tc <- tc[!is.na(tc[[cit_col]]), ]

    merged <- merge(country_data, tc[, c("Country", cit_col)], by.x = "country", by.y = "Country", all.x = TRUE)
    merged$citations <- merged[[cit_col]]
    merged[[cit_col]] <- NULL
    country_data <- merged
  } else {
    country_data$citations <- NA_real_
  }

  # Sort by articles
  country_data <- country_data[order(-country_data$articles), ]

  # Assign quadrants based on medians
  med_art <- median(country_data$articles, na.rm = TRUE)
  med_cit <- median(country_data$citations, na.rm = TRUE)

  country_data$quadrant <- ifelse(
    country_data$articles >= med_art & country_data$citations >= med_cit, "High Productivity, High Impact",
    ifelse(country_data$articles >= med_art & (is.na(country_data$citations) | country_data$citations < med_cit), "High Productivity, Low Impact",
    ifelse((is.na(country_data$articles) | country_data$articles < med_art) & country_data$citations >= med_cit, "Low Productivity, High Impact",
           "Low Productivity, Low Impact")))

  list(status = "success", trends = head(country_data, config$top_n_countries))
}
