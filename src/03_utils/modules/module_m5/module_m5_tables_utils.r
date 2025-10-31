# ================================================================
# m5_tables_utils.R — small tables for top lists & zeros
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
})

m5_top_cited <- function(df, top_n = 20L) {
  if (!nrow(df)) return(df[0, c("title","Year","source","doi","Times_Cited")])
  ord <- order(-df$Times_Cited, df$Year, na.last = TRUE)
  out <- utils::head(df[ord, c("title","Year","source","doi","Times_Cited")], top_n)
  rownames(out) <- NULL
  out
}

m5_top_recent <- function(df, recent_years = 5L, top_n = 20L) {
  if (!nrow(df)) return(df[0, c("title","Year","source","doi","Times_Cited")])
  yr_max <- suppressWarnings(max(df$Year, na.rm = TRUE))
  if (!is.finite(yr_max)) return(df[0, c("title","Year","source","doi","Times_Cited")])
  recent_cut <- yr_max - as.integer(recent_years) + 1L
  rec <- df[df$Year >= recent_cut, , drop = FALSE]
  if (!nrow(rec)) return(rec)
  ord <- order(-rec$Times_Cited, rec$Year, na.last = TRUE)
  out <- utils::head(rec[ord, c("title","Year","source","doi","Times_Cited")], top_n)
  rownames(out) <- NULL
  out
}

# Simple "risers": sort by TC within the recent window (stub — extend if you track per-paper deltas)
m5_risers_recent <- function(df, recent_years = 5L, top_n = 20L) {
  if (!nrow(df)) return(data.frame(title=character(0),Year=integer(0),source=character(0),doi=character(0),delta=numeric(0)))
  yr_max <- suppressWarnings(max(df$Year, na.rm = TRUE))
  if (!is.finite(yr_max)) return(data.frame(title=character(0),Year=integer(0),source=character(0),doi=character(0),delta=numeric(0)))
  recent_cut <- yr_max - as.integer(recent_years) + 1L
  rec <- df[df$Year >= recent_cut, , drop = FALSE]
  if (!nrow(rec)) return(data.frame(title=character(0),Year=integer(0),source=character(0),doi=character(0),delta=numeric(0)))
  rec <- dplyr::mutate(rec, delta = Times_Cited) %>%
    dplyr::arrange(dplyr::desc(delta), dplyr::desc(Year))
  out <- utils::head(rec[, c("title","Year","source","doi","delta")], top_n)
  rownames(out) <- NULL
  out
}

m5_zero_cited_by_year <- function(df) {
  z <- df[df$Times_Cited == 0, c("Year","Times_Cited"), drop = FALSE]
  if (!nrow(z)) return(data.frame(Year = integer(0), Zero_Cited = integer(0)))
  as.data.frame(table(z$Year), stringsAsFactors = FALSE) |>
    stats::setNames(c("Year","Zero_Cited")) |>
    transform(Year = suppressWarnings(as.integer(Year)))
}
