# ============================================================================
# zz_m0_reliability_overrides.R - Reliability extensions for screening
# ============================================================================

m0_inter_rater_reliability_legacy <- m0_inter_rater_reliability

#' Calculate inter-rater reliability for screening
#'
#' Extends the legacy summary with Krippendorff's alpha for nominal decisions.
#'
#' @param ratings Matrix where rows are papers and columns are reviewers.
#' @return Reliability summary list.
#' @export
m0_inter_rater_reliability <- function(ratings) {
  result <- m0_inter_rater_reliability_legacy(ratings)
  if (!is.list(result) || !identical(result$status, "success")) {
    return(result)
  }

  alpha <- tryCatch(
    calculate_krippendorffs_alpha_nominal(ratings),
    error = function(e) NA_real_
  )
  result$krippendorffs_alpha <- alpha
  result$interpretation <- interpret_kappa(if (is.finite(alpha)) alpha else result$cohens_kappa_mean %||% NA_real_)
  result
}

#' Calculate Krippendorff's alpha for nominal labels
#' @keywords internal
calculate_krippendorffs_alpha_nominal <- function(ratings) {
  if (!is.matrix(ratings) && !is.data.frame(ratings)) {
    return(NA_real_)
  }

  ratings <- as.matrix(ratings)
  values <- as.vector(ratings)
  values <- values[!is.na(values)]
  categories <- unique(values)
  if (length(categories) < 2) {
    return(1)
  }

  disagreements <- c()
  for (i in seq_len(nrow(ratings))) {
    row <- ratings[i, ]
    row <- row[!is.na(row)]
    if (length(row) < 2) {
      next
    }
    pairs <- utils::combn(row, 2, simplify = FALSE)
    disagreements <- c(
      disagreements,
      vapply(pairs, function(pair) as.numeric(pair[1] != pair[2]), numeric(1))
    )
  }

  if (length(disagreements) == 0) {
    return(NA_real_)
  }

  do <- mean(disagreements, na.rm = TRUE)
  probs <- table(values) / length(values)
  de <- 1 - sum(probs^2)
  if (!is.finite(de) || de <= .Machine$double.eps) {
    return(1)
  }

  1 - (do / de)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
