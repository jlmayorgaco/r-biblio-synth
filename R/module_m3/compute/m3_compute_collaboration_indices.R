# ============================================================================
# m3_compute_collaboration_indices.R - Country Collaboration Indices
# ============================================================================
# Computes collaboration indices for countries:
# - Salton's Index (cosine similarity)
# - Jaccard Index (intersection over union)
# - Affinity Index (publications ratio)
# - Collaboration Intensity Index
#
# IEEE Q1 Enhancement: Comprehensive bilateral collaboration metrics
# following advanced bibliometric methodologies.

#' Compute collaboration indices between countries
#'
#' @param prepared_data Output from prepare_m3_country_data
#' @param config Configuration list
#' @return List with collaboration indices
#' @export
compute_m3_collaboration_indices <- function(prepared_data, config = biblio_config()) {
  if (is.null(prepared_data) || prepared_data$status != "success") {
    return(list(
      indices = data.frame(),
      bilateral = data.frame(),
      summary = list(),
      status = "error: invalid prepared_data"
    ))
  }
  
  doc_country <- prepared_data$country_doc_level
  if (is.null(doc_country) || nrow(doc_country) == 0) {
    return(list(
      indices = data.frame(),
      bilateral = data.frame(),
      summary = list(),
      status = "error: no country data available"
    ))
  }
  
  collab_matrix <- build_collaboration_matrix(doc_country)
  
  if (is.null(collab_matrix) || nrow(collab_matrix) == 0) {
    return(list(
      indices = data.frame(),
      bilateral = data.frame(),
      summary = list(),
      status = "error: could not build collaboration matrix"
    ))
  }
  
  country_totals <- get_country_totals(doc_country)
  
  salton_matrix <- compute_salton_index(collab_matrix, country_totals)
  
  jaccard_matrix <- compute_jaccard_index(collab_matrix, country_totals)
  
  affinity_matrix <- compute_affinity_index(collab_matrix, country_totals)
  
  collaboration_intensity <- compute_collaboration_intensity(collab_matrix, country_totals)
  
  bilateral_collabs <- extract_bilateral_collaborations(
    salton_matrix, jaccard_matrix, affinity_matrix, country_totals
  )
  
  country_indices <- compile_country_indices(
    salton_matrix, jaccard_matrix, affinity_matrix, country_totals
  )
  
  summary_stats <- list(
    total_countries = nrow(country_totals),
    total_collaborations = sum(collab_matrix[upper.tri(collab_matrix)]),
    mean_salton = mean(salton_matrix[upper.tri(salton_matrix)], na.rm = TRUE),
    mean_jaccard = mean(jaccard_matrix[upper.tri(jaccard_matrix)], na.rm = TRUE),
    mean_affinity = mean(affinity_matrix[upper.tri(affinity_matrix)], na.rm = TRUE),
    top_collaborators = head(bilateral_collabs[order(-bilateral_collabs$salton), ], 10),
    collaboration_density = sum(collab_matrix[upper.tri(collab_matrix)] > 0) / 
                             (nrow(collab_matrix) * (nrow(collab_matrix) - 1) / 2)
  )
  
  list(
    indices = country_indices,
    bilateral = bilateral_collabs,
    summary = summary_stats,
    salton_matrix = salton_matrix,
    jaccard_matrix = jaccard_matrix,
    affinity_matrix = affinity_matrix,
    status = "success"
  )
}

#' Build collaboration matrix
#' @keywords internal
build_collaboration_matrix <- function(doc_country) {
  doc_id_valid <- !is.na(doc_country$doc_id) & !is.na(doc_country$country)
  
  if (sum(doc_id_valid) == 0) return(NULL)
  
  valid_data <- doc_country[doc_id_valid, ]
  
  docs_per_country <- split(valid_data$country, valid_data$doc_id)
  
  countries <- sort(unique(valid_data$country))
  n_countries <- length(countries)
  
  collab_matrix <- matrix(0, nrow = n_countries, ncol = n_countries,
                          dimnames = list(countries, countries))
  
  for (doc_id in names(docs_per_country)) {
    doc_countries <- unique(docs_per_country[[doc_id]])
    if (length(doc_countries) > 1) {
      for (i in 1:length(doc_countries)) {
        for (j in 1:length(doc_countries)) {
          c_i <- which(rownames(collab_matrix) == doc_countries[i])
          c_j <- which(colnames(collab_matrix) == doc_countries[j])
          if (length(c_i) > 0 && length(c_j) > 0) {
            collab_matrix[c_i, c_j] <- collab_matrix[c_i, c_j] + 1
          }
        }
      }
    }
  }
  
  collab_matrix
}

#' Get country totals
#' @keywords internal
get_country_totals <- function(doc_country) {
  doc_id_valid <- !is.na(doc_country$doc_id) & !is.na(doc_country$country)
  valid_data <- doc_country[doc_id_valid, ]
  
  totals <- aggregate(doc_id ~ country, data = valid_data, FUN = function(x) length(unique(x)))
  names(totals) <- c("country", "n_documents")
  
  totals[order(-totals$n_documents), ]
}

#' Compute Salton's Index (cosine similarity)
#' @keywords internal
compute_salton_index <- function(collab_matrix, country_totals) {
  n <- nrow(collab_matrix)
  salton <- matrix(0, nrow = n, ncol = n,
                   dimnames = dimnames(collab_matrix))
  
  doc_counts <- setNames(country_totals$n_documents, country_totals$country)
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        country_i <- rownames(collab_matrix)[i]
        country_j <- colnames(collab_matrix)[j]
        
        n_ij <- collab_matrix[i, j]
        
        n_i <- if (country_i %in% names(doc_counts)) doc_counts[country_i] else 1
        n_j <- if (country_j %in% names(doc_counts)) doc_counts[country_j] else 1
        
        denominator <- sqrt(n_i * n_j)
        
        salton[i, j] <- if (denominator > 0) {
          n_ij / denominator
        } else {
          0
        }
      }
    }
  }
  
  salton
}

#' Compute Jaccard Index
#' @keywords internal
compute_jaccard_index <- function(collab_matrix, country_totals) {
  n <- nrow(collab_matrix)
  jaccard <- matrix(0, nrow = n, ncol = n,
                    dimnames = dimnames(collab_matrix))
  
  doc_counts <- setNames(country_totals$n_documents, country_totals$country)
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        country_i <- rownames(collab_matrix)[i]
        country_j <- colnames(collab_matrix)[j]
        
        n_ij <- collab_matrix[i, j]
        
        n_i <- if (country_i %in% names(doc_counts)) doc_counts[country_i] else 1
        n_j <- if (country_j %in% names(doc_counts)) doc_counts[country_j] else 1
        
        union_count <- n_i + n_j - n_ij
        
        jaccard[i, j] <- if (union_count > 0) {
          n_ij / union_count
        } else {
          0
        }
      }
    }
  }
  
  jaccard
}

#' Compute Affinity Index
#' @keywords internal
compute_affinity_index <- function(collab_matrix, country_totals) {
  n <- nrow(collab_matrix)
  affinity <- matrix(0, nrow = n, ncol = n,
                     dimnames = dimnames(collab_matrix))
  
  doc_counts <- setNames(country_totals$n_documents, country_totals$country)
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        country_i <- rownames(collab_matrix)[i]
        country_j <- colnames(collab_matrix)[j]
        
        n_ij <- collab_matrix[i, j]
        
        n_i <- if (country_i %in% names(doc_counts)) doc_counts[country_i] else 1
        
        affinity[i, j] <- if (n_i > 0) {
          n_ij / n_i
        } else {
          0
        }
      }
    }
  }
  
  affinity
}

#' Compute Collaboration Intensity
#' @keywords internal
compute_collaboration_intensity <- function(collab_matrix, country_totals) {
  n <- nrow(collab_matrix)
  intensity <- matrix(0, nrow = n, ncol = n,
                      dimnames = dimnames(collab_matrix))
  
  total_collabs <- sum(collab_matrix) - sum(diag(collab_matrix))
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        n_ij <- collab_matrix[i, j]
        intensity[i, j] <- if (total_collabs > 0) {
          n_ij / total_collabs
        } else {
          0
        }
      }
    }
  }
  
  intensity
}

#' Extract bilateral collaborations
#' @keywords internal
extract_bilateral_collaborations <- function(salton, jaccard, affinity, country_totals) {
  n <- nrow(salton)
  countries <- rownames(salton)
  
  pairs <- expand.grid(i = 1:n, j = 1:n)
  pairs <- pairs[pairs$i < pairs$j, ]
  
  if (nrow(pairs) == 0) {
    return(data.frame())
  }
  
  bilateral_list <- lapply(1:nrow(pairs), function(k) {
    i <- pairs$i[k]
    j <- pairs$j[k]
    
    data.frame(
      country_1 = countries[i],
      country_2 = countries[j],
      salton = salton[i, j],
      jaccard = jaccard[i, j],
      affinity_1to2 = affinity[i, j],
      affinity_2to1 = affinity[j, i]
    )
  })
  
  result <- do.call(rbind, bilateral_list)
  
  result$avg_affinity <- (result$affinity_1to2 + result$affinity_2to1) / 2
  
  result[order(-result$salton), ]
}

#' Compile country-level indices
#' @keywords internal
compile_country_indices <- function(salton, jaccard, affinity, country_totals) {
  countries <- rownames(salton)
  
  n_countries <- length(countries)
  
  indices_list <- lapply(1:n_countries, function(i) {
    country <- countries[i]
    
    n_docs <- if (country %in% country_totals$country) {
      country_totals$n_documents[country_totals$country == country]
    } else {
      0
    }
    
    top_salton_partner_idx <- which.max(salton[i, -i])
    top_salton_partner <- if (length(top_salton_partner_idx) > 0) {
      countries[-i][top_salton_partner_idx]
    } else {
      NA_character_
    }
    
    top_jaccard_partner_idx <- which.max(jaccard[i, -i])
    top_jaccard_partner <- if (length(top_jaccard_partner_idx) > 0) {
      countries[-i][top_jaccard_partner_idx]
    } else {
      NA_character_
    }
    
    avg_salton <- mean(salton[i, -i], na.rm = TRUE)
    avg_jaccard <- mean(jaccard[i, -i], na.rm = TRUE)
    avg_affinity <- mean(affinity[i, -i], na.rm = TRUE)
    
    max_salton <- max(salton[i, -i], na.rm = TRUE)
    max_jaccard <- max(jaccard[i, -i], na.rm = TRUE)
    max_affinity <- max(affinity[i, -i], na.rm = TRUE)
    
    data.frame(
      country = country,
      n_documents = n_docs,
      avg_salton_index = avg_salton,
      max_salton_index = max_salton,
      top_salton_partner = top_salton_partner,
      avg_jaccard_index = avg_jaccard,
      max_jaccard_index = max_jaccard,
      top_jaccard_partner = top_jaccard_partner,
      avg_affinity_index = avg_affinity,
      max_affinity_index = max_affinity,
      collaboration_centrality = sum(salton[i, ], na.rm = TRUE) / (n_countries - 1)
    )
  })
  
  result <- do.call(rbind, indices_list)

  result <- result[order(-result$n_documents), ]
  result$article_count <- result$n_documents
  total_documents <- sum(result$article_count, na.rm = TRUE)
  result$share <- if (is.finite(total_documents) && total_documents > 0) {
    result$article_count / total_documents
  } else {
    0
  }
  result$rank <- seq_len(nrow(result))

  result
}
