# ============================================================================
# m0_semantic_similarity.R - Semantic Similarity for Deduplication
# ============================================================================
# Uses text embeddings and semantic similarity for improved title matching

#' Semantic title similarity using TF-IDF
#'
#' @param titles Character vector of titles
#' @param threshold Similarity threshold for considering matches (default 0.8)
#' @return Matrix of similarity scores
#' @export
m0_semantic_similarity <- function(titles, threshold = 0.8) {
  if (length(titles) < 2) {
    return(matrix(1, 1, 1, dimnames = list(1, 1)))
  }
  
  # Normalize titles
  titles_norm <- normalize_titles(titles)
  
  # Create TF-IDF matrix
  tfidf_matrix <- create_tfidf_matrix(titles_norm)
  
  if (is.null(tfidf_matrix) || ncol(tfidf_matrix) == 0) {
    # Fallback to simple character-based similarity
    return(char_similarity_matrix(titles))
  }
  
  # Compute cosine similarity
  n <- nrow(tfidf_matrix)
  sim_matrix <- matrix(0, n, n)
  
  for (i in 1:n) {
    for (j in i:n) {
      if (i == j) {
        sim_matrix[i, j] <- 1
      } else {
        sim <- cosine_similarity(tfidf_matrix[i, ], tfidf_matrix[j, ])
        sim_matrix[i, j] <- sim
        sim_matrix[j, i] <- sim
      }
    }
  }
  
  rownames(sim_matrix) <- colnames(sim_matrix) <- seq_len(n)
  
  sim_matrix
}

#' Normalize titles for comparison
#' @keywords internal
normalize_titles <- function(titles) {
  titles <- tolower(as.character(titles))
  titles <- gsub("[[:punct:]]", " ", titles)
  titles <- gsub("[[:digit:]]", " ", titles)
  titles <- gsub("\\s+", " ", titles)
  titles <- trimws(titles)
  
  # Remove common stop words
  stopwords <- c("the", "a", "an", "and", "or", "but", "in", "on", "at", 
                 "to", "for", "of", "with", "by", "from", "is", "are", "was",
                 "were", "be", "been", "being", "have", "has", "had", "do",
                 "does", "did", "will", "would", "could", "should", "may",
                 "might", "must", "shall", "can", "need", "dare", "ought",
                 "used", "using", "use", "this", "that", "these", "those")
  
  for (sw in stopwords) {
    titles <- gsub(paste0("\\b", sw, "\\b"), "", titles)
  }
  
  titles <- gsub("\\s+", " ", titles)
  trimws(titles)
}

#' Create TF-IDF matrix
#' @keywords internal
create_tfidf_matrix <- function(titles) {
  # Tokenize
  tokens <- strsplit(titles, "\\s+")
  
  # Build vocabulary
  vocab <- unique(unlist(tokens))
  vocab <- vocab[vocab != "" & nchar(vocab) > 1]
  
  if (length(vocab) == 0) return(NULL)
  
  n <- length(titles)
  
  # Term frequency
  tf <- matrix(0, n, length(vocab))
  colnames(tf) <- vocab
  
  for (i in seq_along(tokens)) {
    token_freq <- table(tokens[[i]])
    for (term in names(token_freq)) {
      if (term %in% vocab) {
        tf[i, term] <- token_freq[term]
      }
    }
  }
  
  # Document frequency
  df <- colSums(tf > 0)
  
  # IDF
  idf <- log(n / (df + 1))
  
  # TF-IDF
  tfidf <- tf * matrix(idf, nrow = n, ncol = length(vocab), byrow = TRUE)
  
  # Normalize
  row_norms <- sqrt(rowSums(tfidf^2))
  row_norms[row_norms == 0] <- 1
  tfidf <- tfidf / row_norms
  
  tfidf
}

#' Cosine similarity
#' @keywords internal
cosine_similarity <- function(v1, v2) {
  if (length(v1) == 0 || length(v2) == 0) return(0)
  
  dot_product <- sum(v1 * v2, na.rm = TRUE)
  norm1 <- sqrt(sum(v1^2, na.rm = TRUE))
  norm2 <- sqrt(sum(v2^2, na.rm = TRUE))
  
  if (norm1 == 0 || norm2 == 0) return(0)
  
  dot_product / (norm1 * norm2)
}

#' Character similarity matrix (fallback)
#' @keywords internal
char_similarity_matrix <- function(titles) {
  n <- length(titles)
  sim_matrix <- matrix(0, n, n)
  
  for (i in 1:n) {
    for (j in i:n) {
      if (i == j) {
        sim_matrix[i, j] <- 1
      } else {
        sim <- levenshtein_similarity(titles[i], titles[j])
        sim_matrix[i, j] <- sim
        sim_matrix[j, i] <- sim
      }
    }
  }
  
  sim_matrix
}

#' Levenshtein similarity (normalized)
#' @keywords internal
levenshtein_similarity <- function(s1, s2) {
  if (is.na(s1) || is.na(s2)) return(0)
  
  dist <- adist(s1, s2)[1, 1]
  max_len <- max(nchar(s1), nchar(s2))
  
  if (max_len == 0) return(1)
  
  1 - dist / max_len
}

#' Jaccard similarity for tokenized strings
#'
#' @param s1 First string
#' @param s2 Second string
#' @return Jaccard similarity score
#' @export
jaccard_similarity <- function(s1, s2) {
  if (is.na(s1) || is.na(s2)) return(0)
  
  s1 <- tolower(s1)
  s2 <- tolower(s2)
  
  tokens1 <- unique(strsplit(s1, "\\s+")[[1]])
  tokens2 <- unique(strsplit(s2, "\\s+")[[1]])
  
  tokens1 <- tokens1[tokens1 != ""]
  tokens2 <- tokens2[tokens2 != ""]
  
  if (length(tokens1) == 0 || length(tokens2) == 0) return(0)
  
  intersection <- length(intersect(tokens1, tokens2))
  union <- length(union(tokens1, tokens2))
  
  if (union == 0) return(0)
  
  intersection / union
}

#' Find potential duplicates using semantic similarity
#'
#' @param titles Character vector of titles
#' @param threshold Similarity threshold (default 0.85)
#' @param n_grams Also use n-gram overlap (default TRUE)
#' @return List with potential duplicate pairs
#' @export
m0_find_duplicates_semantic <- function(titles, threshold = 0.85, n_grams = TRUE) {
  n <- length(titles)
  
  if (n < 2) {
    return(list(
      duplicates = data.frame(),
      n_pairs = 0,
      status = "insufficient data"
    ))
  }
  
  # Compute similarity matrix
  sim_matrix <- m0_semantic_similarity(titles, threshold)
  
  # Find pairs above threshold
  duplicates <- data.frame(
    index1 = integer(0),
    index2 = integer(0),
    title1 = character(0),
    title2 = character(0),
    similarity = numeric(0),
    jaccard = numeric(0),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      if (sim_matrix[i, j] >= threshold) {
        jaccard <- if (n_grams) jaccard_similarity(titles[i], titles[j]) else NA
        
        duplicates <- rbind(duplicates, data.frame(
          index1 = i,
          index2 = j,
          title1 = substr(titles[i], 1, 100),
          title2 = substr(titles[j], 1, 100),
          similarity = round(sim_matrix[i, j], 4),
          jaccard = round(jaccard, 4),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  duplicates <- duplicates[order(-duplicates$similarity), ]
  
  list(
    duplicates = duplicates,
    n_pairs = nrow(duplicates),
    n_examined = n * (n - 1) / 2,
    threshold = threshold,
    status = "success"
  )
}

#' Author matching with ORCID support
#'
#' @param authors1 First author list
#' @param authors2 Second author list
#' @param orcids1 Corresponding ORCIDs for first list
#' @param orcids2 Corresponding ORCIDs for second list
#' @return List with match scores
#' @export
m0_match_authors <- function(authors1, authors2, orcids1 = NULL, orcids2 = NULL) {
  n1 <- length(authors1)
  n2 <- length(authors2)
  
  matches <- data.frame(
    author1 = character(0),
    author2 = character(0),
    orcid_match = logical(0),
    name_match = numeric(0),
    overall_match = numeric(0),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(authors1)) {
    for (j in seq_along(authors2)) {
      # ORCID match
      orcid_match <- FALSE
      if (!is.null(orcids1) && !is.null(orcids2)) {
        if (!is.na(orcids1[i]) && !is.na(orcids2[j])) {
          orcid_match <- orcids1[i] == orcids2[j]
        }
      }
      
      # Name match
      name_sim <- author_name_similarity(authors1[i], authors2[j])
      
      # Overall match score
      overall <- if (orcid_match) {
        1.0
      } else if (name_sim > 0.9) {
        0.95
      } else {
        name_sim
      }
      
      if (overall > 0.7) {
        matches <- rbind(matches, data.frame(
          author1 = authors1[i],
          author2 = authors2[j],
          orcid_match = orcid_match,
          name_match = round(name_sim, 4),
          overall_match = round(overall, 4),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  matches <- matches[order(-matches$overall_match), ]
  
  list(
    matches = matches,
    n_matches = nrow(matches),
    n_orcid_matches = sum(matches$orcid_match),
    status = "success"
  )
}

#' Author name similarity
#' @keywords internal
author_name_similarity <- function(name1, name2) {
  if (is.na(name1) || is.na(name2)) return(0)
  
  name1 <- normalize_author_name(name1)
  name2 <- normalize_author_name(name2)
  
  # Extract last name and initials
  parts1 <- strsplit(name1, "\\s+")[[1]]
  parts2 <- strsplit(name2, "\\s+")[[1]]
  
  if (length(parts1) == 0 || length(parts2) == 0) return(0)
  
  # Last name comparison (most important)
  last1 <- parts1[length(parts1)]
  last2 <- parts2[length(parts2)]
  
  last_sim <- if (last1 == last2) 1 else {
    levenshtein_similarity(last1, last2)
  }
  
  if (last_sim < 0.7) return(last_sim * 0.5)  # Last name mismatch is decisive
  
  # Initials comparison
  initials1 <- if (length(parts1) > 1) {
    paste(substr(parts1[-length(parts1)], 1, 1), collapse = "")
  } else ""
  
  initials2 <- if (length(parts2) > 1) {
    paste(substr(parts2[-length(parts2)], 1, 1), collapse = "")
  } else ""
  
  init_sim <- jaccard_similarity(initials1, initials2)
  
  # Weight: 60% last name, 40% initials
  last_sim * 0.6 + init_sim * 0.4
}

#' Normalize author name
#' @keywords internal
normalize_author_name <- function(name) {
  name <- tolower(name)
  name <- gsub("[[:punct:]]", " ", name)
  name <- gsub("\\s+", " ", name)
  name <- trimws(name)
  
  # Common name variations
  name <- gsub("\\bdr\\b", "", name)
  name <- gsub("\\bprof\\b", "", name)
  name <- gsub("\\bmr\\b", "", name)
  name <- gsub("\\bmrs\\b", "", name)
  name <- gsub("\\bms\\b", "", name)
  name <- gsub("\\bjr\\b", "", name)
  name <- gsub("\\bsr\\b", "", name)
  name <- gsub("\\biii\\b", "", name)
  name <- gsub("\\bii\\b", "", name)
  
  name <- gsub("\\s+", " ", name)
  trimws(name)
}

#' Journal name disambiguation
#'
#' @param journals Character vector of journal names
#' @return Data frame with normalized names and groups
#' @export
m0_disambiguate_journals <- function(journals) {
  journals <- as.character(journals)
  n <- length(journals)
  
  # Normalize
  journals_norm <- tolower(journals)
  journals_norm <- gsub("[[:punct:]]", " ", journals_norm)
  journals_norm <- gsub("\\s+", " ", journals_norm)
  journals_norm <- trimws(journals_norm)
  
  # Common abbreviation expansions
  abbrev_map <- c(
    "j " = "journal ",
    " j$" = " journal",
    " j " = " journal ",
    "proc" = "proceedings",
    "trans" = "transactions",
    "int" = "international",
    "nat" = "national",
    "acad" = "academy",
    "inst" = "institute",
    "univ" = "university",
    "res" = "research",
    "tech" = "technical",
    "sci" = "science",
    "eng" = "engineering",
    "med" = "medicine",
    "biol" = "biology",
    "phys" = "physics",
    "chem" = "chemistry",
    "math" = "mathematics",
    "comput" = "computational"
  )
  
  for (abbr in names(abbrev_map)) {
    journals_norm <- gsub(abbr, abbrev_map[abbr], journals_norm)
  }
  
  # Create groups
  groups <- data.frame(
    original = journals,
    normalized = journals_norm,
    group_id = integer(n),
    canonical_name = character(n),
    stringsAsFactors = FALSE
  )
  
  # Group by similarity
  processed <- rep(FALSE, n)
  group_id <- 1
  
  for (i in 1:n) {
    if (processed[i]) next
    
    # Find similar journals
    similar <- which(!processed & 
                     (journals_norm == journals_norm[i] |
                      sapply(journals_norm, function(x) jaccard_similarity(x, journals_norm[i])) > 0.85))
    
    groups$group_id[similar] <- group_id
    groups$canonical_name[similar] <- journals[i]  # Use first occurrence as canonical
    processed[similar] <- TRUE
    group_id <- group_id + 1
  }
  
  # Summary
  unique_groups <- length(unique(groups$group_id))
  
  list(
    journals = groups,
    n_unique = unique_groups,
    n_duplicates = n - unique_groups,
    status = "success"
  )
}
