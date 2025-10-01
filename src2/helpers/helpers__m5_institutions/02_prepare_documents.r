# ============================================================================ #
# helpers__m5_institutions/02_prepare_documents.R
# Institution extraction & normalization for M5_Institutions
# - Uses bibliometrix::metaTagExtraction(Field = "AU_UN") exclusively
# - Always returns: doc_id, Institution_List, n_institutions
# - Attaches attr(., "institutions_long") for diagnostics
# Toggle debug: options(m5i.debug = TRUE)
# ============================================================================ #

# ---- Minimal config used elsewhere (keep here for M5 defaults) --------------
m5i_default_cfg <- function() {
  list(
    ieee = list(width_in = 3.42, height_in = 2.2, dpi = 600),
    quadrants = list(
      out_dir = "results2/M5/quadrants",
      w_in = 3.42, h_in = 2.4, dpi = 600,
      xlab = "Publications (TP)", ylab = "Citations (TC)"
    ),
    collab = list(min_edge_weight = 1, layout = "fr", sep_regex = "[;|,]"),
    report_path = "results2/M5"
  )
}

# ---- Utilities ---------------------------------------------------------------

# Null-coalescing
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
}

# tiny debug helper (quiet unless option enabled)
.m5i_dbg <- function(...) {
  if (isTRUE(getOption("m5i.debug", FALSE))) cat("[M5I][debug]", ..., "\n")
}

# normalize basic whitespace/case
m5i_norm <- function(x) {
  x <- trimws(tolower(x))
  gsub("\\s+", " ", x)
}

# token cleaning (remove obvious non-institution fragments)
.m5i_clean_token <- function(x) {
  x <- trimws(gsub("\\s+", " ", as.character(x)))
  x <- gsub("^[-#/\\.,;: ]+|[-#/\\.,;: ]+$", "", x)  # trim punctuation
  bad <- grepl(
    paste0(
      "^\\d{1,4}([ -]?\\d{1,4})*$",                  # pure numbers / codes
      "|^\\d{2,}-\\d{2,}$",                          # postal-like codes
      "|^[A-Z]?\\d{2,}$",                            # building codes
      "|^(DEPT|DEPARTMENT|SYST\\.? ENG\\.?|LAB|BLDG|ROOM|RM|BOX)\\b"  # lone depts
    ),
    x, ignore.case = TRUE
  )
  x[bad] <- ""
  keep <- grepl("\\b[[:alpha:]]{3,}\\b", x)         # requires at least one 3+ letter word
  x[!keep] <- ""
  x
}

# quick heuristic to prioritize real institutions
.m5i_score_token <- function(x) {
  kw <- c("UNIVERSITY","UNIV","INSTITUTE","INSTITUT","INSTITUTO","COLLEGE",
          "POLITECN","POLYTECH","TECHNOLOGY","TECHNOL","NATIONAL","STATE",
          "CENTRE","CENTER","LABORATORY","SCHOOL","FACULT","ACADEMY",
          "HOSPITAL","CNRS","INRIA","CSIC","CONICET","UST","MIT","ETH","EPFL")
  s <- 0
  s <- s + 2 * grepl(paste(kw, collapse="|"), toupper(x))
  s <- s + 1 * grepl("\\b(OF|DE|DI|DA|DEL|THE)\\b", toupper(x))
  s <- s - 1 * (nchar(x) < 6)
  s <- s - 1 * grepl("[/\\\\#]", x)
  as.integer(s)
}

# Title Case but preserve common acronyms
.m5i_normalize_case <- function(x) {
  acr <- c("CNRS","INRIA","CSIC","USA","UK","UNAM","USP","UFRJ","EPFL","ETH","MIT","TUM","TU","UNIV")
  y <- tools::toTitleCase(tolower(x))
  for (a in acr) y[toupper(x) == a] <- a
  y
}

# Apply alias map. Supports either:
#  - columns ALIAS / NORMALIZED
#  - columns alias / canonical
.m5i_alias_apply_generic <- function(tokens, inst_alias) {
  if (is.null(inst_alias) || !length(tokens)) return(tokens)
  up <- toupper(names(inst_alias))

  if (all(c("ALIAS","NORMALIZED") %in% up)) {
    alias_col <- inst_alias[[which(up == "ALIAS")[1]]]
    norm_col  <- inst_alias[[which(up == "NORMALIZED")[1]]]
    lut <- stats::setNames(trimws(as.character(norm_col)),
                           m5i_norm(trimws(as.character(alias_col))))
    out <- trimws(tokens)
    out_norm <- m5i_norm(out)
    hit <- out_norm %in% names(lut)
    out[hit] <- unname(lut[out_norm[hit]])
    return(unique(out))
  }

  if (all(c("ALIAS","CANONICAL") %in% up) || all(c("alias","canonical") %in% names(inst_alias))) {
    alias_col <- inst_alias[[which(up == "ALIAS")[1]]]
    canon_col <- inst_alias[[which(up == "CANONICAL")[1]]]
    lut <- stats::setNames(trimws(as.character(canon_col)),
                           m5i_norm(trimws(as.character(alias_col))))
    out <- trimws(tokens)
    out_norm <- m5i_norm(out)
    hit <- out_norm %in% names(lut)
    out[hit] <- unname(lut[out_norm[hit]])
    return(unique(out))
  }

  tokens
}

# ============================================================================ #
# Public API
# ============================================================================ #

# m5i_prepare_documents — institution list per document using bibliometrix AU_UN
# Notes:
#  - Requires M_biblio + bibliometrix; otherwise returns empty Institution_List.
#  - Other arguments are accepted for API compatibility but ignored.
m5i_prepare_documents <- function(df,
                                  M_biblio = NULL,
                                  inst_alias = NULL,
                                  inst_col_guess = NULL,  # ignored (compat)
                                  year_col = NULL,        # ignored (compat)
                                  citation_col = NULL) {  # ignored (compat)
  stopifnot(is.data.frame(df))

  # Default empty output (never break downstream)
  .empty_out <- function(d) {
    out <- d
    out$doc_id           <- seq_len(nrow(out))
    out$Institution_List <- rep("", nrow(out))
    out$n_institutions   <- rep(0L, nrow(out))
    attr(out, "institutions_long") <- data.frame(doc_id = integer(0), Institution = character(0))
    out
  }

  if (is.null(M_biblio) || !requireNamespace("bibliometrix", quietly = TRUE)) {
    .m5i_dbg("No M_biblio or bibliometrix not installed; returning empty Institution_List.")
    return(.empty_out(df))
  }

  # Extract AU_UN via bibliometrix
  au <- tryCatch(
    bibliometrix::metaTagExtraction(M_biblio, Field = "AU_UN", sep = ";"),
    error = function(e) { .m5i_dbg("metaTagExtraction AU_UN error:", conditionMessage(e)); NULL },
    warning = function(w) { invokeRestart("muffleWarning") }
  )
  if (is.null(au) || !"AU_UN" %in% names(au)) {
    .m5i_dbg("AU_UN field not available; returning empty Institution_List.")
    return(.empty_out(df))
  }

  # Normalize to list of string tokens per doc
  v <- au$AU_UN
  inst_list <- if (is.list(v)) {
    lapply(v, function(tokens) {
      tok <- unique(trimws(unlist(strsplit(paste(tokens, collapse=";"), ";", fixed = TRUE))))
      tok[nzchar(tok)]
    })
  } else {
    lapply(v, function(s) {
      tok <- unique(trimws(unlist(strsplit(as.character(s), ";", fixed = TRUE))))
      tok[nzchar(tok)]
    })
  }

  # Clean, score, alias-map, normalize case
  inst_list <- lapply(inst_list, function(tokens) {
    if (!length(tokens)) return(character(0))
    tokens <- .m5i_clean_token(tokens)
    tokens <- tokens[nzchar(tokens)]
    if (!length(tokens)) return(character(0))
    sc <- .m5i_score_token(tokens)
    tokens <- tokens[order(-sc, nchar(tokens), tokens)]
    tokens <- .m5i_alias_apply_generic(tokens, inst_alias)
    tokens <- .m5i_normalize_case(tokens)
    unique(tokens)
  })

  # Build final columns
  inst_str <- vapply(inst_list, function(tks) paste(tks, collapse = "; "), character(1))
  n_inst   <- vapply(inst_list, function(tks) length(unique(tks)), integer(1))

  out <- df
  out$doc_id           <- seq_len(nrow(out))
  out$Institution_List <- inst_str
  out$n_institutions   <- n_inst

  # Attach long attribute for diagnostics
  long <- if (length(inst_list)) {
    do.call(rbind, lapply(seq_along(inst_list), function(i) {
      if (!length(inst_list[[i]])) return(NULL)
      data.frame(doc_id = i, Institution = inst_list[[i]], stringsAsFactors = FALSE)
    }))
  } else data.frame(doc_id = integer(0), Institution = character(0))
  if (is.null(long)) long <- data.frame(doc_id = integer(0), Institution = character(0))
  attr(out, "institutions_long") <- long

  out
}

# ============================================================================ #
# Diagnostics (optional): quick sanity checks on extraction quality
# ============================================================================ #
m5i_debug_institutions <- function(df, top_n = 30, bad_n = 20) {
  cat("\n[M5I][debug] ===== Institutions diagnostics =====\n")

  if (!"n_institutions" %in% names(df)) {
    cat("[M5I][debug] n_institutions column not found. Did m5i_prepare_documents() run?\n")
    return(invisible(NULL))
  }

  n_docs        <- nrow(df)
  n_with_insts  <- sum(df$n_institutions > 0, na.rm = TRUE)
  n_empty       <- sum(df$n_institutions == 0 | is.na(df$n_institutions))
  cat(sprintf("[M5I][debug] Docs total: %d | with institutions: %d | empty: %d\n",
              n_docs, n_with_insts, n_empty))

  inst_long <- attr(df, "institutions_long")
  if (is.null(inst_long) || !is.data.frame(inst_long) || !"Institution" %in% names(inst_long)) {
    cat("[M5I][debug] institutions_long attribute not found or invalid.\n")
    return(invisible(NULL))
  }

  cat(sprintf("[M5I][debug] Top %d institutions:\n", top_n))
  if (requireNamespace("dplyr", quietly = TRUE)) {
    top_tbl <- dplyr::count(inst_long, Institution, sort = TRUE) %>% utils::head(top_n)
  } else {
    top_tbl <- as.data.frame(sort(table(inst_long$Institution), decreasing = TRUE))
    names(top_tbl) <- c("Institution", "n")
    top_tbl <- utils::head(top_tbl, top_n)
  }
  print(top_tbl, row.names = FALSE)

  bad_rx <- "^#|\\d{2,}-\\d{2,}|/|^RM\\b|^BLDG\\b"
  if (requireNamespace("dplyr", quietly = TRUE)) {
    bad_tbl <- dplyr::filter(inst_long, grepl(bad_rx, Institution, ignore.case = TRUE)) %>% utils::head(bad_n)
  } else {
    idx <- grepl(bad_rx, inst_long$Institution, ignore.case = TRUE)
    bad_tbl <- utils::head(inst_long[idx, , drop = FALSE], bad_n)
  }

  cat(sprintf("[M5I][debug] Sample of %d suspicious tokens:\n", bad_n))
  if (nrow(bad_tbl)) print(bad_tbl, row.names = FALSE) else cat("[M5I][debug] (none found)\n")
  cat("[M5I][debug] ======================================\n\n")
  invisible(NULL)
}
