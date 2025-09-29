# ---- Simple theme extraction stub (extend as needed) ------------------------

m5i_build_themes <- function(df_docs, text_col = NULL, top_n = 20) {
  # If you don’t have text, return empty.
  if (is.null(text_col) || !text_col %in% names(df_docs)) {
    return(list(info = "No text column supplied to m5i_build_themes()", top_terms = data.frame()))
  }
  txt <- tolower(df_docs[[text_col]])
  txt <- gsub("[^a-z0-9\\s]", " ", txt)
  toks <- unlist(strsplit(txt, "\\s+")); toks <- toks[nchar(toks) > 3]
  top <- sort(table(toks), decreasing = TRUE)
  data.frame(term = names(top)[1:min(top_n, length(top))], n = as.integer(top)[1:min(top_n, length(top))])
}
