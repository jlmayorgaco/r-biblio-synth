# ============================================================================ #
# helpers__m1_data_ingestion/05_mapping.R
# Standardize column names/types for downstream modules
# ============================================================================ #

m1i_apply_standard_mapping <- function(df) {
  map <- c(
    AU = "Authors", AF = "Authors_Full", TI = "Title", SO = "Source_Title",
    J9 = "Journal_Abbrev", JI = "Journal_Short", LA = "Language", DT = "Document_Type",
    DI = "DOI", SN = "ISSN", url = "URL", VL = "Volume",
    DE = "Author_Keywords", ID = "Indexed_Keywords",
    C1 = "Affiliations", RP = "Corresponding_Author",
    TC = "Times_Cited", PY = "Year",
    CR = "Cited_References", AU_UN = "Author_Universities", AU1_UN = "Author_Universities_Alt",
    AU_UN_NR = "AU_UN_NR", SR_FULL = "Source_Ref_Full", SR = "Source_Ref",
    AB = "Abstract", pmid = "pmid", publication_stage = "publication_stage",
    PU = "Publisher", DB = "Database", BE = "BE", BN = "BN", coden = "coden",
    PN = "Pages", PP = "Pages_Extended"
  )

  present <- intersect(names(df), names(map))
  if (length(present) > 0) {
    new_names <- unname(map[present])
    colnames(df)[match(present, colnames(df))] <- new_names
  }

  if (!"Year" %in% names(df))        df$Year <- NA_integer_
  if (!"Times_Cited" %in% names(df)) df$Times_Cited <- NA_real_

  df$Year        <- suppressWarnings(as.integer(df$Year))
  df$Times_Cited <- suppressWarnings(as.numeric(df$Times_Cited))

  chr_candidates <- c("Authors","Authors_Full","Title","Source_Title","Journal_Abbrev",
                      "Journal_Short","Language","Document_Type","Affiliations",
                      "Corresponding_Author","DOI","ISSN","URL","Publisher","Database")
  for (nm in intersect(chr_candidates, names(df))) {
    df[[nm]] <- as.character(df[[nm]])
  }
  df
}
