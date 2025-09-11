# ================================================================
# M1_DataIngestion — Module 1: Load & Convert Bibliographic Data
# Schema-locked mapping -> friendly columns (Year, Times_Cited, ...)
# Raw df kept in memory (and in results$df for pipeline), NOT in JSON.
# JSON export = metadata + compact EDA (no rows).
# ================================================================

M1_DataIngestion <- R6::R6Class(
  "M1_DataIngestion",
  inherit = M0_Abstract_Module,

  public = list(

    # ------------------------
    # Predeclared fields (avoid R6 locked-field errors)
    # ------------------------
    df = NULL,       # standardized raw data (in-memory only)
    schema = NULL,   # locked schema (friendly column names)

    initialize = function(bib_file, dbsource = "scopus", format = "bibtex") {
      self$input <- bib_file
      self$params$dbsource <- dbsource
      self$params$format   <- format
      self$results <- list()
      self$plots   <- list()
    },

    # ------------------------
    # Run ingestion pipeline
    # ------------------------
    run = function() {
      cat("[M1] Reading bibliographic data from:", self$input, "\n")

      # --- Load raw data from bibliometrix ---
      df_raw <- tryCatch({
        suppressMessages(
          suppressWarnings(
            bibliometrix::convert2df(
              file     = self$input,
              dbsource = self$params$dbsource,
              format   = self$params$format
            )
          )
        )
      }, error = function(e) {
        stop("[M1] Error: could not read input file. ", conditionMessage(e))
      })

      # --- Apply stable mapping to friendly names & basic typing ---
      df <- private$.apply_standard_mapping(df_raw)

      # --- Country extraction if Affiliations present ---
      if ("Affiliations" %in% names(df)) {
        aff_chr <- as.character(df$Affiliations)

        # helpers expected in your codebase; fall back gracefully if absent
        extract_fun <- if (exists("extract_country_from_affiliation", mode = "function")) {
          get("extract_country_from_affiliation")
        } else function(x) NULL

        norm_fun <- if (exists("normalize_countries", mode = "function")) {
          get("normalize_countries")
        } else function(x) x

        df$Country_List <- lapply(aff_chr, function(x) {
          tryCatch(extract_fun(x), error = function(e) NULL)
        })
        df$Country_List <- tryCatch(norm_fun(df$Country_List), error = function(e) df$Country_List)

        df$Main_Country <- vapply(df$Country_List, function(x) {
          if (is.null(x) || length(x) == 0) return(NA_character_)
          x[1]
        }, FUN.VALUE = character(1))
      }

      # --- Keep standardized df in memory (NOT serialized to JSON) ---
      self$df <- df
      self$results$df <- df   # pipeline compatibility (downstream modules read here)

      # --- Lock the schema (what downstream can rely on) ---
      self$schema <- list(
        Year                 = "Year",
        Times_Cited          = "Times_Cited",
        Affiliations         = "Affiliations",
        Authors              = "Authors",
        Authors_Full         = "Authors_Full",
        Author_Keywords      = "Author_Keywords",
        Indexed_Keywords     = "Indexed_Keywords",
        Source_Title         = "Source_Title",
        Journal_Abbrev       = "Journal_Abbrev",
        Journal_Short        = "Journal_Short",
        Language             = "Language",
        Document_Type        = "Document_Type",
        Title                = "Title",
        DOI                  = "DOI",
        ISSN                 = "ISSN",
        Volume               = "Volume",
        URL                  = "URL",
        Cited_References     = "Cited_References",
        Author_Universities  = "Author_Universities",
        Author_Universities_Alt = "Author_Universities_Alt",
        Source_Ref_Full      = "Source_Ref_Full",
        Source_Ref           = "Source_Ref",
        Country_List         = "Country_List",
        Main_Country         = "Main_Country",
        Abstract             = "Abstract",
        Publisher            = "Publisher",
        Database             = "Database",
        Pages                = "Pages",
        Pages_Extended       = "Pages_Extended",
        pmid                 = "pmid",
        publication_stage    = "publication_stage",
        Corresponding_Author = "Corresponding_Author",
        coden                = "coden",
        BE                   = "BE",
        BN                   = "BN",
        AU_UN_NR             = "AU_UN_NR"
      )

      # --- Metadata & compact EDA (no rows) ---
      self$results$metadata <- list(
        n_docs    = nrow(df),
        n_columns = ncol(df),
        columns   = names(df),
        source    = self$params$dbsource,
        format    = self$params$format,
        schema    = self$schema
      )
      self$results$eda <- private$.compute_eda(df)

      cat("[M1] Ingestion completed. Documents loaded:", nrow(df), "\n")
      invisible(self)
    },

    # ------------------------
    # Export results
    # ------------------------
    export = function(out_dir = "results/", formats = c("json","csv")) {
      if (is.null(self$df)) stop("[M1] Nothing to export — run() has not been executed.")
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

      # CSV export of rows is optional/explicit
      if ("csv" %in% formats) {
        df_export <- tryCatch(flatten_list_columns_for_csv(self$df), error = function(e) self$df)
        csv_file  <- file.path(out_dir, "M1_DataIngestion.csv")
        utils::write.csv(df_export, csv_file, row.names = FALSE)
        cat("[M1] Exported CSV:", csv_file, "\n")
      }

      # JSON: summary only (no rows)
      if ("json" %in% formats) {
        json_payload <- list(
          metadata = self$results$metadata,
          eda      = self$results$eda
        )
        json_file <- file.path(out_dir, "M1_DataIngestion.json")
        io_helpers$export_json(json_payload, json_file)
        cat("[M1] Exported JSON:", json_file, "\n")
      }

      invisible(self)
    }
  ),

  private = list(

    # ------------------------------------------------------------
    # Stable mapping from Scopus/WoS-style codes to friendly names.
    # Only present columns are renamed; ensures critical fields & types.
    # ------------------------------------------------------------
    .apply_standard_mapping = function(df) {

      # Raw→Friendly map (extend as needed)
      map <- c(
        # authors, titles, source
        AU = "Authors",
        AF = "Authors_Full",
        TI = "Title",
        SO = "Source_Title",
        J9 = "Journal_Abbrev",
        JI = "Journal_Short",
        LA = "Language",
        DT = "Document_Type",

        # identifiers
        DI = "DOI",
        SN = "ISSN",
        url = "URL",
        VL = "Volume",

        # keywords
        DE = "Author_Keywords",
        ID = "Indexed_Keywords",

        # affiliations / addresses
        C1 = "Affiliations",
        RP = "Corresponding_Author",

        # citations & year
        TC = "Times_Cited",
        PY = "Year",

        # references & institutions
        CR      = "Cited_References",
        AU_UN   = "Author_Universities",
        AU1_UN  = "Author_Universities_Alt",
        AU_UN_NR= "AU_UN_NR",
        SR_FULL = "Source_Ref_Full",
        SR      = "Source_Ref",

        # misc (keep if present)
        AB = "Abstract",
        pmid = "pmid",
        publication_stage = "publication_stage",
        PU = "Publisher",
        DB = "Database",
        BE = "BE",
        BN = "BN",
        coden = "coden",
        PN = "Pages",
        PP = "Pages_Extended"
      )

      present <- intersect(names(df), names(map))
      if (length(present) > 0) {
        new_names <- unname(map[present])
        colnames(df)[match(present, colnames(df))] <- new_names
      }

      # Ensure critical fields exist and are correctly typed
      if (!"Year" %in% names(df))        df$Year <- NA_integer_
      if (!"Times_Cited" %in% names(df)) df$Times_Cited <- NA_real_

      df$Year        <- suppressWarnings(as.integer(df$Year))
      df$Times_Cited <- suppressWarnings(as.numeric(df$Times_Cited))

      # Coerce common textuals to character (avoid factors)
      chr_candidates <- c("Authors","Authors_Full","Title","Source_Title","Journal_Abbrev",
                          "Journal_Short","Language","Document_Type","Affiliations",
                          "Corresponding_Author","DOI","ISSN","URL","Publisher","Database")
      for (nm in intersect(chr_candidates, names(df))) {
        df[[nm]] <- as.character(df[[nm]])
      }

      df
    },

    # ------------------------------------------------------------
    # Minimal, robust EDA (no row-level data)
    # ------------------------------------------------------------
    .compute_eda = function(df) {
      n <- nrow(df)
      cols <- names(df)

      gcol <- function(name) if (name %in% cols) df[[name]] else NULL
      topn <- function(vec, n = 10) {
        if (is.null(vec)) return(NULL)
        tb <- sort(table(vec), decreasing = TRUE)
        as.list(head(tb, n))
      }

      years        <- suppressWarnings(as.integer(gcol("Year")))
      doc_types    <- gcol("Document_Type")
      langs        <- gcol("Language")
      source_titles<- gcol("Source_Title")
      times_cited  <- suppressWarnings(as.numeric(gcol("Times_Cited")))
      main_country <- gcol("Main_Country")

      miss_pct <- lapply(cols, function(cl) {
        x <- df[[cl]]
        is_blank <- is.na(x)
        if (is.character(x)) is_blank <- is_blank | (trimws(x) == "")
        round(mean(is_blank) * 100, 2)
      })
      names(miss_pct) <- cols

      year_span <- if (!all(is.na(years))) {
        rng <- range(years, na.rm = TRUE)
        list(min = rng[1], max = rng[2])
      } else NULL

      tc_stats <- if (!all(is.na(times_cited))) {
        qs <- stats::quantile(times_cited, probs = c(0, .25, .5, .75, .9, .95, 1), na.rm = TRUE, names = FALSE)
        list(
          n_with_tc = sum(!is.na(times_cited)),
          mean      = round(mean(times_cited, na.rm = TRUE), 3),
          sd        = round(stats::sd(times_cited, na.rm = TRUE), 3),
          quantiles = setNames(round(qs, 3), c("min","p25","p50","p75","p90","p95","max"))
        )
      } else NULL

      top_sources   <- topn(source_titles, 10)
      top_types     <- topn(doc_types, 10)
      top_langs     <- topn(langs, 10)
      top_countries <- topn(main_country, 10)

      country_coverage <- if ("Country_List" %in% cols) {
        with_country <- vapply(df$Country_List, function(x) !is.null(x) && length(x) > 0, logical(1))
        round(mean(with_country) * 100, 2)
      } else NA_real_

      list(
        n_docs               = n,
        year_span            = year_span,
        doc_types_top10      = top_types,
        languages_top10      = top_langs,
        sources_top10        = top_sources,
        countries_top10      = top_countries,
        times_cited_stats    = tc_stats,
        country_coverage_pct = country_coverage,
        missingness_pct      = miss_pct
      )
    }
  )
)
