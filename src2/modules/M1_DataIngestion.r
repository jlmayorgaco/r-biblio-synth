# ================================================================
# M1_DataIngestion — Module 1: Load & Convert Bibliographic Data
# + PRISMA counters & diagram generation hooks
# ================================================================

M1_DataIngestion <- R6::R6Class(
  "M1_DataIngestion",
  inherit = M0_Abstract_Module,

  public = list(

    # ------------------------
    # Predeclared fields
    # ------------------------
    df = NULL,
    schema = NULL,

    initialize = function(bib_file,
                          index_json = "data/index.json",
                          dbsource   = "scopus",
                          format     = "bibtex",
                          prisma_filters = list(
                            language = "English",
                            doc_types_keep = c("Article", "Conference Paper")
                          ),
                          prisma_out_dir = "results2/figures",
                          prisma_format  = c("html","png")) {

      self$input <- bib_file
      self$params$dbsource <- dbsource
      self$params$format   <- format
      self$params$index_json <- index_json
      self$params$prisma_filters <- prisma_filters
      self$params$prisma_out_dir <- prisma_out_dir
      self$params$prisma_format  <- prisma_format
      self$results <- list()
      self$plots   <- list()
    },

    # ------------------------
    # Run ingestion pipeline
    # ------------------------
    run = function() {
      cat("[M1] Reading bibliographic data from:", self$input, "\n")

      # Load manual PRISMA counters from index.json (if present)
      manual_prisma <- list()
      if (!is.null(self$params$index_json) && file.exists(self$params$index_json)) {
        index_meta <- jsonlite::fromJSON(self$params$index_json, simplifyVector = TRUE)
        self$results$index_meta <- index_meta  # provenance
        if (!is.null(index_meta$prisma)) manual_prisma <- index_meta$prisma
      }

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

      # --- Keep standardized df in memory ---
      self$df <- df
      self$results$df <- df

      # --- Lock the schema ---
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

      # --- Metadata & compact EDA ---
      self$results$metadata <- list(
        n_docs    = nrow(df),
        n_columns = ncol(df),
        columns   = names(df),
        source    = self$params$dbsource,
        format    = self$params$format,
        schema    = self$schema
      )
      self$results$eda <- private$.compute_eda(df)

      # --- PRISMA counters (auto + manual overrides from index.json)
      self$results$prisma <- private$.compute_prisma_counts(
        df               = df,
        filters          = self$params$prisma_filters,
        manual_overrides = manual_prisma
      )

      self$prisma_diagram()

      cat("[M1] Ingestion completed. Documents loaded:", nrow(df), "\n")
      invisible(self)
    },

    # ------------------------
    # Generate PRISMA diagram (optional; call explicitly)
    # ------------------------
    prisma_diagram = function() {
      if (is.null(self$results$prisma))
        stop("[M1] PRISMA counters not found. Run() first.")

      out_dir <- self$params$prisma_out_dir
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

      p <- self$results$prisma

      # Prefer PRISMA2020 (Haddaway et al.)
      if (requireNamespace("PRISMA2020", quietly = TRUE)) {
        message("[M1] Generating PRISMA diagram via PRISMA2020...")
        if ("PRISMA_flowdiagram" %in% ls(getNamespace("PRISMA2020"))) {
          wdgt <- PRISMA2020::PRISMA_flowdiagram(
            database_results     = p$identified$db,
            other_results        = p$identified$other,
            duplicates           = p$removed$duplicates,
            records_screened     = p$screening$screened,
            records_excluded     = p$screening$excluded,
            reports_sought       = p$eligibility$sought,
            reports_notretrieved = p$eligibility$not_retrieved,
            reports_assessed     = p$eligibility$assessed,
            reports_excluded     = p$eligibility$excluded,
            new_studies          = p$included$studies,
            interactive          = FALSE
          )

          print('self$params$prisma_format')
          print(self$params$prisma_format)
          print('wdgt')
          print(wdgt)

          stop(' ======= STOP =======')

          if (requireNamespace("htmlwidgets", quietly = TRUE)) {
            if ("html" %in% self$params$prisma_format) {
              htmlwidgets::saveWidget(wdgt, file = file.path(out_dir, "PRISMA_2020.html"), selfcontained = TRUE)
            }
            if (requireNamespace("webshot2", quietly = TRUE) && "png" %in% self$params$prisma_format) {
              tmp_html <- tempfile(fileext = ".html")
              htmlwidgets::saveWidget(wdgt, file = tmp_html, selfcontained = TRUE)
              webshot2::webshot(tmp_html, file.path(out_dir, "PRISMA_2020.png"),
                                vwidth = 1400, vheight = 1000)
            }
          }
          else {
        stop(" NO htmlwidgets")
      }
          return(invisible(TRUE))
        } else {
        stop(" Non %in% ls(getNamespace(PRISMA2020)")
      }
        message("[M1] PRISMA2020 installed, but PRISMA_flowdiagram() not found; trying 'prisma' fallback.")
      } else {
        stop(" NO PRIMSA2020 ")
      }

      message("[M1] No PRISMA diagram package found. Install one of:\n",
              "  remotes::install_github('nealhaddaway/PRISMA2020')\n",
              "  install.packages('prisma')\n")
      invisible(FALSE)
    },

    # ------------------------
    # Export results
    # ------------------------
    export = function(out_dir = "results/", formats = c("json","csv")) {
      if (is.null(self$df)) stop("[M1] Nothing to export — run() has not been executed.")
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

      if ("csv" %in% formats) {
        df_export <- tryCatch(flatten_list_columns_for_csv(self$df), error = function(e) self$df)
        csv_file  <- file.path(out_dir, "M1_DataIngestion.csv")
        utils::write.csv(df_export, csv_file, row.names = FALSE)
        cat("[M1] Exported CSV:", csv_file, "\n")
      }

      if ("json" %in% formats) {
        json_payload <- list(
          metadata = self$results$metadata,
          eda      = self$results$eda,
          prisma   = self$results$prisma
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
    # Stable mapping
    # ------------------------------------------------------------
    .apply_standard_mapping = function(df) {
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
    },

    # ------------------------------------------------------------
    # Minimal EDA
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
    },

    # ------------------------------------------------------------
    # PRISMA counters (auto + manual)
    # ------------------------------------------------------------
    .compute_prisma_counts = function(df, filters, manual_overrides) {

      n_identified_db    <- nrow(df)                 # identified via databases
      n_identified_other <- as.integer(manual_overrides$identified_other %||% 0)

      # --- Duplicates (heuristic: DOI lowercase; fallback to normalized title)
      DOI <- tolower(ifelse(is.na(df$DOI), "", df$DOI))
      TitleNorm <- stringr::str_squish(tolower(ifelse(is.na(df$Title), "", df$Title)))
      key <- ifelse(DOI != "", paste0("doi:", DOI), paste0("ti:", TitleNorm))
      n_unique <- length(unique(key))
      n_duplicates <- max(n_identified_db + n_identified_other - n_unique, 0)

      # --- Automatic pre-screen (language + doc types)
      lang_keep  <- filters$language %||% "English"
      types_keep <- filters$doc_types_keep %||% c("Article","Conference Paper")

      lang_ok  <- df$Language %in% lang_keep
      type_ok  <- df$Document_Type %in% types_keep

      # Count how many fail language/type filters
      n_auto_excluded <- sum(!(lang_ok & type_ok), na.rm = TRUE)

      # Allow user to specify other removals before screening
      n_other_removed <- as.integer(manual_overrides$removed_other %||% 0)

      # --- Records screened (title/abstract)
      n_screened <- as.integer(manual_overrides$screened %||%
                                 (n_unique - n_duplicates - n_auto_excluded - n_other_removed))
      if (is.na(n_screened) || n_screened < 0) n_screened <- 0

      # --- Excluded at screening (title/abstract)
      n_excluded_screen <- as.integer(manual_overrides$excluded_screen %||% NA_integer_)

      # --- Eligibility (full-text)
      n_sought         <- as.integer(manual_overrides$sought %||% NA_integer_)
      n_notretrieved   <- as.integer(manual_overrides$not_retrieved %||% 0)
      n_assessed       <- as.integer(manual_overrides$assessed %||%
                                       if (!is.na(n_sought)) n_sought - n_notretrieved else NA_integer_)
      n_excluded_ft    <- as.integer(manual_overrides$excluded_fulltext %||% NA_integer_)

      # --- Included
      n_included       <- as.integer(manual_overrides$included %||% NA_integer_)

      list(
        identified = list(
          db    = n_identified_db,
          other = n_identified_other
        ),
        removed = list(
          duplicates = n_duplicates,
          auto       = max(n_auto_excluded, 0),
          other      = max(n_other_removed, 0)
        ),
        screening = list(
          screened = n_screened,
          excluded = n_excluded_screen
        ),
        eligibility = list(
          sought        = n_sought,
          not_retrieved = n_notretrieved,
          assessed      = n_assessed,
          excluded      = n_excluded_ft
        ),
        included = list(
          studies = n_included
        ),
        # Echo filters so methods are transparent
        filters = filters
      )
    }
  )
)

# small helper for NULL-coalescing
`%||%` <- function(a, b) if (is.null(a)) b else a
