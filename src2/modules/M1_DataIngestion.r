# ============================================================================ #
# M1_DataIngestion — Module 1: Load & Convert Bibliographic Data
# + Auto-discover Scopus/WoS files, merge & deduplicate
# + Year filter (keep <= year_max; default = last year)
# + PRISMA counters & diagram generation hooks
# ============================================================================ #

suppressPackageStartupMessages({
  library(bibliometrix)
  library(stringr)
  library(jsonlite)
})

M1_DEFAULT_YEAR_MAX <- as.integer(format(Sys.Date(), "%Y")) - 1L

# ---------- Load helpers (mirror of M5_Institutions) --------------------------
# If you already define `root` elsewhere, this will use it; otherwise fallback.
if (!exists("root", inherits = FALSE)) root <- getwd()

helpers_dir_m1 <- file.path(root, "helpers", "helpers__m1_data_ingestion")

# Always use the exact file name we created: 00_loader.R (capital R).
loader_path_m1 <- file.path(helpers_dir_m1, "00_loader.R")
if (!file.exists(loader_path_m1)) {
  stop("[M1] Missing helpers loader at: ", loader_path_m1,
       "\nExpected directory: ", helpers_dir_m1,
       "\nTip: ensure the folder is named helpers/helpers__m1_data_ingestion and the file is 00_loader.R")
}

# Source the loader and then load all helper files for M1 (m1i_*)
source(loader_path_m1, local = FALSE)
if (!exists("m1i_source_helpers", mode = "function")) {
  stop("[M1] m1i_source_helpers() not found after sourcing loader: ", loader_path_m1)
}
m1i_source_helpers(helpers_dir_m1)
# ----------------------------------------------------------------------------- 


M1_DataIngestion <- R6::R6Class(
  "M1_DataIngestion",
  inherit = M0_Abstract_Module,

  public = list(

    df = NULL,
    schema = NULL,

    initialize = function(bib_file         = NULL,
                          input_dir        = "data",
                          auto_discover    = TRUE,
                          index_json       = file.path("data", "index.json"),
                          dbsource         = "scopus",
                          format           = "bibtex",
                          prisma_filters   = list(
                            language = "English",
                            doc_types_keep = c("Article", "Conference Paper")
                          ),
                          prisma_out_dir   = "results2/figures",
                          prisma_format    = c("html","png"),
                          M_biblio         = NULL,
                          year_max         = M1_DEFAULT_YEAR_MAX,
                          ...) {

      self$input <- bib_file
      self$params$input_dir      <- input_dir
      self$params$auto_discover  <- isTRUE(auto_discover)

      self$params$dbsource <- dbsource
      self$params$format   <- format
      self$params$index_json <- index_json
      self$params$prisma_filters <- prisma_filters
      self$params$prisma_out_dir <- prisma_out_dir
      self$params$prisma_format  <- prisma_format
      self$params$year_max       <- as.integer(year_max)

      self$results <- list()
      self$plots   <- list()
    },

    run = function() {
      # 1) Discover sources (Scopus/WoS)
      sources <- m1i_discover_sources(
        input_dir     = self$params$input_dir,
        auto_discover = self$params$auto_discover
      )

      if (length(sources$scopus_files) + length(sources$wos_files) == 0) {
        if (!is.null(self$input) && file.exists(self$input)) {
          cat("[M1] Auto-discovery found nothing. Falling back to single file:", self$input, "\n")
        } else {
          stop("[M1] No input files found. Place scopus_*.bib and/or wos_*.txt in '",
               self$params$input_dir, "' or provide bib_file.")
        }
      } else {
        cat("[M1] Discovered files in '", self$params$input_dir, "':\n", sep = "")
        if (length(sources$scopus_files)) cat("  - Scopus: ", length(sources$scopus_files), " files\n", sep = "")
        if (length(sources$wos_files))    cat("  - WoS   : ", length(sources$wos_files), " files\n", sep = "")
      }

      # 2) Manual PRISMA counters from index.json (optional)
      manual_prisma <- list()
      if (!is.null(self$params$index_json) && file.exists(self$params$index_json)) {
        index_meta <- jsonlite::fromJSON(self$params$index_json, simplifyVector = TRUE)
        self$results$index_meta <- index_meta
        if (!is.null(index_meta$prisma)) manual_prisma <- index_meta$prisma
      }

      # 3) Convert each source
      M_scopus <- NULL
      M_wos    <- NULL

      if (length(sources$scopus_files)) {
        cat("[M1] Reading Scopus BibTeX ...\n")
        M_scopus <- m1i_safe_convert(files = sources$scopus_files, db = "scopus", format = "bibtex")
      }

      if (length(sources$wos_files)) {
        cat("[M1] Reading WoS PlainText ...\n")
        M_wos <- m1i_safe_convert(files = sources$wos_files, db = "wos", format = "plaintext")
      }

      # 4) Fallback single explicit file; else merge
      if (is.null(M_scopus) && is.null(M_wos) && !is.null(self$input) && file.exists(self$input)) {
        cat("[M1] Reading explicit file via parameters: ", self$input, "\n", sep = "")
        df_raw <- m1i_safe_convert(files = self$input, db = self$params$dbsource, format = self$params$format)
      } else {
        df_raw <- m1i_merge_sources(M_scopus, M_wos, dbsource_fallback = self$params$dbsource)
      }

      # Normalize class/DB attribute
      if (inherits(df_raw, "bibliometrixDB") && !inherits(df_raw, "bibliometrixData")) {
        class(df_raw) <- c("bibliometrixData", class(df_raw))
      }
      if (is.null(attr(df_raw, "DB")) || !nzchar(as.character(attr(df_raw, "DB")))) {
        attr(df_raw, "DB") <- if (!is.null(M_scopus) && !is.null(M_wos)) "MULTI" else
          c(scopus = "SCOPUS", wos = "ISI", openalex = "OPENALEX")[tolower(self$params$dbsource)] %||%
          toupper(self$params$dbsource)
      }

      m1i_dbg("class(df_raw): %s", paste(class(df_raw), collapse = ", "))
      m1i_dbg("attr(DB): %s", as.character(attr(df_raw, "DB")))
      m1i_dbg("rows: %s", tryCatch(nrow(df_raw), error = function(e) NA_integer_))

      # 5) Standard mapping
      df <- m1i_apply_standard_mapping(df_raw)

      # 6) Year filter (<= year_max)
      if (!is.null(self$params$year_max) && !is.na(self$params$year_max)) {
        pre_n <- nrow(df)
        df <- df[!is.na(df$Year) & df$Year <= self$params$year_max, , drop = FALSE]
        cat(sprintf("[M1] Year filter applied: keeping Year <= %d. %d → %d rows.\n",
                    self$params$year_max, pre_n, nrow(df)))
      }

      # 7) Optional country extraction (if helpers exist in your project)
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

      # 8) Store
      self$df <- df
      self$results$df <- df
      self$results$M_biblio <- df_raw

      # 9) Schema lock
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

      # 10) Metadata & EDA
      self$results$metadata <- list(
        n_docs    = nrow(df),
        n_columns = ncol(df),
        columns   = names(df),
        source    = attr(df_raw, "DB"),
        format    = "mixed",
        schema    = self$schema,
        discovered = sources
      )
      self$results$eda <- m1i_compute_eda(df)

      # 11) PRISMA counts
      self$results$prisma <- m1i_compute_prisma_counts(
        df               = df,
        filters          = self$params$prisma_filters,
        manual_overrides = manual_prisma
      )

      cat("[M1] Ingestion completed. Documents loaded:", nrow(df), "\n")
      invisible(self)
    },

    prisma_diagram = function() {
      if (is.null(self$results$prisma)) stop("[M1] PRISMA counters not found. Run() first.")
      out_dir <- self$params$prisma_out_dir
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

      p <- self$results$prisma

      if (m1i_try_require("PRISMA2020") &&
          "PRISMA_flowdiagram" %in% ls(getNamespace("PRISMA2020"))) {
        message("[M1] Generating PRISMA diagram via PRISMA2020...")
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
        if (m1i_try_require("htmlwidgets")) {
          if ("html" %in% self$params$prisma_format) {
            htmlwidgets::saveWidget(wdgt, file = file.path(out_dir, "PRISMA_2020.html"), selfcontained = TRUE)
          }
          if (m1i_try_require("webshot2") && "png" %in% self$params$prisma_format) {
            tmp_html <- tempfile(fileext = ".html")
            htmlwidgets::saveWidget(wdgt, file = tmp_html, selfcontained = TRUE)
            webshot2::webshot(tmp_html, file.path(out_dir, "PRISMA_2020.png"), vwidth = 1400, vheight = 1000)
          }
        } else {
          warning("[M1] htmlwidgets not installed; skipping PRISMA HTML/PNG export.")
        }
        return(invisible(TRUE))
      }

      if (m1i_try_require("prisma")) {
        message("[M1] 'prisma' package detected; implement fallback drawing here if desired.")
        return(invisible(FALSE))
      }

      message("[M1] No PRISMA diagram package found. Install one of:",
              "\n  remotes::install_github('nealhaddaway/PRISMA2020')",
              "\n  install.packages('prisma')")
      invisible(FALSE)
    },

   export = function(out_dir = "results2", subdir = "M1", formats = c("json","csv")) {
  if (is.null(self$df)) stop("[M1] Nothing to export — run() has not been executed.")

  # base dir (e.g., results2)
  base_dir <- m1i_ensure_dir(out_dir)
  # nested dir (e.g., results2/M1)
  final_dir <- if (nzchar(subdir)) m1i_ensure_dir(file.path(base_dir, subdir)) else base_dir

  if ("csv" %in% formats) {
    df_export <- tryCatch(m1i_flatten_list_columns_for_csv(self$df), error = function(e) self$df)
    csv_file  <- file.path(final_dir, "M1_DataIngestion.csv")
    utils::write.csv(df_export, csv_file, row.names = FALSE)
    cat("[M1] Exported CSV:", csv_file, "\n")
  }

  if ("json" %in% formats) {
    json_payload <- list(
      metadata = self$results$metadata,
      eda      = self$results$eda,
      prisma   = self$results$prisma
    )
    json_file <- file.path(final_dir, "M1_DataIngestion.json")
    if (exists("io_helpers", inherits = TRUE) && is.list(io_helpers) &&
        is.function(io_helpers$export_json)) {
      io_helpers$export_json(json_payload, json_file)
    } else {
      jsonlite::write_json(json_payload, json_file, pretty = TRUE, auto_unbox = TRUE)
    }
    cat("[M1] Exported JSON:", json_file, "\n")
  }

  invisible(self)
}

  )
)
