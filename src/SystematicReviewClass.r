# ---------------------------------------------------------------------------- #
# -- SystematicReviewClass.r ------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# -- Packages and Install Dependencies
# ---------------------------------------------------------------------------- #

# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Function to install and load packages
install_and_load <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if (length(new_packages)) install.packages(new_packages, dependencies = TRUE)
  invisible(lapply(packages, require, character.only = TRUE))
}

# List of required packages
packages <- c("bibliometrix", "rworldmap",  "maps", "ggwordcloud", "ggsci", "changepoint", "lomb", "WaveletComp", "kableExtra", "jsonlite", 
              "pander", "rlang", "dplyr", "broom", "Metrics", "knitr", "ggplot2",  "tidyr",
              "plotly", "webshot", "gridExtra", "igraph", "nls2", "reshape2", "minpack.lm", "htmlwidgets")

# Install and load necessary packages
install_and_load(packages)


# ---------------------------------------------------------------------------- #
# -- Loading Modules
# ---------------------------------------------------------------------------- #
# Load inner libraries and functions
source('../../src/Config/PlotThemes.r')
source('../../src/M1_Main_Information/M1_Main_Information.r')
#source('../../src/M2_Annual_Production/M2_Annual_Production.r')
#source('../../src/M3_Most_Prod_Authors/M3_Most_Prod_Authors.r')

# ---------------------------------------------------------------------------- #
# -- Class Definition
# ---------------------------------------------------------------------------- #
SystematicReview <- setRefClass(

  # -------------------------------------------- #
  # ---- Systematic Review Class --------------- #
  # -------------------------------------------- #
  "SystematicReview",
  
  # -------------------------------------------- #
  # ---- Systematic Review Attributes ---------- #
  # -------------------------------------------- #
  fields = list(
    title = "character",
    date = "character",
    query = "character",
    bibPath = "character",
    keywords = "character",
    data = "ANY",
    results = "list"
  ),

  # -------------------------------------------- #
  # ---- Systematic Review Methods ------------- #
  # -------------------------------------------- #
  methods = list(

    # Constructor
    new = function() {
      title <<- ''
      date <<- ''
      query <<- ''
      bibPath <<- ''
      keywords <<- character()
      data <<- NULL
      results <<- list(
        main_information = list()
      )
    },

    # Setters
    setTitle = function(title) {
      .self$title <- title
    },
    setDate = function(date) {
      .self$date <- date
    },
    setQuery = function(query) {
      .self$query <- query
    },
    setBibPath = function(bibPath) {
      .self$bibPath <- bibPath
    },
    setKeywords = function(keywords) {
      .self$keywords <- keywords
    },

    # Initialize data
    init = function() {

      # Welcome message
      message("\n =============================== >")
      message(" == SystematicReview :: init...")
      message(" =============================== >\n")

      # Paths
      bib_path <- .self$bibPath

      # Convert BibTeX data to dataframe
      .self$data <- tryCatch({
        convert2df(bib_path, dbsource = "scopus", format = "bibtex")
      }, error = function(e) {
        stop("Error: Could not convert BibTeX file to dataframe. ", conditionMessage(e))
      })

    },

    # Check health status
    do_m0_check_health_status = function() {
      # Placeholder for health status check logic
      message("\n M0 :: Checking health status...\n")
    },

    # Check required columns
    do_m0_check_required_columns = function() {
      # Placeholder for checking required columns
      message("\n M0 :: Checking required columns...\n")
    },

    # ---------------------------------------------------------------------------- #
    # -- Module 1: Data Cleaning ------------------------------------------------- #
    # ---------------------------------------------------------------------------- #
    do_m0_cleaning_data = function() {
      # Placeholder for data cleaning logic
      message("\n M1 :: Cleaning data ...\n")

      # Check for missing mandatory tags
      res <- tryCatch({
        missingData(.self$data)
      }, error = function(e) {
        stop("Error: Could not check for missing mandatory tags. ", conditionMessage(e))
      })
    },

    # ---------------------------------------------------------------------------- #
    # Module 2: Main Information
    # ---------------------------------------------------------------------------- #
    do_m1_main_information = function() {

      # Step 0: Set the main output directory
      output_dir <- "results/M1_Main_Information"
      figures_dir <- file.path(output_dir, "figures")
      jsons_dir <- file.path(output_dir, "jsons")

      # Step 1: Delete the main output directory if it exists
      if (dir.exists(output_dir)) {
        unlink(output_dir, recursive = TRUE)
        message("[INFO] Deleted existing directory: ", output_dir)
      }

      # Step 2: Recreate the directory structure
      dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
      dir.create(jsons_dir, recursive = TRUE, showWarnings = FALSE)
      message("[INFO] Created new directory structure: ", output_dir)

      # Step 3: Inform user about the processing stage
      message("\nM2 :: Analyzing Main Information...\n")

      # Step 4: Extract the main information
      data <- .self$data
      overview <- fn_m1_main_information(data)

      # Step 6: Store processed overview results in a structured list
      results_overview <- list(
        main_information = overview$main_information,
        author_prod_over_time = overview$author_prod_over_time,
        most_prod_authors = overview$most_prod_authors,
        most_cited_papers = overview$most_cited_papers,
        most_prod_countries = overview$most_prod_countries,
        tc_per_country = overview$tc_per_countries,
        most_rel_source = overview$most_rel_sources,
        most_rel_keywords = overview$most_rel_keywords,
        bradford_law = overview$bradford_law
      )

      # Step 7: Save overview as JSON
      save_json(results_overview,  "m1_main_information.json")




      # ---------------------------------------------------------------------------- #
      # Step 8: Analyze and generate specific visualizations
      # ---------------------------------------------------------------------------- #

      # Save document type pie chart
      fn_m1_mtrc1_articles_types_pie(overview$main_information$document_types)

      # Top Keywords
      fn_m1_mtrc2_most_rel_keywords_wordcloud(overview$most_rel_keywords)
      fn_m1_mtrc2_most_rel_keywords_wordcloud2(overview$most_rel_keywords)

      # Top Authors
      fn_m1_mtrc3_analyze_and_plot_most_prod_authors(overview$most_prod_authors)
      fn_m1_mtrc3_generate_lorenz_curve(overview$most_prod_authors)

      # Most Cited Papers
      fn_m1_mtrc4_analyze_and_plot_most_cited_papers(overview$most_cited_papers)
      fn_m1_mtrc4_analyze_and_plot_citations_per_year(overview$most_cited_papers)
      fn_m1_mtrc4_generate_bubble_chart(overview$most_cited_papers)

      # Most Productive Countries
      fn_m1_mtrc5_analyze_and_plot_most_prod_countries(overview$most_prod_countries, output_dir)
      # Total Citations per Country
      #fn_m1_mtrc5_analyze_and_plot_tc_per_country(overview$tc_per_countries, output_dir)


      # Most Relevant Sources
      #analyze_and_plot_most_rel_sources(overview$most_rel_sources, output_dir)

      #word_counts <- create_wordcloud_from_text(overview$extracted_data, "results/M1_Main_Information/figures")


      # Step 9: Inform completion
      message("[INFO] Main Information analysis completed.\n")
    }
  )
)
