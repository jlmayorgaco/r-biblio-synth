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
# ---------------------------------------------------------------------------- #
# List of Required Packages for Comprehensive Analysis
# ---------------------------------------------------------------------------- #

# 📊 Data Manipulation and Wrangling
packages <- c(
  "dplyr",       # Data manipulation (filtering, selecting, summarizing)
  "tibble",      # Enhanced data frames
  "tidyr",       # Data tidying (gather, spread, separate)
  "broom",       # Convert statistical analysis objects into tidy data frames
  "reshape2"     # Data reshaping (melt and cast)
)

# 📈 Data Visualization
packages <- c(packages,
  "ggplot2",     # Core plotting package
  "plotly",      # Interactive plots
  "ggrepel",     # Improved text labels for ggplot2
  "RColorBrewer",# Color palettes for ggplot2
  "patchwork",   # Combine multiple ggplot2 plots
  "ggsci",       # Scientific journal themes for ggplot2
  "treemapify",  # Treemap visualizations
  "ggfortify",   # Plot diagnostic plots for statistical models
  "extrafont",   # Custom fonts for ggplot2
  "ggwordcloud", # Word clouds with ggplot2
  "wordcloud2",  # Interactive word clouds
  "grid",        # Low-level graphics
  "gridSVG",     # Export grid graphics as SVG
  "gridExtra"    # Arrange multiple plots
)

# 🌍 Geographic and Mapping
packages <- c(packages,
  "rworldmap",   # World maps
  "maps",        # Geospatial data
  "countrycode"  # Convert country names to codes
)

# 📦 Web and HTML Export
packages <- c(packages,
  "jsonlite",    # JSON handling
  "htmlwidgets", # Interactive HTML widgets
  "webshot",     # Capture web pages as images
  "png"          # Read and write PNG files
)

# 🧩 Text Mining and Processing
packages <- c(packages,
  "tm",          # Text mining
  "stopwords",   # Stop words for text analysis
  "bibliometrix" # Bibliometric analysis
)

# 📊 Statistical Analysis
packages <- c(packages,
  "car",         # Regression diagnostics
  "MASS",        # Statistical functions and datasets
  "nortest",     # Tests for normality
  "tseries",     # Time series analysis
  "lmtest",      # Hypothesis testing for regression models
  "BayesFactor", # Bayesian analysis
  "psych",        # Descriptive statistics
  "wavelets",
  "fitdistrplus", 
  "ineq", 
  "moments"
)

# 🛠️ Model Fitting and Optimization
packages <- c(packages,
  "nls2",        # Nonlinear least squares
  "minpack.lm"   # Levenberg-Marquardt algorithm for NLS
)

# 🛠️ Metrics and Model Performance
packages <- c(packages,
  "Metrics",     # Error metrics (MAE, MSE, etc.)
  "broom",       # Tidy summaries of models
  "rlang"        # Functional programming tools for tidyverse
)

# 📊 Time Series Analysis
packages <- c(packages,
  "forecast",    # Time series forecasting
  "changepoint", # Change point detection
  "lomb",        # Lomb-Scargle periodograms
  "WaveletComp", # Wavelet analysis
  "signal",      # Signal processing
  "splines"      # Spline regression
)

# ⚙️ Robustness and Resampling
packages <- c(packages,
  "boot",        # Bootstrap methods
  "robustbase"   # Robust statistics
)

# 🔄 Autocorrelation and Spectral Analysis
packages <- c(packages,
  "orcutt",      # Cochrane-Orcutt procedure for autocorrelation
  "spectral",    # Spectral analysis
  "waveslim"     # Wavelet transforms
)

# 🛠️ Influence and Diagnostics
packages <- c(packages,
  "e1071",       # Support vector machines and skewness/kurtosis
  "car",         # Influence diagnostics for regression
  "lmtest"       # Diagnostics for regression models
)

# 📋 Reporting and Tables
packages <- c(packages,
  "kableExtra",  # Enhanced tables in RMarkdown
  "knitr",       # Dynamic report generation
  "pander"       # Format R objects for reports
)

# 📊 Graph and Network Analysis
packages <- c(packages,
  "igraph"       # Network analysis and graph theory
)

# 📋 Miscellaneous Utilities
packages <- c(packages,
  "pander",      # Format tables and figures in RMarkdown
  "rlang",        # Programming with tidyverse
  "purrr",
  "skimr", 
  "GGally",
  "ggbrace"
)



# Install and load necessary packages
install_and_load(packages)



webshot::install_phantomjs()

# ---------------------------------------------------------------------------- #
# -- Loading Modules
# ---------------------------------------------------------------------------- #
# Load inner libraries and functions
source('../../src/Config/PaletteGenerator.r')
source('../../src/Config/PlotThemes.r')
source('../../src/M1_Main_Information/M1_Main_Information.r')
source('../../src/M2_Annual_Production/M2_Annual_Production.r')
source('../../src/M4_Countries/M4_Countries.r')
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
        #convert2df(bib_path, dbsource = "openalex", format = "csv")
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
      #fn_m1_mtrc2_most_rel_keywords_wordcloud(overview$most_rel_keywords)
      #fn_m1_mtrc2_most_rel_keywords_wordcloud2(overview$most_rel_keywords)

      # Top Authors
      fn_m1_mtrc3_analyze_and_plot_most_prod_authors(overview$most_prod_authors)
      fn_m1_mtrc3_generate_lorenz_curve(overview$most_prod_authors)

      # Most Cited Papers
      fn_m1_mtrc4_analyze_and_plot_most_cited_papers(overview$most_cited_papers)
      fn_m1_mtrc4_analyze_and_plot_citations_per_year(overview$most_cited_papers)
      fn_m1_mtrc4_generate_bubble_chart(overview$most_cited_papers)

      fn_m1_mtrc5_analyze_and_plot_most_prod_countries(overview$most_prod_countries )
      fn_m1_mtrc5_analyze_and_plot_tc_per_country(overview$tc_per_countries )

      # Step 9: Inform completion
      message("[INFO] Main Information analysis completed.\n")
    },

    do_m2_annual_production = function(){

      # Step 0: Set the main output directory
      output_dir <- "results/M2_Annual_Production"
      figures_dir <- file.path(output_dir, "figures")
      jsons_dir <- file.path(output_dir, "jsons")

      # Step 1: Delete the existing output directory (if exists)
      if (dir.exists(output_dir)) {
        unlink(output_dir, recursive = TRUE)
        message("[INFO] Deleted existing directory: ", output_dir)
      }

      # Step 2: Recreate directory structure
      dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
      dir.create(jsons_dir, recursive = TRUE, showWarnings = FALSE)
      message("[INFO] Created new directory structure: ", output_dir)

      # Step 3: Inform user about the processing stage
      message("\nM2 :: Analyzing Annual Production...\n")

      # Step 4: Convert bibliometric data to annual production format
      data <- .self$data

      message("\nM2 :: Analyzing Annual Production... p1 \n")
      print(colnames(data))

      # Extract relevant columns (Year & Articles)
      df_annual <- data %>%
        dplyr::select("PY") %>%             # Use dplyr::select with quotes
        dplyr::filter(!is.na(PY)) %>%       # Use dplyr::filter explicitly
        dplyr::group_by(PY) %>%
        dplyr::summarise(Articles = n()) %>%
        dplyr::rename(Year = PY)            # Use dplyr::rename explicitly

            # Step 5: Initialize and Run M2 Analysis
      m2_analysis <- M2_Annual_Production$new(df_annual, year_col = "Year", articles_col = "Articles")
      m2_analysis$runMetrics()

      # Step 6: Log completion
      message("[INFO] Annual Production analysis completed.\n")

    },

    do_m3_authors = function(){},

    do_m4_countries = function(){

      # Step 0: Setup output folders
      output_dir <- "results/M4_Countries"
      figures_dir <- file.path(output_dir, "figures")
      jsons_dir <- file.path(output_dir, "jsons")

      if (dir.exists(output_dir)) {
        unlink(output_dir, recursive = TRUE)
        message("[INFO] Deleted existing directory: ", output_dir)
      }

      dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
      dir.create(jsons_dir, recursive = TRUE, showWarnings = FALSE)
      message("[INFO] Created new directory structure: ", output_dir)

      # Step 1: Get original data
      raw_data <- .self$data
      message("\nM4 :: Analyzing Countries...\n")
      message("\nM4 :: Available columns:")
      print(colnames(raw_data))

      # Step 2: Extract country metadata safely
      df_countries <- extract_country_data(raw_data)

      # Step 3: Check and filter
      if (!"Country" %in% colnames(df_countries)) {
        stop("[ERROR] 'Country' column not found. Extraction failed.")
      }

      df_countries <- df_countries %>%
        dplyr::filter(!is.na(Country) & !is.na(Year)) %>%
        dplyr::select(Country, Year, dplyr::everything())



      message("\nM4 :: After Filter:")
      # Step 4: Initialize and run analysis
      m4_analysis <- M4_Countries$new(df_countries, country_col = "Country", year_col = "Year")
      m4_analysis$runMetrics()

      message("[INFO] Countries analysis completed.\n")
    },

    do_m4_countries2 = function(){

      data <- .self$data

      # Most Productive Countries
      analysis <- BubbleCountryAnalysis$new(data,  N_years = 5,  num_countries = 12,  show_arrows = TRUE, show_scale_arrows = TRUE)
      top_countries_df <- analysis$get_top_countries_dataset()
      top_countries_tp_vs_tc_plot <- analysis$generate_bubble_tp_vs_tc_plot()
      top_countries_scp_vs_mcp_plot <- analysis$generate_bubble_scp_vs_mcp_plot()

      analysis$do_run_by_countries()

      save_plot(
        plot = top_countries_tp_vs_tc_plot,
        filename_prefix = "M4_G1_BUBBLE_COUNTRIES_TP_VS_TC_MEDIAN_QUADRANTS", 
        width = 11,
        height = 6,
        dpi = 1200,
        output_dir = "results/M4_Countries/figures"
      )
      save_plot(
        plot = top_countries_scp_vs_mcp_plot,
        filename_prefix = "M4_G1_BUBBLE_COUNTRIE_SCP_VS_MCP_MEDIAN_QUADRANTS", 
        width = 11,
        height = 6,
        dpi = 1200,
        output_dir = "results/M4_Countries/figures"
      )

      save_json(top_countries_df , "M1_G5_BUBBLE_COUNTRIES_MEDIAN_QUADRANTS.json")
    }
  )
)
