# ---------------------------------------------------------------------------- #
# Metric 3: Most Productive Authors
# ---------------------------------------------------------------------------- #

# Load necessary files
message("Loaded file: _helpers.r")
source('../../src/M3_Most_Prod_Authors/_helpers.r')

message("Loaded file: _utils.r")
source('../../src/M3_Most_Prod_Authors/_utils.r')

message("Loaded file: _settings.r")
source('../../src/M3_Most_Prod_Authors/_settings.r')

M3_M0_EDA <- setRefClass(
  "M3_M0_EDA",

  # Fields
  fields = list(
    df = "data.frame",
    author_col = "character",
    articles_col = "character",   
    fractionalized_col = "character",
    results = "list"
  ),


  # Methods
  methods = list(

    # Constructor
    initialize = function(df, author_col = "Authors", articles_col = "Articles", fractionalized_col = "Articles Fractionalized") {
      .self$df <- df
      .self$author_col <- author_col
      .self$articles_col <- articles_col
      .self$fractionalized_col <- fractionalized_col
      .self$results <- list()
    },

    # Run analysis
    run = function() {
       message(" ==> M3_Most_Prod_Authors :: run")

       print(.self$df)

        # Extract author productivity
        analysis <- bibliometrix::biblioAnalysis(.self$df)
      
        message("[INFO] Most productive authors analysis completed.")
    },

    # Save results as JSON
    save_json = function(output_path) {
      message(" ==> M3_Most_Prod_Authors :: save_json")

      json_data <- jsonlite::toJSON(.self$results, pretty = TRUE, auto_unbox = TRUE)
      output_file <- file.path(output_path, "most_prod_authors.json")

      write(json_data, file = output_file)
      message("[INFO] JSON saved successfully at: ", output_file)
    },

    # Save plot
    save_plot = function(output_path) {
      message(" ==> M3_Most_Prod_Authors :: save_plot")

      # Prepare data for plotting
      most_prod_authors <- .self$results$most_productive_authors
      top_authors <- head(most_prod_authors, 10) # Top 10 authors

      # Create bar plot
      p <- ggplot(top_authors, aes(x = reorder(Authors, Count), y = Count)) +
        geom_bar(stat = "identity", fill = plot_colors$primary) +
        coord_flip() +
        labs(
          title = "Top 10 Most Productive Authors",
          x = "Authors",
          y = "Number of Publications"
        ) +
        ieee_theme

      # Save plot
      output_file_png <- file.path(output_path, "most_prod_authors.png")
      output_file_svg <- file.path(output_path, "most_prod_authors.svg")

      ggsave(filename = output_file_png, plot = p, width = export_settings$plot_width, height = export_settings$plot_height, dpi = export_settings$dpi)
      ggsave(filename = output_file_svg, plot = p, width = export_settings$plot_width, height = export_settings$plot_height, device = "svg")

      message("[INFO] Plot saved successfully.")
    }
  )
)
