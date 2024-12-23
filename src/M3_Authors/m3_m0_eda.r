# ---------------------------------------------------------------------------- #
# Metric 0: EDA for Authors Data
# ---------------------------------------------------------------------------- #

M3_M0_EDA <- setRefClass(
  "M3_M0_EDA",

  # Fields
  fields = list(
    df = "data.frame",
    author_col = "character",
    year_col = "character",
    results = "list"
  ),

  # Methods
  methods = list(

    # Constructor
    initialize = function(df, author_col = "Authors", year_col = "Year") {
      .self$df <- df
      .self$author_col <- author_col
      .self$year_col <- year_col
      .self$results <- list()
    },

    # Run EDA
    run = function() {
      message(" ==> M3_M0 :: run")
      
      # 1. Number of unique authors
      unique_authors <- .self$get_unique_authors()
      
      # 2. Distribution of publications per author
      publications_per_author <- .self$get_publications_per_author()
      
      # 3. Distribution of publications over time
      publications_over_time <- .self$get_publications_over_time()

      # Save results
      .self$results <- list(
        unique_authors = unique_authors,
        publications_per_author = publications_per_author,
        publications_over_time = publications_over_time
      )
    },

    # Get unique authors
    get_unique_authors = function() {
      authors <- strsplit(.self$df[[.self$author_col]], ";\\s*")  # Split authors
      unique_authors <- length(unique(unlist(authors)))
      return(unique_authors)
    },

    # Get publications per author
    get_publications_per_author = function() {
      authors <- strsplit(.self$df[[.self$author_col]], ";\\s*")  # Split authors
      authors <- unlist(authors)
      author_count <- table(authors)
      author_df <- as.data.frame(author_count, stringsAsFactors = FALSE)
      colnames(author_df) <- c("Author", "Count")
      author_df <- author_df[order(-author_df$Count), ]
      return(author_df)
    },

    # Get publications over time
    get_publications_over_time = function() {
      publications <- .self$df %>%
        dplyr::group_by(.data[[.self$year_col]]) %>%
        dplyr::summarize(Publications = n())
      return(publications)
    },

    # Save results to JSON
    save_json = function(output_path) {
      json_data <- jsonlite::toJSON(.self$results, pretty = TRUE, auto_unbox = TRUE)
      json_path <- file.path(output_path, "m3_m0_eda.json")
      write(json_data, file = json_path)
      message("[INFO] JSON saved successfully: ", json_path)
    },

    # Save plots
    save_plot = function(output_path) {
      # Plot: Publications per Author
      author_plot <- ggplot(.self$results$publications_per_author, aes(x = reorder(Author, -Count), y = Count)) +
        geom_bar(stat = "identity", fill = plot_colors$primary) +
        coord_flip() +
        labs(
          title = "Publications per Author",
          x = "Author",
          y = "Number of Publications"
        ) +
        ieee_theme
      ggsave(file.path(output_path, "Publications_Per_Author.png"), plot = author_plot, width = 8, height = 5)

      # Plot: Publications Over Time
      time_plot <- ggplot(.self$results$publications_over_time, aes(x = .data[[.self$year_col]], y = Publications)) +
        geom_line(color = plot_colors$highlight, size = 1) +
        labs(
          title = "Publications Over Time",
          x = "Year",
          y = "Number of Publications"
        ) +
        ieee_theme
      ggsave(file.path(output_path, "Publications_Over_Time.png"), plot = time_plot, width = 8, height = 5)
    }
  )
)
