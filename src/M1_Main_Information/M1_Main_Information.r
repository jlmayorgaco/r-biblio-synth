# ---------------------------------------------------------------------------- #
# -- M2 Main Information  ---------------------------------------------------- #
# ---------------------------------------------------------------------------- #
library(ggwordcloud)

# Main function to extract and clean main information from bibliometric data
fn_m1_main_information <- function(bib_data) {
  res1 <- biblioAnalysis(bib_data, sep = ";")
  s1 <- summary(res1, pause = FALSE, verbose = FALSE)

  # Extract summary information
  summary_df <- s1$MainInformationDF

  # Extract and clean additional summary information
  most_prod_authors <- clean_whitespace(s1$MostProdAuthors)
  annual_production <- clean_whitespace(s1$AnnualProduction)
  most_cited_papers <- clean_whitespace(s1$MostCitedPapers)
  most_prod_countries <- clean_whitespace(s1$MostProdCountries)
  tc_per_countries <- clean_whitespace(s1$TCperCountries)
  most_rel_sources <- clean_whitespace(s1$MostRelSources)
  most_rel_keywords <- clean_whitespace(s1$MostRelKeywords)

  bradford_law <- bradford(bib_data)
  bradford_law_df <- bradford_law$table

  main_information_data <- list(
    main_information = list(
      timespan = as.character(summary_df[summary_df$Description == "Timespan", "Results"]),
      sources = as.integer(summary_df[summary_df$Description == "Sources (Journals, Books, etc)", "Results"]),
      documents = as.integer(summary_df[summary_df$Description == "Documents", "Results"]),
      annual_growth_rate = as.numeric(summary_df[summary_df$Description == "Annual Growth Rate %", "Results"]),
      document_average_age = as.numeric(summary_df[summary_df$Description == "Document Average Age", "Results"]),
      avg_citations_per_doc = as.numeric(summary_df[summary_df$Description == "Average citations per doc", "Results"]),
      avg_citations_per_year_per_doc = as.numeric(summary_df[summary_df$Description == "Average citations per year per doc", "Results"]),
      references = as.integer(summary_df[summary_df$Description == "References", "Results"]),
      document_types = list(
        article = as.integer(summary_df[summary_df$Description == "article", "Results"]),
        article_article = as.integer(summary_df[summary_df$Description == "article article", "Results"]),
        article_conference_paper = as.integer(summary_df[summary_df$Description == "article conference paper", "Results"]),
        article_review = as.integer(summary_df[summary_df$Description == "article review", "Results"]),
        conference_paper = as.integer(summary_df[summary_df$Description == "conference paper", "Results"]),
        review = as.integer(summary_df[summary_df$Description == "review", "Results"])
      ),
      keywords_plus = as.integer(summary_df[summary_df$Description == "Author Keywords (DE)", "Results"]),
      author_keywords = as.integer(summary_df[summary_df$Description == "Keywords-Plus (ID)", "Results"]),
      authors = as.integer(summary_df[summary_df$Description == "Authors", "Results"]),
      authors_per_doc = as.numeric(summary_df[summary_df$Description == "Authors per Document", "Results"]),
      co_authors_per_doc = as.numeric(summary_df[summary_df$Description == "Co-Authors per Documents", "Results"]),
      international_collaborations = as.numeric(summary_df[summary_df$Description == "International co-authorships %", "Results"]),
      single_author_documents = as.integer(summary_df[summary_df$Description == "Single-authored documents", "Results"]),
      multi_author_documents = as.integer(summary_df[summary_df$Description == "Multi-authored documents", "Results"])
    ),
    most_cited_papers = most_cited_papers,
    most_prod_countries = most_prod_countries,
    tc_per_countries = tc_per_countries,
    most_rel_sources = most_rel_sources,
    most_rel_keywords = most_rel_keywords,
    most_prod_authors = most_prod_authors,
    author_prod_over_time = annual_production,
    bradford_law = bradford_law_df
  )

  v_document_types <- main_information_data$main_information$document_types;
  fn_save_m1_articles_pie(v_document_types);

  return(main_information_data)
}

fn_save_m1_articles_pie <- function(v_document_types){

  # Prepare the data frame for ggplot
  v_names <- names(v_document_types)
  v_counts <- sapply(v_document_types, function(x) ifelse(length(x) == 0, 0, x))

  # Calculate the percentage for each document type
  v_percentages <- (v_counts / sum(v_counts)) * 100

  df <- data.frame(
    Document_Type = v_names,
    Count = v_percentages
  )
  
  # Map the Document_Type column to the new labels
  df$Document_Type <- LABEL_MAPPING[df$Document_Type]

  # Filter out zero counts
  df <- df[df$Count > 0, ]

  # Create a pie chart using ggplot2
  p <- ggplot(df, aes(x = "", y = Count, fill = Document_Type))
  p <- p + geom_bar(width = 1, stat = "identity")
  p <- p + coord_polar("y", start = 0)
  p <- p + labs(title = "Document Types Distribution")
  #p <- p + ieee_theme
  p <- p + theme_void()
  p <- p + theme(legend.title = element_blank())

    # Save the plot as PNG and SVG
  output_file_png <- file.path("results/M1_Main_Information", "/figures/M1_Document_Types_Pie_Plot_PNG.png")
  output_file_svg <- file.path("results/M1_Main_Information", "/figures/M1_Document_Types_Pie_Plot_SVG.svg")
  
  v_k_scaling <- 0.5;
  v_k_width_hight <- 1.5; 
  v_width <- 8.8 * v_k_scaling;
  v_hight <- v_width / v_k_width_hight;

  # Ensure the output directory exists
  if (dir.exists("results/M1_Main_Information/figures")) {
    unlink("results/M1_Main_Information/figures", recursive = TRUE)  # Delete the directory and its contents
  }
  # Create a new, clean output directory
  dir.create("results/M1_Main_Information/figures", recursive = TRUE)

  ggsave(filename = output_file_png, plot = p, width = v_width, height = v_hight, dpi = 900)
  ggsave(filename = output_file_svg, plot = p, width = v_width, height = v_hight, device = "svg")

}
# Function to clean whitespace from a data frame
clean_whitespace <- function(df) {
  # Trim whitespace from column names
  colnames(df) <- trimws(colnames(df))
  
  # Trim whitespace from each element of the data frame
  df[] <- lapply(df, function(x) {
    if (is.character(x)) {
      # Remove leading and trailing spaces and reduce multiple spaces to a single space
      gsub("\\s+", " ", trimws(x))
    } else {
      x
    }
  })
  return(df)
}



fn_most_rel_keywords <-function (most_rel_keywords){

    names(most_rel_keywords)[2] <- "Article-Author-Keywords"
    names(most_rel_keywords)[4] <- "Article-Keywords-Plus"

    # Convert the counts to numeric, if they aren't already
    most_rel_keywords$`Article-Keywords-Plus` <- as.numeric(most_rel_keywords$`Article-Keywords-Plus`)
    most_rel_keywords$`Article-Author-Keywords` <- as.numeric(most_rel_keywords$`Article-Author-Keywords`)

    print(' ')
    print(' ')
    print(' ')
    print(' =====> most_rel_keywords')
    print(most_rel_keywords)
    print(' ')
    print(' ')
    print(' ')

    p <- ggplot(most_rel_keywords, aes(label = `Author Keywords (DE)` , size = `Article-Author-Keywords`)) 
    p <- p + geom_text_wordcloud()
    p <- p + theme_minimal()
    p <- p + labs(
        title = "Most Relevant Author Keywords",
         x = "Author Keywords (DE)",
         y = "Number of Articles"
    )
    p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))

    v_k_scaling <- 0.5;
    v_k_width_hight <- 1.5; 
    v_width <- 8.8 * v_k_scaling;
    v_hight <- v_width / v_k_width_hight;

    output_file_png <- file.path("results/M1_Main_Information", "/figures/M1_Most_Rel_Keywords_Bar_Plot_PNG.png")
    output_file_svg <- file.path("results/M1_Main_Information", "/figures/M1_Most_Rel_Keywords_Bar_Plot_SVG.svg")
    
    ggsave(filename = output_file_png, plot = p, width = v_width, height = v_hight, dpi = 900)
    ggsave(filename = output_file_svg, plot = p, width = v_width, height = v_hight, device = "svg")


    # Save the most_rel_keywords data frame as a CSV file
    write.csv(most_rel_keywords, file = file.path("results/M1_Main_Information", "/jsons/M1_Most_Rel_Keywords_Dataset.csv"), row.names = FALSE)

  }