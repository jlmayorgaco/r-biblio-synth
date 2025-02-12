# ---------------------------------------------------------------------------- #
# Function: Validate and Prepare Data
# ---------------------------------------------------------------------------- #
validate_and_prepare_data <- function(data) {
  colnames(data) <- make.unique(colnames(data))
  
  required_cols <- c("TC", "TCperYear", "NTC", "PaperID")
  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop("[ERROR] Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  numeric_cols <- c("TC", "TCperYear", "NTC")
  for (col in numeric_cols) {
    data[[col]] <- suppressWarnings(as.numeric(data[[col]]))
  }
  
  data <- data[complete.cases(data[numeric_cols]), ]
  if (nrow(data) == 0) {
    stop("[ERROR] No valid rows in the dataset after cleaning values.")
  }
  
  return(data)
}

# ---------------------------------------------------------------------------- #
# Function: Create Region Data for Quadrants (Fixed)
# ---------------------------------------------------------------------------- #
create_region_data <- function(data) {
  median_TC <- median(data$TC, na.rm = TRUE)
  median_TCperYear <- median(data$TCperYear, na.rm = TRUE)
  
  region_data <- data.frame(
    xmin = c(median_TCperYear, 0, median_TCperYear, 0),
    xmax = c(max(data$TCperYear, na.rm = TRUE), median_TCperYear, max(data$TCperYear, na.rm = TRUE), median_TCperYear),
    ymin = c(median_TC, median_TC, 0, 0),
    ymax = c(max(data$TC, na.rm = TRUE), max(data$TC, na.rm = TRUE), median_TC, median_TC),
    region = factor(c("I: High TC, High TCperYear", "II: High TC, Low TCperYear", 
                      "III: Low TC, High TCperYear", "IV: Low TC, Low TCperYear"))
  )

  # Ensure there are no NA values
  region_data$xmin[is.na(region_data$xmin)] <- 0
  region_data$ymin[is.na(region_data$ymin)] <- 0
  region_data$xmax[is.na(region_data$xmax)] <- max(data$TCperYear, na.rm = TRUE)
  region_data$ymax[is.na(region_data$ymax)] <- max(data$TC, na.rm = TRUE)

  # Define colors for each region
  region_data$color <- c(THEME_COLORS$Main[5], THEME_COLORS$Main[4], THEME_COLORS$Main[2], THEME_COLORS$Main[3])
  
  return(region_data)
}
# ---------------------------------------------------------------------------- #
# Function: Create Layers for Median-Based Quadrants and Save JSON
# ---------------------------------------------------------------------------- #
create_median_quadrant_layers <- function(data, output_json = "region_points.json", text_size = 5, alpha = 0.2) {
  
  message("[INFO] Generating bubble chart for Median-Based Quadrants...")

  # Calculate means
  mean_TC <- mean(data$TC)
  mean_TCperYear <- mean(data$TCperYear)
  

  process_and_save_region_data(data)



  # Create a data frame for the region layers
  delta <- 1.1
  dx <- round(max(data$TCperYear) / 20)
  dy <- round(max(data$TC) / 20)

  region_data <- data.frame(
    xmin = c(mean_TCperYear, 0, mean_TCperYear, 0),
    xmax = c(delta * max(data$TCperYear), mean_TCperYear, delta * max(data$TCperYear), mean_TCperYear),
    ymin = c(mean_TC, mean_TC, 0, 0),
    ymax = c(delta * max(data$TC), delta * max(data$TC), mean_TC, mean_TC),
    region = c("I: High TC, High TCperYear", "II: High TC, Low TCperYear", 
               "III: Low TC, High TCperYear", "IV: Low TC, Low TCperYear"),
    color = c(THEME_COLORS$Main[5], THEME_COLORS$Main[4], THEME_COLORS$Main[2], THEME_COLORS$Main[3])
  )

  # Create geom layers
  geom_median <- list(
    geom_rect(data = region_data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = region), 
              alpha = alpha, inherit.aes = FALSE),
    geom_hline(yintercept = mean_TC, linetype = "dashed", color = IEEE_MAYORGA_THEME_COLORS$Main[3]),
    geom_vline(xintercept = mean_TCperYear, linetype = "dashed", color = IEEE_MAYORGA_THEME_COLORS$Main[3]),
    geom_text(aes(x = (max(data$TCperYear) + dx), y = (max(data$TC) + dy), label = "I"), size = text_size, fontface = "bold", color = "blue", hjust = 0, vjust = 1),
    geom_text(aes(x = dx, y = (max(data$TC) + dy), label = "II"), size = text_size, fontface = "bold", color = "blue", hjust = 0, vjust = 1),
    geom_text(aes(x = (max(data$TCperYear) + dx), y = dy, label = "III"), size = text_size, fontface = "bold", color = "blue", hjust = 0, vjust = 0),
    geom_text(aes(x = dx, y = dy, label = "IV"), size = text_size, fontface = "bold", color = "blue", hjust = 0, vjust = 0)
  )

  return(geom_median)
}

# ---------------------------------------------------------------------------- #
# Function: Generate and Save Plot (Fixed)
# ---------------------------------------------------------------------------- #
generate_and_save_plot <- function(df, region_data, title, custom_geom = NULL)  {
  
  # Debugging
  message("[DEBUG] Inside generate_and_save_plot function...")
  message("[DEBUG] Title: ", title)
  
  dpi <- 600
  delta_dydx <- 0.1
  delta <- (1 + delta_dydx)

  bubble_chart <- ggplot(df, aes(x = TCperYear, y = TC, size = NTC, label = PaperID))
  
  if (!is.null(custom_geom)) {
    message("[DEBUG] Adding custom layers...")
    bubble_chart <- bubble_chart + custom_geom + scale_fill_manual(
            values = setNames(region_data$color, region_data$region),
            name = "Quadrants"
          )
  }

  bubble_chart <- bubble_chart +
    geom_point(alpha = 0.7, color = IEEE_MAYORGA_THEME_COLORS$Main[1]) +
    geom_text_repel(aes(label = PaperID), size = 3, fontface = "bold") +
    labs(
      title = title,
      x = "Citations per Year",
      y = "Total Citations",
      size = "Normalized Citations"
    ) +
    scale_x_continuous(limits = c(0, delta*max(df$TCperYear, na.rm = TRUE)), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, delta*max(df$TC, na.rm = TRUE)), expand = c(0, 0)) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 12),
      legend.position = "right",
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(color = "gray70"),
      panel.grid.minor = element_line(color = "gray90")
    )

  return(bubble_chart)
}








# ---------------------------------------------------------------------------- #
# Function: Perform K-Means Clustering
# ---------------------------------------------------------------------------- #
perform_kmeans_clustering <- function(data, k = 4) {
  set.seed(123)  # For reproducibility
  kmeans_result <- kmeans(data[, c("TC", "TCperYear")], centers = k)
  data$Cluster <- as.factor(kmeans_result$cluster)
  return(data)
}

# ---------------------------------------------------------------------------- #
# Function: Create Cluster Areas for Plotting (Fix)
# ---------------------------------------------------------------------------- #
create_cluster_areas <- function(data) {
  cluster_hulls <- do.call(rbind, lapply(split(data, data$Cluster), function(cluster_data) {
    cluster_data[chull(cluster_data$TCperYear, cluster_data$TC), ]
  }))
  # Ensure the result is a valid data frame
  cluster_hulls <- as.data.frame(cluster_hulls)
  return(cluster_hulls)
}

# ---------------------------------------------------------------------------- #
# Function: Create Layers for K-Means Clustering (Fixed)
# ---------------------------------------------------------------------------- #
create_kmeans_layers <- function(data, alpha = 0.2) {
  cluster_hulls <- create_cluster_areas(data)
  
  list(
    # Draw convex hulls for each cluster
    geom_polygon(data = cluster_hulls, aes(x = TCperYear, y = TC, group = Cluster, fill = Cluster), alpha = alpha, inherit.aes = FALSE),
    
    # Add cluster labels at the centroids
    geom_text(data = cluster_hulls, aes(x = median(TCperYear), y = median(TC), label = Cluster), fontface = "bold", color = "black")
  )
}


# ---------------------------------------------------------------------------- #
# Function: Generate and Save Plot with Clustering
# ---------------------------------------------------------------------------- #
generate_and_save_kmeans_plot <- function(data, title) {
  message("[INFO] Generating bubble chart for K-Means Clustering...")

  clustered_data <- perform_kmeans_clustering(data, 2)
  kmeans_layers <- create_kmeans_layers(clustered_data)

  bubble_chart <- ggplot(clustered_data, aes(x = TCperYear, y = TC, size = NTC, label = PaperID)) +
    kmeans_layers +
    geom_point(aes(color = Cluster), alpha = 0.7) +
    geom_text_repel(aes(label = PaperID), size = 3, fontface = "bold") +
    labs(
      title = title,
      x = "Citations per Year",
      y = "Total Citations",
      size = "Normalized Citations",
      color = "Cluster"
    ) +
    scale_x_continuous(limits = c(0, max(data$TCperYear, na.rm = TRUE) * 1.1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, max(data$TC, na.rm = TRUE) * 1.1), expand = c(0, 0)) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 12),
      legend.position = "right",
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(color = "gray70"),
      panel.grid.minor = element_line(color = "gray90")
    )
  
  return(bubble_chart)
}



process_and_save_region_data <- function(data, output_file = "m1_bubble_papers_median_region_by_tc_tcperyear.json") {
  
  # Step 1: Classify and Group Points by Region
  message("[INFO] Classifying and grouping points by region...")
  mean_TC <- mean(data$TC, na.rm = TRUE)
  mean_TCperYear <- mean(data$TCperYear, na.rm = TRUE)
  
  data$region <- dplyr::case_when(
    data$TC >= mean_TC & data$TCperYear >= mean_TCperYear ~ "I: High TC, High TCperYear",
    data$TC >= mean_TC & data$TCperYear < mean_TCperYear  ~ "II: High TC, Low TCperYear",
    data$TC < mean_TC & data$TCperYear >= mean_TCperYear  ~ "III: Low TC, High TCperYear",
    TRUE                                                  ~ "IV: Low TC, Low TCperYear"
  )
  
  # Group points by region
  region_points <- split(data, data$region)
  message("[INFO] Points have been classified into regions.")

  # Debugging Step: Check how many regions have data
  for (region_name in names(region_points)) {
    message("[DEBUG] Region: ", region_name, " | Number of points: ", nrow(region_points[[region_name]]))
  }

  # Step 2: Add Descriptions
  message("[INFO] Adding descriptions to each region...")
region_descriptions <- list(
  "I: High TC, High TCperYear" = list(
    category = "New Classics",
    sentence = "Recent studies have incorporated the category 'classic' to describe rapidly rising papers that generate novel and successful research.",
    citation = "Bornmann & Marx (2014), 'How Good is Research Really?'",
    explanation = "This region captures emerging classics—recent publications that are quickly gaining traction and establishing themselves as key references."
  ),
  "II: High TC, Low TCperYear" = list(
    category = "Classics",
    sentence = "Classics are often described as seminal papers that form the foundation of a field and have sustained influence over time.",
    citation = "Small (1973), 'Co-Citation in the Scientific Literature'",
    explanation = "This region represents established classics that remain highly cited but whose citation growth has slowed as their foundational contributions become widely accepted."
  ),
  "III: Low TC, High TCperYear" = list(
    category = "Rising Stars",
    sentence = "Citation patterns show a fast accumulation of citations in early years, indicative of emerging influential works.",
    citation = "Ke et al. (2015), 'Defining and Identifying Sleeping Beauties in Science'",
    explanation = "These papers recently started gaining recognition after a period of relative obscurity, aligning with the concept of 'Sleeping Beauties.'"
  ),
  "IV: Low TC, Low TCperYear" = list(
    category = "Forgotten Papers",
    sentence = "Certain papers receive fewer citations due to their niche focus or conceptual absorption into the field's common knowledge.",
    citation = "Merton (1965), 'On the Shoulders of Giants'",
    explanation = "These papers were influential at some point but have faded from prominence over time, often due to shifts in research focus."
  )
)

      
    # Step 3: Add descriptions to each region
    for (region_name in names(region_points)) {
      if (region_name %in% names(region_descriptions)) {
        num_rows <- nrow(region_points[[region_name]])
        if (num_rows > 0) {
          region_points[[region_name]]$description <- rep(
            list(region_descriptions[[region_name]]), 
            num_rows
          )
        }
      }
    }


  # Step 4: Save the final JSON
  save_json(region_points, output_file)
  message("[INFO] Region data with descriptions saved to: ", output_file)
}
