generate_palette_png <- function(theme_colors, output_file) {
  tryCatch({
    library(ggplot2)
    library(dplyr)
    library(gridExtra)

    # Flatten the color palette for plotting
    flatten_colors <- function(colors) {
      if (is.list(colors)) {
        unlist(colors, recursive = TRUE, use.names = TRUE)
      } else {
        colors
      }
    }

    # Function to calculate luminance
    calculate_luminance <- function(hex_color) {
      rgb <- col2rgb(hex_color) / 255
      c_linear <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055) ^ 2.4)
      luminance <- 0.2126 * c_linear[1] + 0.7152 * c_linear[2] + 0.0722 * c_linear[3]
      luminance
    }

    # Determine text color based on luminance
    get_text_color <- function(hex_color, threshold = 0.5) {
      if (calculate_luminance(hex_color) > threshold) {
        "black"  # Use black text for light colors
      } else {
        "white"  # Use white text for dark colors
      }
    }

    # Flattened color list
    flat_colors <- flatten_colors(theme_colors)

    # Ensure no duplicate names
    color_names <- make.unique(names(flat_colors))

    # Add line breaks for grayscale labels if necessary
    format_label <- function(label, category) {
      if (is.na(label) || is.na(category)) {
        return("NA")
      }
      if (category == "Grayscale" && grepl("\\.", label)) {
        parts <- strsplit(label, "\\.")[[1]]
        paste(parts, collapse = "\n")
      } else {
        label
      }
    }

    # Create a data frame for visualization
    color_df <- data.frame(
      Name = factor(color_names, levels = color_names),  # Preserve order
      Color = flat_colors
    )

    # Group colors based on their category
    categories <- names(theme_colors)
    color_df$Category <- rep(categories, sapply(theme_colors, function(x) length(flatten_colors(x))))

    # Reorder categories for visualization
    category_order <- c("Main", "Categorical", "Diverging", "Sequential", "Text", "Grayscale")
    color_df$Category <- factor(color_df$Category, levels = category_order)

    # Adjust the data frame for equal-width tiles
    color_df <- color_df %>%
      group_by(Category) %>%
      mutate(
        Position = row_number(),
        Width = 1,
        Center = Position - 0.5,
        Label = mapply(format_label, as.character(Name), as.character(Category)),
        TextColor = sapply(Color, get_text_color)  # Determine text color based on luminance
      )

    # Create a plot for each category
    plots <- lapply(split(color_df, color_df$Category), function(df) {
      ggplot(df, aes(x = Center, y = 1, fill = Color, width = Width)) +
        geom_tile(height = 0.9) +
        geom_text(
          aes(label = Label, color = TextColor), 
          size = 4, 
          fontface = "bold", 
          lineheight = 0.8
        ) +
        scale_fill_identity() +
        scale_color_identity() +  # Use the computed TextColor for text
        theme_minimal() +
        theme(
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
        ) +
        labs(title = unique(df$Category))
    })

    # Combine all category plots into one grid
    palette_plot <- gridExtra::marrangeGrob(grobs = plots, ncol = 1, nrow = length(plots))

    # Save the palette visualization to a PNG file
    ggsave(output_file, palette_plot, width = 10, height = length(plots) * 1.5, dpi = 600)
    message("[INFO] Palette visualization saved successfully: ", output_file)
  }, error = function(e) {
    message("[ERROR] Failed to generate palette visualization: ", e$message)
    stop("[ERROR] Palette visualization generation failed.")
  })
}
