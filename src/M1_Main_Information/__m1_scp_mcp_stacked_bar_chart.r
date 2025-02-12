# ---------------------------------------------------------------------------- #
# Function: Generate Dual-Axis Bar Plot with Mirrored Area Graph
# Description: This function creates a dual-axis bar plot with an overlaid mirrored
#              area graph representing SCP and MCP document counts. The area fills
#              from the top ("roof") down to the MCP_Ratio2 value for each category.
# ---------------------------------------------------------------------------- #

generate_dual_bar_plot_with_mirrored_area <- function(data, 
                                                      title, 
                                                      x_label, 
                                                      y_label, 
                                                      x_var, 
                                                      y_var_scp, 
                                                      y_var_mcp, 
                                                      file_name = NULL) {
  
  # -------------------------------------------------------------------------- #
  # 1. Data Preprocessing
  # -------------------------------------------------------------------------- #
  message("[INFO] Preprocessing data...")

  # Ensure numeric values for y_var_scp and y_var_mcp
  data[[y_var_scp]] <- as.numeric(data[[y_var_scp]])
  data[[y_var_mcp]] <- as.numeric(data[[y_var_mcp]])

  # Calculate total documents and reorder x_var based on total_documents
  data$total_documents <- data[[y_var_scp]] + data[[y_var_mcp]]
  data[[x_var]] <- factor(data[[x_var]], 
                          levels = data[[x_var]][order(data$total_documents, decreasing = FALSE)])

  # Ensure MCP_Ratio exists and is numeric
  if (!"MCP_Ratio" %in% colnames(data)) {
    data$MCP_Ratio <- data[[y_var_mcp]] / data$total_documents
  }
  data$MCP_Ratio <- as.numeric(data$MCP_Ratio)

  # Create a scaled ratio for plotting from roof to bottom
  max_documents <- max(data$total_documents)
  data$MCP_Ratio2 <-  max_documents * data$MCP_Ratio

  # Convert data into long format for stacked bars
  data_long <- data %>%
    pivot_longer(
      cols = c(!!sym(y_var_scp), !!sym(y_var_mcp)), 
      names_to = "Type", 
      values_to = "Documents"
    )

  # Create labels for SCP, MCP, and MCP Ratio
  data$Label_SCP_MCP <- paste0("SCP: ", data[[y_var_scp]], "\nMCP: ", data[[y_var_mcp]])
  data$Label_Ratio <- paste0("Ratio: ", 
                             ifelse(is.na(data$MCP_Ratio), 
                                    "N/A", 
                                    paste0(round(data$MCP_Ratio * 100, 1), "%")))

  # Define IEEE-style colors (adjust as needed)
  ieee_colors <- c("MCP" = THEME_COLORS$Main[1], 
                   "SCP" = THEME_COLORS$Main[2])

  # -------------------------------------------------------------------------- #
  # 2. Prepare Polygon Data (Roof to MCP_Ratio2)
  # -------------------------------------------------------------------------- #
  message("[INFO] Preparing roof-to-bottom polygon data...")

  # For each x_var category, we define two points:
  #  - (x_var, top)        => top = 1.75 * max_documents
  #  - (x_var, MCP_Ratio2) => ratio-based value
  # This will produce vertical "strips" from top down to MCP_Ratio2.
  
  polygon_data <- data %>%
    arrange(.data[[x_var]]) %>%
    mutate(
      y_top   = max_documents * 1.75, 
      y_ratio = 100 + (100 - MCP_Ratio2)
    ) %>%
    # Pivot to get two rows per x_var: one for top, one for ratio
    pivot_longer(
      cols = c(y_top, y_ratio), 
      names_to = "boundary", 
      values_to = "y_value"
    ) %>%
    arrange(.data[[x_var]], boundary)

  # -------------------------------------------------------------------------- #
  # 3. Plot Creation
  # -------------------------------------------------------------------------- #
  message("[INFO] Creating plot...")

  bar_plot <- ggplot(data_long, aes(x = .data[[x_var]], y = Documents, fill = Type)) +

    # Roof-to-bottom polygons (vertical strips) 
    geom_polygon(
      data = polygon_data,
      aes(
        x = .data[[x_var]], 
        y = y_value,
        group = .data[[x_var]]  # Each x_var forms a vertical polygon
      ),
      fill = "#6a3d9a", 
      alpha = 0.5, 
      color = "blue"
    ) +

    # Stacked bar plot for MCP and SCP
    geom_bar(stat = "identity", color = "black", linewidth = 0.3) +

    # Add labels for SCP and MCP counts
    geom_text(
      data = data, 
      aes(
        x = .data[[x_var]], 
        y = total_documents + max_documents * 0.02, 
        label = Label_SCP_MCP
      ),
      inherit.aes = FALSE, 
      hjust = 0, 
      size = 4
    ) +

    # Add labels for MCP Ratio
    geom_text(
      data = data, 
      aes(
        x = .data[[x_var]], 
        y = total_documents + max_documents * 0.15, 
        label = Label_Ratio
      ),
      inherit.aes = FALSE, 
      hjust = 0, 
      size = 4
    ) +

    # Custom fill colors
    scale_fill_manual(values = ieee_colors, labels = c("MCP", "SCP")) +

    # Y-axis with secondary axis for MCP % Ratio
    scale_y_continuous(
      name = "No. of Documents",
      limits = c(0, max_documents * 1.75),
      expand = c(0, 0),
      sec.axis = sec_axis(~ . * 100 / max_documents, name = "MCP % Ratio (%)")
    ) +

    # Plot labels and title
    labs(
      title = title, 
      x = x_label, 
      y = y_label
    ) +

    # ------------------------------------------------------------------------ #
    # 4. Theme Customization
    # ------------------------------------------------------------------------ #
    coord_flip() +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 20, b = 20)),
      axis.title.x = element_text(size = 14, margin = margin(t = 10, b = 20)),
      axis.title.y = element_text(size = 14, margin = margin(r = 10)),
      axis.text = element_text(size = 12),
      
      axis.title.x.top = element_text(color = "blue", size = 14, face = "bold", margin = margin(b = 0)),
      axis.text.x.top = element_text(color = "blue", size = 12),
      axis.ticks.x.top = element_line(color = "blue", size = 1),
      
      axis.line.x.top = element_line(color = "blue", size = 1),
      axis.line.x.bottom = element_line(color = "transparent", size = 0),
      axis.line.y.left = element_line(color = "transparent", size = 0),
      axis.line.y.right = element_line(color = "blue", size = 1),
      
      panel.border = element_blank(),
      legend.title = element_blank(),
      legend.text = element_text(size = 12),
      panel.grid.major.x = element_line(color = "gray", size = 0.1),
      panel.grid.minor.x = element_line(color = "lightgray", size = 0.1),
      panel.grid.major.y = element_line(color = "gray", size = 0.1),
      panel.grid.minor.y = element_line(color = "lightgray", size = 0.1),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = THEME_COLORS$Grayscale$White, color = NA)
    )

  # -------------------------------------------------------------------------- #
  # 5. Save Plot (Optional)
  # -------------------------------------------------------------------------- #
  if (!is.null(file_name)) {
    message("[DEBUG] Saving plot to: ", file_name)
    save_plot(bar_plot, file_name, width = 14, height = 8, dpi = 1000)
  }

  message("[INFO] Plot creation completed.")
  return(bar_plot)
}
