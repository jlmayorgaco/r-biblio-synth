library(R6)
library(ggplot2)
library(ggfortify)
library(jsonlite)
library(gridExtra)
library(rsvg)

# -------------------------------------------
# BASE CLASS: IEEE Plot Standard
# -------------------------------------------
R_IEEE_Plot <- R6Class("R_IEEE_Plot",
  public = list(
    title = NULL,
    x_label = NULL,
    y_label = NULL,
    plot = NULL,
    metrics = NULL,

    # Constructor
    initialize = function(title = "", x_label = "", y_label = "") {
      self$title <- title
      self$x_label <- x_label
      self$y_label <- y_label
    },

    # Common theme for all plots
    getTheme = function() {
        theme_minimal(base_size = 12) +  # Increased base size for IEEE standards
        theme(
            panel.background = element_rect(fill = "white", color = "black"),  # Added black border
            plot.title = element_text(hjust = 0.5, size = 12, face = "bold", family = "serif"),  # Use "serif" instead
            axis.title = element_text(size = 11, family = "serif"),
            axis.text = element_text(size = 10, family = "serif"),
            axis.ticks = element_line(linewidth = 0.3),
            axis.line = element_line(color = "black", linewidth = 0.5),
            panel.grid.major = element_line(linewidth = 0.3, color = "#dddddd"),
            panel.grid.minor = element_line(linewidth = 0.2, color = "#eeeeee"),
            panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
            legend.position = "bottom",
            legend.text = element_text(size = 10, family = "serif"),
            legend.title = element_text(size = 10, face = "bold", family = "serif"),
            plot.margin = margin(5, 5, 5, 5)
        )        
    },


    # Method to set axis labels dynamically
    setLabels = function(x_label, y_label) {
      self$x_label <- x_label
      self$y_label <- y_label
    },

    # Generate and return one-column IEEE-sized plot with dimensions
    getOneColumnPlot = function() {
        list(
            plot = self$plot + theme(plot.margin = margin(5, 5, 5, 5)) +
            labs(title = self$title, x = self$x_label, y = self$y_label),
            width = 3.5,   # IEEE single-column width
            height = 2.625, # IEEE aspect ratio 4:3
            dpi = 900      # High resolution for publication
        )
    },

    # Generate and return double-column IEEE-sized plot with dimensions
    getDoubleColumnPlot = function() {
        list(
            plot = self$plot + theme(plot.margin = margin(10, 10, 10, 10)) +
            labs(title = self$title, x = self$x_label, y = self$y_label),
            width = 7.2,   # IEEE double-column width
            height = 3.24, # Maintains 4:3 aspect ratio
            dpi = 900      # High resolution for publication
        )
    },

    # Generate and return a JSON report containing metrics
    getReport = function() {
    # Convert self$metrics into a JSON-friendly format
    metrics_fixed <- lapply(self$metrics, function(x) {
        if (inherits(x, "table")) {
        return(as.list(setNames(as.vector(x), names(x))))  # Convert table to a named list
        } else if (is.matrix(x)) {
        return(as.data.frame(x))  # Convert matrix to data frame
        } else if (is.vector(x) || is.numeric(x) || is.character(x)) {
        return(x)  # Keep simple types unchanged
        } else {
        return(deparse(substitute(x)))  # Convert unknown objects to text descriptions
        }
    })

    return(toJSON(metrics_fixed, pretty = TRUE, auto_unbox = TRUE))
    }


  )
)
