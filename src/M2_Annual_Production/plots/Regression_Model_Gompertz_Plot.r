library(R6)
library(ggplot2)
library(grid)
library(rsvg)

# -------------------------------------------
# CLASS: Regression Model Gompertz Plot
# -------------------------------------------
Regression_Model_Gompertz_Plot <- R6Class("Regression_Model_Gompertz_Plot",
  public = list(
    p = NULL,              # Plot object
    t_real = NULL,         # Time series data
    y_real = NULL,         # Real y-values
    THEME_COLORS = NULL,   # Theme colors
    model_params = NULL,   # Extracted model parameters
    eq_dir = NULL,         # Optional LaTeX directory
    use_latex = FALSE,     # Flag to check if LaTeX should be used

    # -------------------------------------------
    # ✅ Constructor (LaTeX is now optional)
    # -------------------------------------------
    initialize = function() {
      message("[INFO] Gompertz plot initialized without LaTeX. Use `set_latex_directory()` to enable it.")
    },

    # -------------------------------------------
    # ✅ Setters for encapsulated data storage
    # -------------------------------------------
    setP = function(p) {
      self$p <- p
    },

    setData = function(t_real, y_real) {
      self$t_real <- t_real
      self$y_real <- y_real
    },

    setColors = function(colors) {
      self$THEME_COLORS <- colors
    },

    setParams = function(param_string) {
      param_list <- strsplit(param_string, ", ")[[1]]
      param_dict <- list()
      for (param in param_list) {
        parts <- strsplit(param, " = ")[[1]]
        if (length(parts) == 2) {
          key <- trimws(parts[1])
          value <- as.numeric(parts[2])
          param_dict[[key]] <- value
        }
      }
      self$model_params <- param_dict
      self$validate_model_params()
    },

    set_latex_directory = function(eq_dir) {
      self$eq_dir <- normalizePath(eq_dir, mustWork = FALSE)
      self$use_latex <- TRUE
      message("[INFO] LaTeX directory set: ", self$eq_dir)
    },

    # -------------------------------------------
    # ✅ Validate Model Parameters
    # -------------------------------------------
    validate_model_params = function() {
      required_keys <- c("N0", "t0", "Nmax", "k", "y0")
      if (!all(required_keys %in% names(self$model_params))) {
        stop("[ERROR] Missing required parameters: ", 
             paste(setdiff(required_keys, names(self$model_params)), collapse = ", "))
      }
    },

    # -------------------------------------------
    # ✅ Compute Derived Values (tr)
    # -------------------------------------------
    compute_tr = function() {
      N0 <- self$model_params[["y0"]] + self$model_params[["N0"]]
      tr_index <- which(self$y_real >= 0.9 * N0)[1]
      return(if (!is.na(tr_index)) self$t_real[tr_index] else NA)
    },

    # -------------------------------------------
    # ✅ Add Features to Plot (t0, tr, N0)
    # -------------------------------------------
    add_plot_features = function() {
      p <- self$p
      N0 <- self$model_params[["y0"]] + self$model_params[["N0"]]
      t0 <- self$model_params[["t0"]]
      tr <- self$compute_tr()


      if (!is.na(tr)) {
        tr_label_text <- "t[r]"
        tr_label_pos_x <- (tr - 3)
        tr_label_pos_y <- max(self$y_real) - 9
        tr_label_angle <- 0
        tr_label_size <- 10/ .pt
        t0_label_just_v <- 2.0
        t0_label_just_h <- 1.0
        tr_color <- self$THEME_COLORS$Main[3]
        tr_line_size <- 0.5
        tr_line_type <- "dashed"
        p <- p +
          geom_vline(xintercept = tr, size = tr_line_size, linetype = tr_line_type, color = tr_color) +
          geom_text(
            aes(
                x = tr_label_pos_x , 
                y = tr_label_pos_y, 
                label = tr_label_text
            ), parse = TRUE,
            color = tr_color, 
            angle = tr_label_angle, 
            vjust = tr_label_just_v, 
            hjust = tr_label_just_h, 
            size = tr_label_size
        )
      }

        label_N0 <- "N[0]"
        label_t0 <- "t[0]"

        t0_label_text <- "t[0]"
        t0_label_pos_x <- (t0 - 1)
        t0_label_pos_y <- max(self$y_real)
        t0_label_angle <- 0
        t0_label_size <- 10/ .pt
        t0_label_just_v <- 2.0
        t0_label_just_h <- 1.0
        t0_color <- self$THEME_COLORS$Main[1]
        t0_line_size <- 0.5
        t0_line_type <- "dashed"

        N0_label_text <- "N[0]"
        N0_label_pos_x <- min(self$t_real)
        N0_label_pos_y <- N0
        N0_label_angle <- 0
        N0_label_size <- 10/ .pt
        N0_label_just_v <- -0.5
        N0_label_just_h <- 0.0
        N0_color <- self$THEME_COLORS$Main[5]
        N0_line_size <- 0.5
        N0_line_type <- "dashed"
      
      p <- p +
        geom_vline(xintercept = t0, size = t0_line_size, linetype = t0_line_type, color = t0_color) +
        geom_text(aes(x = t0_label_pos_x, y = t0_label_pos_y, label = label_t0) , parse = TRUE,
                  color = t0_color, angle = t0_label_angle, vjust = t0_label_just_v, hjust = t0_label_just_h, size = t0_label_size) +

        geom_hline(yintercept = N0, size = N0_line_size, linetype = N0_line_type, color = N0_color) +
        geom_text(aes(x = N0_label_pos_x, y = N0_label_pos_y, label = N0_label_text),  parse = TRUE,
                  color = N0_color, hjust = N0_label_just_h, vjust = N0_label_just_v, size = N0_label_size)

      self$p <- p  # Store updated plot
    },

    # -------------------------------------------
    # ✅ Process LaTeX Equation (Only if Enabled)
    # -------------------------------------------
    process_latex_equation = function() {
      if (!self$use_latex) return(NULL)

      eq_template <- file.path(self$eq_dir, "m2_regression_model_gompertz.tex")
      eq_rendered <- file.path(self$eq_dir, "m2_regression_model_gompertz_rendered.tex")
      svg_file <- file.path(self$eq_dir, "m2_regression_model_gompertz_rendered.svg")

      if (!file.exists(eq_template)) stop("[ERROR] Template LaTeX file not found: ", eq_template)

      # Read and replace placeholders
      latex_template <- readLines(eq_template)
      format_number <- function(num) {
        if (num < 1) return(sprintf("%.3f", num))
        else if (num < 10) return(sprintf("%.1f", num))
        else return(sprintf("%.0f", num))
      }

      latex_rendered <- gsub("<<N0>>", format_number(self$model_params[["N0"]]), latex_template)
      latex_rendered <- gsub("<<k>>", format_number(self$model_params[["k"]]), latex_rendered)
      latex_rendered <- gsub("<<c>>", format_number(self$model_params[["Nmax"]]), latex_rendered)
      latex_rendered <- gsub("<<t0>>", format_number(self$model_params[["t0"]]), latex_rendered)

      # Save LaTeX file
      writeLines(latex_rendered, eq_rendered)

      return(svg_file)
    },

    # -------------------------------------------
    # ✅ Attach Equation to Plot (Only if LaTeX is Enabled)
    # -------------------------------------------
    add_equation_to_plot = function() {
      if (!self$use_latex) return()

      svg_file <- self$process_latex_equation()
      if (is.null(svg_file)) return()

      svg_rendered <- rsvg::rsvg(svg_file, width = 5000)
      grob_svg <- rasterGrob(svg_rendered, interpolate = TRUE)

      x0 <- min(self$t_real) - 7
      y0 <- max(self$y_real) - 70  
      delta <- 50  

      self$p <- self$p + annotation_custom(grob_svg, xmin = x0, xmax = x0 + delta, ymin = y0, ymax = y0 + delta)
    },

    # -------------------------------------------
    # ✅ Final Plot Rendering
    # -------------------------------------------
    render = function() {
      self$add_plot_features()
      self$add_equation_to_plot()
      return(self$p)
    }
  )
)
