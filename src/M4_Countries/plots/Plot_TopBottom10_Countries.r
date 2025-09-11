# -------------------------------------------
# PLOT CLASS: Top/Bottom Country Plots (Year-aware templating, IEEE-ready)
# -------------------------------------------
Plot_TopBottom10_Countries <- R6::R6Class("Plot_TopBottom10_Countries",

  inherit = R_IEEE_Plot,

  public = list(
    data           = NULL,
    gini           = NULL,
    metric         = NULL,
    year_range     = NULL,
    type           = NULL, # "top" or "bottom"
    country_col    = "Country",
    value_col      = NULL,
    plot           = NULL,

    # Optional: values to compute Lorenz/Gini on (full distribution recommended)
    lorenz_values  = NULL,

    # Centralized config
    config = list(
      fonts = list(base = "Times New Roman"),
      colors = list(
        bar_top         = "#377eb8",
        bar_bottom      = "#b2182b",
        bar_other       = "gray50",
        lorenz_diag     = "gray50",
        lorenz_line     = "black",
        bg              = "white",
        inset_card_fill = "transparent"   # "white" gives a card; "transparent" blends
      ),
      sizes = list(
        bar_width     = 0.72,
        value_text    = 3.5,
        lorenz_text   = 2.8,
        lorenz_line   = 0.5,
        diag_line     = 0.4,
        border_line   = 0.35,  # thinner inset border
        grid_major_x  = 0.25   # lighter grid
      ),
      labels = list(
        title_in_plot  = FALSE,                                   # IEEE default: FALSE
        title_pattern  = "{type} Countries by {metric} ({years})",
        x_axis_pattern = "Total Publications ({years})",
        format_commas  = TRUE
      ),
      insets = list(
        w     = 0.30,   # adjusted inset size
        h     = 0.30,
        x_pad = 0.1,   # right margin
        y     = 0.35    # distance from bottom
      ),
      logic  = list(top_contrib_threshold = 0.80),
      margins = list(plot_main = c(2, 6, 2, 2), lorenz = c(1, 1, 1, 1))
    ),

    initialize = function(data, gini = NULL, year_range, metric, type = "top", country_col = "Country") {
      self$data        <- data
      self$gini        <- gini
      self$year_range  <- year_range
      self$metric      <- metric
      self$type        <- type
      self$country_col <- country_col
      self$value_col   <- metric

      private$assert_dependencies()

      # Base title (kept for drafts; may be hidden by config$title_in_plot)
      local_title <- paste(ifelse(type == "top", "Top 10", "Bottom 10"), "Countries by", metric)
      super$initialize(title = local_title, x_label = metric, y_label = "Country")
    },

    set_lorenz_values = function(x) {
      stopifnot(is.numeric(x), length(x) > 1)
      self$lorenz_values <- x
    },

save = function(output_path, export = c("png","svg")) {
  if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

  base <- paste0("m0_eda_", tolower(self$type), "10_", tolower(self$metric))

  # --- PNG (raster) ---
  if ("png" %in% export) {
    fig <- self$getOneColumnPlot()
    ggsave(file.path(output_path, paste0(base, ".png")),
           plot = fig$plot, width = fig$width, height = fig$height,
           dpi = fig$dpi, units = "in")
  }

  # --- SVG (vector) ---
  if ("svg" %in% export) {
    if (requireNamespace("svglite", quietly = TRUE)) {
      fig <- self$getOneColumnPlot()
      ggsave(file.path(output_path, paste0(base, ".svg")),
             plot = fig$plot, width = fig$width, height = fig$height,
             device = svglite::svglite, units = "in")
    } else {
      warning("Paquete 'svglite' no instalado; se omite SVG. Ejecuta install.packages('svglite').")
    }
  }

  # --- EPS (vector, PostScript) ---
  if ("eps" %in% export) {
    # Forzamos familia EPS-compatible
    old_family <- self$config$fonts$base
    self$config$fonts$base <- "Times"   # <- clave para EPS

    # Re-generamos el plot con esa familia (afecta geom_text internos)
    self$generatePlot()
    fig_eps <- self$getOneColumnPlot()

    # No dependemos de Cairo; usamos postscript estándar
    ggsave(file.path(output_path, paste0(base, ".eps")),
           plot = fig_eps$plot, width = fig_eps$width, height = fig_eps$height,
           device = grDevices::postscript, paper = "special", horizontal = FALSE,
           onefile = FALSE, family = "Times", units = "in")

    # Restauramos la fuente original y volvemos a generar el plot “normal”
    self$config$fonts$base <- old_family
    self$generatePlot()
  }

  invisible(TRUE)
},



    generatePlot = function() {
      message("🔧 [1] Iniciando generatePlot...")
      private$validate_inputs()

      df <- private$prepare_data(self$data)

      # Build plots
      p_main   <- private$build_main_plot(df)
      p_lorenz <- private$build_lorenz_plot(private$get_lorenz_values(df[[self$value_col]]))

      # Inset placement
      inset   <- self$config$insets
      inset_x <- 1 - inset$w - inset$x_pad
      inset_y <- inset$y

      self$plot <- cowplot::ggdraw(p_main) +
        cowplot::draw_plot(p_lorenz, x = inset_x, y = inset_y, width = inset$w, height = inset$h) +
        theme(plot.background = element_rect(fill = self$config$colors$bg, color = NA))

      message("✅ [Final] Plot generado correctamente (Gini = ", sprintf("%.3f", self$gini), ")")
    }
  ),

  private = list(
    assert_dependencies = function() {
      requireNamespace("cowplot",  quietly = TRUE)
      requireNamespace("scales",   quietly = TRUE)
      if (!requireNamespace("DescTools", quietly = TRUE)) requireNamespace("ineq", quietly = TRUE)
    },

    validate_inputs = function() {
      if (is.null(self$data) || !NROW(self$data)) stop("❌ self$data is empty.")
      if (!(self$value_col %in% names(self$data)) || !(self$country_col %in% names(self$data))) {
        stop("❌ Las columnas especificadas no existen en self$data.")
      }
    },

    prepare_data = function(dfin) {
      df <- dfin
      df[[self$country_col]][df[[self$country_col]] == "NA"] <- NA
      df <- df[!is.na(df[[self$country_col]]), , drop = FALSE]

      thr       <- self$config$logic$top_contrib_threshold
      df_sorted <- df[order(-df[[self$value_col]]), , drop = FALSE]
      cum_sum   <- cumsum(df_sorted[[self$value_col]])
      total_sum <- sum(df_sorted[[self$value_col]], na.rm = TRUE)
      idx_thr   <- suppressWarnings(which(cum_sum > thr * total_sum)[1])
      idx_thr   <- ifelse(is.na(idx_thr), nrow(df_sorted), idx_thr)
      top_names <- df_sorted[[self$country_col]][seq_len(idx_thr)]

      df$Highlight <- df[[self$country_col]] %in% top_names
      df
    },

    # ---- templating helpers ----
    year_range_string = function() {
      if (!is.null(self$year_range) && length(self$year_range) == 2) {
        sprintf("%d–%d", self$year_range[1], self$year_range[2])
      } else ""
    },

    render_title = function() {
      lab   <- self$config$labels
      typ   <- ifelse(self$type == "top", "Top 10", "Bottom 10")
      years <- private$year_range_string()

      pat <- if (!is.null(lab$title_pattern)) lab$title_pattern else super$title

      out <- gsub("{type}",   typ,         pat, fixed = TRUE)
      out <- gsub("{metric}", self$metric, out, fixed = TRUE)
      out <- gsub("{years}",  years,       out, fixed = TRUE)
      out <- gsub(" \\(\\)$", "", out)
      out
    },

    render_x_axis = function() {
      years <- private$year_range_string()
      lab   <- self$config$labels
      pat   <- if (!is.null(lab$x_axis_pattern)) lab$x_axis_pattern
               else if (!is.null(lab$x_axis))    lab$x_axis
               else                              "Total Publications"

      out <- gsub("{years}", years, pat, fixed = TRUE)
      out <- gsub(" \\(\\)$", "", out)
      out
    },

    get_lorenz_values = function(values_subset) {
      if (!is.null(self$lorenz_values)) return(sort(self$lorenz_values, decreasing = FALSE, na.last = NA))
      sort(values_subset, decreasing = FALSE, na.last = NA)
    },

    build_main_plot = function(df) {
      cfg <- self$config
      fill_main <- ifelse(self$type == "top", cfg$colors$bar_top, cfg$colors$bar_bottom)

      p <- ggplot(df, aes(
        x = .data[[self$value_col]],
        y = reorder(.data[[self$country_col]], .data[[self$value_col]])
      )) +
        geom_col(aes(fill = Highlight), width = cfg$sizes$bar_width, show.legend = FALSE) +
        geom_text(aes(label = .data[[self$value_col]]),
                  hjust = -0.2, size = cfg$sizes$value_text, family = cfg$fonts$base) +
        scale_fill_manual(values = c("TRUE" = fill_main, "FALSE" = scales::alpha(cfg$colors$bar_other, 0.25))) +
        labs(
          title = if (cfg$labels$title_in_plot) private$render_title() else NULL,
          x     = private$render_x_axis(),
          y     = NULL
        ) +
        super$getTheme() +
        theme(
          axis.line           = element_blank(),
          axis.title.y        = element_blank(),
          axis.text.y         = element_text(margin = margin(r = 3)),
          panel.grid.major.y  = element_blank(),
          panel.grid.minor    = element_blank(),
          panel.border        = element_blank(),  # remove heavy frame
          panel.grid.major.x  = element_line(linewidth = cfg$sizes$grid_major_x, colour = "grey80"),
          plot.margin         = do.call(margin, as.list(cfg$margins$plot_main))
        )

      max_val <- max(df[[self$value_col]], na.rm = TRUE)
      p + scale_x_continuous(
          labels = if (cfg$labels$format_commas) scales::comma else waiver(),
          expand = expansion(mult = c(0, 0.12)),
          limits = c(0, max_val * 1.08)
        ) +
        coord_cartesian(clip = "off")  # allow value labels to overflow cleanly
    },

    build_lorenz_plot = function(values_vec) {
      cfg <- self$config
      has_desc <- requireNamespace("DescTools", quietly = TRUE)
      lc_fun   <- if (has_desc) DescTools::Lc   else ineq::Lc
      gini_fun <- if (has_desc) DescTools::Gini else ineq::Gini

      lc_obj    <- lc_fun(values_vec)
      lorenz_df <- data.frame(p = lc_obj$p, L = lc_obj$L)
      self$gini <- if (is.null(self$gini)) gini_fun(values_vec) else self$gini

      sm <- as.data.frame(spline(lorenz_df$p, lorenz_df$L, n = 200))
      gini_lab <- data.frame(x = 0.32, y = 0.9, label = sprintf("Gini = %.3f", self$gini))

      ggplot(sm, aes(x = x, y = y)) +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed",
                    color = cfg$colors$lorenz_diag, linewidth = cfg$sizes$diag_line) +
        geom_line(color = cfg$colors$lorenz_line, linewidth = cfg$sizes$lorenz_line, lineend = "round") +
        geom_text(data = gini_lab, aes(x, y, label = label),
                  size = cfg$sizes$lorenz_text, family = cfg$fonts$base, fontface = "bold") +
        scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
        scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
        labs(title = NULL) +
        theme_void(base_family = cfg$fonts$base) +
        theme(
          plot.background  = element_rect(fill = cfg$colors$inset_card_fill, color = NA),
          panel.background = element_rect(fill = "transparent", color = NA),
          panel.border     = element_rect(color = "black", fill = NA, linewidth = cfg$sizes$border_line),
          plot.margin      = do.call(margin, as.list(cfg$margins$lorenz))
        )
    }
  )
)
