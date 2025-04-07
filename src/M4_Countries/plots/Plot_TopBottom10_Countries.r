
# -------------------------------------------
# PLOT CLASS: Top/Bottom Country Plots
# -------------------------------------------
Plot_TopBottom10_Countries <- R6::R6Class("Plot_TopBottom10_Countries",

  inherit = R_IEEE_Plot,

  public = list(
    data = NULL,
    gini = NULL,
    metric = NULL,
    year_range = NULL,
    type = NULL, # "top" or "bottom"
    country_col = "Country",
    value_col = NULL,

    # Constructor
    initialize = function(data, gini, year_range, metric, type = "top", country_col = "Country") {
      self$data <- data
      self$year_range <- year_range
      self$metric <- metric
      self$type <- type
      self$country_col <- country_col
      self$value_col <- metric

      title_prefix <- ifelse(type == "top", "Top 10", "Bottom 10")

        # Compute year range if available
        year_suffix <- ""
        if (!is.null(self$year_range) && length(self$year_range) == 2) {
            year_suffix <- paste0(" (", self$year_range[1], "–", self$year_range[2], ")")
        }


 
        
      super$initialize(
        title = paste(title_prefix, "Countries by", metric, ""),
        x_label = metric,
        y_label = "Country"
      )

      message(' ')
      message(' ')
      message(' ')
      message(' ============> self$data <- data ')
      message(' ')
      print(data)
      message(' ')
      message(' ')
      message(' ')
      message(' ')
      message(' ')

      #self$generatePlot()
    },

    # Save plot to path with filename based on metric and type
    save = function(output_path) {
      fname <- paste0("m0_eda_", tolower(self$type), "10_", tolower(self$metric), ".png")
      fig <- self$getOneColumnPlot()
      ggsave(file.path(output_path, fname), plot = fig$plot, width = fig$width, height = fig$height, dpi = fig$dpi)
    },

    generatePlot = function() {
        message("🔧 [1] Iniciando generatePlot...")

        # ---------------------------
        # 📌 Validaciones iniciales
        # ---------------------------
        if (!(self$value_col %in% names(self$data)) || !(self$country_col %in% names(self$data))) {
            stop("❌ Las columnas especificadas no existen en self$data.")
        }

        self$data[[self$country_col]][self$data[[self$country_col]] == "NA"] <- NA
        self$data <- self$data[!is.na(self$data[[self$country_col]]), ]

        # ---------------------------
        # 📊 Cálculo de top 80%
        # ---------------------------
        df_sorted <- self$data[order(-self$data[[self$value_col]]), ]
        cum_sum <- cumsum(df_sorted[[self$value_col]])
        total_sum <- sum(df_sorted[[self$value_col]], na.rm = TRUE)
        index_80 <- which(cum_sum > 0.8 * total_sum)[1]
        top80_countries <- df_sorted[[self$country_col]][1:index_80]
        self$data$Highlight <- self$data[[self$country_col]] %in% top80_countries

        # ---------------------------
        # 🏷️ Título con año si aplica
        # ---------------------------
        if (!is.null(self$year_range) && length(self$year_range) == 2) {
            self$title <- paste0(self$title, " (", self$year_range[1], "–", self$year_range[2], ")")
        }

        # ---------------------------
        # 🎨 Estética y colores
        # ---------------------------
        fill_color <- ifelse(self$type == "top", "steelblue", "darkred")

        # ---------------------------
        # 📈 Crear gráfico principal
        # ---------------------------
        p <- ggplot(data = self$data, aes(
            x = .data[[self$value_col]],
            y = reorder(.data[[self$country_col]], .data[[self$value_col]])
        )) +
            geom_bar(aes(fill = Highlight), stat = "identity", show.legend = FALSE) +
            geom_text(aes(label = .data[[self$value_col]]),
                    hjust = -0.2, size = 3.5, family = "serif") +
            scale_fill_manual(values = c("TRUE" = fill_color, "FALSE" = alpha("gray60", 0.3))) +
            labs(title = self$title, x = "", y = NULL) +
            super$getTheme() +
            theme(
            plot.margin = margin(0, 0, 0, 0),
            axis.title.y = element_blank(),
            axis.text.y = element_text(margin = margin(r = 3))
            )

        max_val <- max(self$data[[self$value_col]], na.rm = TRUE)
        p <- p + xlim(0, max_val * 1.1)

        # ---------------------------
        # 📉 Subgráfico: Lorenz + Gini
        # ---------------------------
        values <- sort(self$data[[self$value_col]], decreasing = FALSE, na.last = NA)
        lc_obj <- DescTools::Lc(values)
        lorenz_df <- data.frame(p = lc_obj$p, L = lc_obj$L)
        gini_val <- DescTools::Gini(values)

        smooth_curve <- as.data.frame(spline(lorenz_df$p, lorenz_df$L))
        gini_label_df <- data.frame(
            x = 0.35,
            y = 0.85,
            label = paste0("Gini = ", round(gini_val, 3))
        )

        gini_plot <- ggplot(smooth_curve, aes(x = x, y = y)) +
            geom_line(color = "black", linewidth = 0.5) +
            geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
            geom_text(
            data = gini_label_df,
            aes(x = x, y = y, label = label),
            size = 3, family = "serif", fontface = "bold",
            inherit.aes = FALSE
            ) +
            labs(title = "Inequality (Lorenz) Curve", x = "", y = "") +
            scale_x_continuous(expand = c(0, 0), breaks = seq(0, 0.9, 0.1)) +
            scale_y_continuous(expand = c(0, 0), breaks = seq(0, 0.9, 0.1)) +
            theme_minimal(base_family = "serif") +
            theme(
            plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
            plot.background = element_rect(fill = "red", color = NA),
            plot.margin = margin(0, 0, 0, 0),
            panel.background = element_rect(fill = "white", color = NA),
            panel.grid = element_blank(),
            panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
            axis.text = element_blank(),
            axis.ticks.length = unit(-2, "pt"),
            axis.ticks = element_line(color = "black", linewidth = 0.4),
            axis.line = element_blank(),
            axis.title = element_blank()
            )

        # ---------------------------
        # 📐 Calcular posición Y exacta del panel (usando ggplotGrob)
        # ---------------------------
        gtable_p <- ggplotGrob(p)
        panel_index <- which(gtable_p$layout$name == "panel")
        panel_layout <- gtable_p$layout[panel_index, ]

        height_total <- sum(as.numeric(gtable_p$heights))
        panel_height <- sum(as.numeric(gtable_p$heights[panel_layout$t:panel_layout$b]))
        y_bottom_panel <- sum(as.numeric(gtable_p$heights[1:panel_layout$b])) / height_total

        # Identificamos el índice y layout del panel
        panel_index <- which(gtable_p$layout$name == "panel")
        panel_layout <- gtable_p$layout[panel_index, ]

        # Convertimos todas las alturas a cm
        heights_cm <- convertHeight(gtable_p$heights, unitTo = "cm", valueOnly = TRUE)

        # Altura total del gráfico
        height_total_cm <- sum(heights_cm)

        # Altura del título (generalmente la fila justo antes del panel)
        title_height_cm <- sum(heights_cm[1:(panel_layout$t - 1)])

        # Altura del panel (donde están las barras)
        panel_height_cm <- convertHeight(unit(1, "null"), "cm", valueOnly = TRUE)

        # Altura del espacio inferior (lo que queda después del panel)
        bottom_space_cm <- sum(heights_cm[(panel_layout$b + 1):length(heights_cm)])

        # Mostrar resultados
        cat("📐 Altura total          :", round(height_total_cm, 2), "cm\n")
        cat("📐 Altura del título     :", round(title_height_cm, 2), "cm\n")
        cat("📐 Altura del panel      :", round(panel_height_cm, 2), "cm\n")
        cat("📐 Espacio inferior (cm) :", round(bottom_space_cm, 2), "cm\n")

        # ---------------------------
        # 🧩 Combinar usando cowplot
        # ---------------------------
        
        y_offset <- 0.00000000000000000      # desplazamiento manual encima del eje X
       
        w_rel <- 0.33
        h_rel <- 0.4

        x_rel <- 0.5   # posición X relativa
        y_rel <- 0#h_rel # y_bottom_panel # - h_rel + y_offset

        hjust <- -0.5
        vjust <- -0

        final_plot <- cowplot::ggdraw(p) + cowplot::draw_plot(gini_plot, x = x_rel, y = y_rel, width = w_rel, height = h_rel, hjust = hjust, vjust = vjust) +
            theme(plot.margin = margin(0, 0, 0, 0)) +
            coord_cartesian(clip = "off") +
            theme(plot.background = element_rect(fill = "white", color = NA))

        # ---------------------------
        # ✅ Finalizar
        # ---------------------------
        self$plot <- final_plot
        message("✅ [Final] Plot generado correctamente")
    }
 
  )
)
