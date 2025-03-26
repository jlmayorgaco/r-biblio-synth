install.packages("ggbrace")
library(ggbrace)
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
        title = paste(title_prefix, "Countries by", metric, "", year_suffix),
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

generatePlot = function() {
  message("🔧 [1] Iniciando generatePlot...")

  fill_color <- ifelse(self$type == "top", "steelblue", "darkred")
  message("🎨 [2] Color definido: ", fill_color)

  # Sort and compute top 80%
  df_sorted <- self$data[order(-self$data[[self$value_col]]), ]
  message("📊 [3] Data ordenada")

  cum_sum <- cumsum(df_sorted[[self$value_col]])
  total_sum <- sum(df_sorted[[self$value_col]], na.rm = TRUE)
  index_80 <- which(cum_sum >= 0.8 * total_sum)[1]
  top80_countries <- df_sorted[[self$country_col]][1:index_80]
  message("📈 [4] Index que acumula 80%: ", index_80)

  # Flag countries
  self$data$Highlight <- self$data[[self$country_col]] %in% top80_countries
  message("🏷️ [5] Marcas de Highlight aplicadas")

  # Iniciar ggplot
  p <- ggplot(self$data, aes_string(
    x = self$value_col,
    y = paste0("reorder(", self$country_col, ", ", self$value_col, ")")
  ))
  message("📐 [6] ggplot creado")

  p <- p + geom_bar(stat = "identity", aes(fill = Highlight))  # Asegura uso de aes para fill
  message("📊 [7] Barras agregadas")

  p <- p + geom_text(
    aes_string(label = self$value_col),
    hjust = -0.2, size = 3.5, family = "serif"
  )
  message("📝 [8] Etiquetas agregadas")

  p <- p + super$getTheme()
  message("🎨 [9] Tema IEEE aplicado")

  p <- p + theme(
    axis.title.y = element_blank(),
    plot.margin = margin(5, 5, 5, 5),
    axis.text.y = element_text(margin = margin(r = 3))
  )

  p <- p + labs(
    title = self$title,
    x = "",  # o usa self$x_label si prefieres
    y = NULL
  )
  message("🏷️ [10] Título y etiquetas aplicadas")

  max_val <- max(self$data[[self$value_col]], na.rm = TRUE)
  p <- p + xlim(0, max_val * 1.1)
  message("📏 [11] Limite X ajustado")

  # Línea horizontal y etiqueta del 80%
  if(FALSE){
  country_80 <- df_sorted[[self$country_col]][index_80]
  threshold_df <- data.frame(
    x = max_val * 0.95,
    y = country_80,
    label = "80%"
  )
 

  p <- p +
    geom_hline(
      yintercept = index_80,
      linetype = "dashed",
      color = "black",
      linewidth = 0.5
    ) +
    geom_text(
      data = threshold_df,
      aes(x = x, y = y, label = label),
      vjust = -0.3,
      hjust = 1,
      family = "serif",
      size = 3
    )
  message("🧭 [12] Línea y etiqueta del 80% agregadas")

   }

# Gini inset (Lorenz curve + Gini value)
if (!is.null(self$data) && self$value_col %in% names(self$data)) {
  message("📈 Generando curva de Lorenz...")

  # Ordenar valores
  values <- sort(self$data[[self$value_col]], decreasing = FALSE, na.last = NA)

  # Calcular curva de Lorenz
  lorenz_data <- as.data.frame(DescTools::Lc(values))
  gini_val <- DescTools::Gini(values)

  # Crear gráfico de Lorenz con Gini como etiqueta
  gini_plot <- ggplot(lorenz_data, aes(x = p, y = L)) +
    geom_line(color = "black", size = 0.5) +
    geom_abline(intercept = 0, slope = 1, linetype = "dotted", color = "gray") +
    annotate("text", x = 0.5, y = 0.1, label = paste0("Gini = ", round(gini_val, 3)),
             size = 3, family = "serif", fontface = "bold") +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = "black", linewidth = 0.3)
    )

  gini_grob <- ggplotGrob(gini_plot)

  p <- p + annotation_custom(
    grob = gini_grob,
    xmin = max_val * 0.6, xmax = max_val * 1.05,
    ymin = -Inf, ymax = Inf
  )

  message("✅ Curva de Lorenz incrustada con Gini = ", round(gini_val, 3))
}


  self$plot <- p
  message("✅ [14] Plot generado y asignado a self$plot")
}



,


    # Save plot to path with filename based on metric and type
    save = function(output_path) {
      fname <- paste0("m0_eda_", tolower(self$type), "10_", tolower(self$metric), ".png")
      fig <- self$getOneColumnPlot()
      ggsave(file.path(output_path, fname), plot = fig$plot, width = fig$width, height = fig$height, dpi = fig$dpi)
    }
  )
)
