# ---------------------------------------------------------------------------- #
# Metric 1 (M4_M1_TCP_MCP): TCP/TC and SCP/MCP Quadrant Maps
# Uses: Map_Countries_Quadrant_Plot (R6 class provided separately)
# ---------------------------------------------------------------------------- #
suppressPackageStartupMessages({
  library(ggplot2)
  library(jsonlite)
})

M4_M1_QUADRANTS_TP_TC_MCP_SCP <- setRefClass(
  "M4_M1_QUADRANTS_TP_TC_MCP_SCP",

  fields = list(
    df           = "data.frame",
    country_col  = "character",
    year_col     = "character",
    map          = "ANY",         # instance of Map_Countries_Quadrant_Plot
    plots        = "list",
    results      = "list",
    params       = "list"
  ),

  methods = list(

    # ------------------------------------------------------------------------
    # Constructor
    # ------------------------------------------------------------------------
    initialize = function(df,
                          country_col = "Countries",     # bibliometrix metaTagExtraction name
                          year_col    = "Publication Year",
                          N_years     = 5,
                          num_countries = 10,
                          show_arrows = TRUE,
                          show_scale_arrows = TRUE) {

      .self$df          <- as.data.frame(df)
      .self$country_col <- country_col
      .self$year_col    <- year_col
      .self$plots       <- list()
      .self$results     <- list()
      .self$params      <- list(
        N_years = N_years,
        num_countries = num_countries,
        show_arrows   = show_arrows,
        show_scale_arrows = show_scale_arrows
      )
    },

    # ------------------------------------------------------------------------
    # Core pipeline
    # ------------------------------------------------------------------------
    run = function() {
  # Dependencias suaves
  requireNamespace("ggplot2", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)

  # 1) Instanciar el motor de cuadrantes con el df crudo
  df <- .self$df
  m1 <- Map_Countries_Quadrant_Plot$new(df, N_years = 5, num_countries = 10)
  .self$map <- m1



  # 2) Generar figuras
  p_tp_tc   <- m1$generate_bubble_tp_vs_tc_plot(layout="onecol")
  p_scp_mcp <- m1$generate_bubble_scp_vs_mcp_plot()


  .self$plots <- list(
    tp_tc   = p_tp_tc,
    scp_mcp = p_scp_mcp
  )

  # 3) Resúmenes reproducibles usando los datos ya procesados
  cd <- .self$map$get_top_countries_dataset()

  # --- TP / TC ---
  tp_tc_summary <- tryCatch({
    tmp <- dplyr::mutate(
      cd,
      Total_Citations   = Total_Citations_first_half   + Total_Citations_second_half,
      Total_Paper_Count = Total_Paper_Count_first_half + Total_Paper_Count_second_half
    )
    list(
      median_citations = stats::median(tmp$Total_Citations,   na.rm = TRUE),
      median_papers    = stats::median(tmp$Total_Paper_Count, na.rm = TRUE),
      n_countries      = nrow(tmp)
    )
  }, error = function(e) list(error = paste("tp_tc_summary:", conditionMessage(e))))

  # --- SCP / MCP ---
  scp_mcp_summary <- tryCatch({
    tmp <- dplyr::mutate(
      cd,
      Total_SCP = SCP_Count_first_half + SCP_Count_second_half,
      Total_MCP = MCP_Count_first_half + MCP_Count_second_half
    )
    list(
      median_scp  = stats::median(tmp$Total_SCP, na.rm = TRUE),
      median_mcp  = stats::median(tmp$Total_MCP, na.rm = TRUE),
      n_countries = nrow(tmp)
    )
  }, error = function(e) list(error = paste("scp_mcp_summary:", conditionMessage(e))))

  .self$results <- list(
    params    = list(N_years = 5, num_countries = 10),
    summaries = list(tp_tc = tp_tc_summary, scp_mcp = scp_mcp_summary)
  )

  invisible(.self)
},

    # ------------------------------------------------------------------------
    # Save both plots (PNG/SVG/EPS). EPS usa familia 'Times' (PostScript-safe)
    # ------------------------------------------------------------------------
    save_plot = function(output_path, export = c("png","svg","eps"),
                         basename_tp_tc   = "m1_tcp_mcp_tp_tc",
                         basename_scp_mcp = "m1_tcp_mcp_scp_mcp") {

      if (!length(.self$plots)) stop("[M4_M1_TCP_MCP] run() must be called before save_plot().")
      if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

      .self$.__save_one__(.self$plots$tp_tc,   file.path(output_path, basename_tp_tc),   export)
      .self$.__save_one__(.self$plots$scp_mcp, file.path(output_path, basename_scp_mcp), export)

      invisible(TRUE)
    },

    # ------------------------------------------------------------------------
    # Save JSON summary
    # ------------------------------------------------------------------------
    save_json = function(output_path, filename = "m1_tcp_mcp.json") {
      if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)
      jsonlite::write_json(.self$results,
                           path = file.path(output_path, filename),
                           pretty = TRUE, auto_unbox = TRUE, null = "null")
      invisible(TRUE)
    },

    # ============================ PRIVATE HELPERS ============================

    # guardar un plot en varios formatos (PNG/SVG/EPS con familia segura)
    .__save_one__ = function(plot, filepath_base, export = c("png","svg")) {
      # Tamaños sensatos por defecto (en pulgadas) y dpi “rápido”
      width_in  <- 7.16   # ~183 mm
      height_in <- 3.5    # reasonable aspect ratio, adjust if you need taller
      dpi_r     <- 600    # high quality for print


      # --- PNG ---
      if ("png" %in% export) {
        ggsave(paste0(filepath_base, ".png"),
               plot = plot, width = width_in, height = height_in,
               dpi = dpi_r, units = "in")
      }

      # --- SVG ---
      if ("svg" %in% export) {
        if (requireNamespace("svglite", quietly = TRUE)) {
          ggsave(paste0(filepath_base, ".svg"),
                 plot = plot, width = width_in, height = height_in,
                 device = svglite::svglite, units = "in")
        } else {
          warning("[M4_M1_TCP_MCP] 'svglite' not installed; skipping SVG for ", basename(filepath_base))
        }
      }

      # --- EPS (PostScript)—forzamos familia Times
      if ("eps" %in% export) {
        plot_eps <- plot + theme(text = element_text(family = "Times"))
        ggsave(paste0(filepath_base, ".eps"),
               plot = plot_eps, width = width_in, height = height_in,
               device = grDevices::postscript,
               family = "Times", paper = "special",
               horizontal = FALSE, onefile = FALSE, units = "in")
      }
    }
  )
)



# ===== DEBUG: inspección de 'data' para M4_M1_TCP_MCP =====
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

debug_inspect_data <- function(data) {
  cat("\n\n========== DEBUG INPUT ==========\n")
  cat("* class(data):", paste(class(data), collapse=", "), "\n")
  cat("* nrow / ncol:", tryCatch(nrow(data), error=function(e) NA), "/", tryCatch(ncol(data), error=function(e) NA), "\n")
  cat("* primeras 40 columnas:\n", paste(head(names(data), 40), collapse=", "), "\n")

  # Candidatas por nombre (insensible a mayúsculas)
  has <- function(pat) grep(pat, names(data), ignore.case = TRUE, value = TRUE)
  year_candidates <- c(intersect(names(data), c("Publication Year","PY","Year","year","PublicationYear","pub_year")), has("^py$|year"))
  tc_candidates   <- c(intersect(names(data), c("Times Cited","TC","Citations","times_cited","TimesCited")), has("cited|citations|^tc$"))
  ctry_candidates <- c(intersect(names(data), c("Countries","Country","Countries_String","Country_String","Countries_Array","Country_Array")), has("countr"))

  cat("* candidatos YEAR:", paste(unique(year_candidates), collapse=", "), "\n")
  cat("* candidatos TIMES CITED:", paste(unique(tc_candidates), collapse=", "), "\n")
  cat("* candidatos COUNTRIES:", paste(unique(ctry_candidates), collapse=", "), "\n")

  # Muestra rápida de columnas candidatas (si existen)
  show_cols <- unique(c(head(year_candidates,1), head(tc_candidates,1), head(ctry_candidates,2)))
  show_cols <- show_cols[nzchar(show_cols)]
  if (length(show_cols)) {
    cat("\n> Vista previa de columnas candidatas:\n")
    print(utils::head(as.data.frame(data)[, intersect(show_cols, names(data)), drop=FALSE], 5))
  } else {
    cat("\n> No se encontraron columnas candidatas por nombres comunes.\n")
  }

  # Marcas bibliometrix
  if ("DB" %in% names(data)) {
    cat("* Se detecta columna 'DB' (posible objeto bibliometrix). Valores únicos de DB:\n")
    print(unique(as.data.frame(data)$DB))
  }

  cat("========== END DEBUG ==========\n\n")
}


quick_check_m1 <- function(m1) {
  stopifnot("Map_Countries_Quadrant_Plot" %in% class(m1))

  cat("\n== Class & params ==\n")
  print(list(
    class = class(m1),
    N_years = m1$N_years,
    num_countries = m1$num_countries,
    show_arrows = m1$show_arrows,
    show_scale_arrows = m1$show_scale_arrows
  ))

  cat("\n== Raw df ==\n")
  cat("nrow:", nrow(m1$df), " ncol:", ncol(m1$df), "\n")
  print(head(names(m1$df), 30))

  req <- c("Publication Year", "Times Cited", "Countries_Array")
  missing <- setdiff(req, names(m1$df))
  if (length(missing)) stop("Faltan columnas: ", paste(missing, collapse = ", "))

  cat("\nTipos de columnas clave:\n")
  print(sapply(m1$df[req], class))

  cat("\nEjemplos Countries_Array (primeras 3 filas):\n")
  print(utils::head(lapply(m1$df$Countries_Array, head), 3))

  yr <- range(m1$df[["Publication Year"]], na.rm = TRUE)
  tc_min <- suppressWarnings(min(m1$df[["Times Cited"]], na.rm = TRUE))
  tc_max <- suppressWarnings(max(m1$df[["Times Cited"]], na.rm = TRUE))
  cat("\nRango de años:", paste(yr, collapse = " - "),
      "\nTimes Cited min/max:", tc_min, "/", tc_max, "\n")

  cat("\n== Country-level summaries (joined halves) ==\n")
  stopifnot(is.data.frame(m1$country_data))
  print(dplyr::glimpse(m1$country_data))

  cat("\nFilas en country_data:", nrow(m1$country_data), "\n")
  cat("¿NA en deltas? ",
      anyNA(m1$country_data$delta_x) || anyNA(m1$country_data$delta_y), "\n")

  if ("Total_Citations_second_half" %in% names(m1$country_data)) {
    cat("\nTop por citas (second half):\n")
    print(
      m1$country_data %>%
        dplyr::arrange(dplyr::desc(Total_Citations_second_half)) %>%
        dplyr::select(dplyr::starts_with("Countries_Array"),
                      dplyr::starts_with("Total_Citations")) %>%
        utils::head(10)
    )
  }

  invisible(TRUE)
}
