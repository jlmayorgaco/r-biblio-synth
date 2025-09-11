# ==============================================================================
# M4_M2_GEO_MAPPING (fixed + verbose logs)
# ==============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(jsonlite)
})

M4_M2_GEO_MAPPING <- setRefClass(
  "M4_M2_GEO_MAPPING",
  fields = list(
    df           = "data.frame",
    country_col  = "character",
    year_col     = "character",
    tc_col       = "character",
    plots        = "list",
    results      = "list",
    params       = "list",
    agg          = "data.frame",
    joined       = "ANY"
  ),

  methods = list(

    initialize = function(df,
                          country_col = "Countries_Array",
                          year_col    = "Publication Year",
                          tc_col      = "Times Cited",
                          metric      = c("TP","TC","CPP","MCP_ratio"),
                          year_min    = NA,
                          year_max    = NA,
                          # binning
                          bin_style   = c("jenks","quantile","equal","log_equal"),
                          bins        = 6,
                          # aesthetics
                          palette     = c("grayscale","viridis"),
                          projection  = c("robinson","eckert4"),
                          legend_right= TRUE,
                          # labels
                          label_top_n = 0,
                          # north arrow
                          show_north  = TRUE,
                          north_scale = 0.045,
                          north_pos   = NULL,        # c(left,bottom,width,height)
                          # scale bar
                          show_scale  = TRUE,
                          scale_km    = 1000,
                          scale_cells = 10,
                          scale_pos   = NULL,        # c(left,bottom,width,height)
                          # ocean backdrop
                          show_ocean  = TRUE,
                          ocean_fill  = "#f6f7fa",
                          # misc
                          debug       = FALSE) {

      .self$df          <- as.data.frame(df)
      .self$country_col <- country_col
      .self$year_col    <- year_col
      .self$tc_col      <- tc_col
      .self$plots       <- list()
      .self$results     <- list()

      .self$params <- list(
        metric        = match.arg(metric),
        year_min      = year_min,
        year_max      = year_max,
        bin_style     = match.arg(bin_style, c("jenks","quantile","equal","log_equal")),
        bins          = as.integer(bins),
        palette       = match.arg(palette),
        projection    = match.arg(projection),
        legend_right  = isTRUE(legend_right),
        label_top_n   = as.integer(label_top_n),
        show_north    = isTRUE(show_north),
        north_scale   = as.numeric(north_scale),
        north_pos     = north_pos,
        show_scale    = isTRUE(show_scale),
        scale_km      = as.numeric(scale_km),
        scale_cells   = max(2L, as.integer(scale_cells)),
        scale_pos     = scale_pos,
        show_ocean    = isTRUE(show_ocean),
        ocean_fill    = as.character(ocean_fill),
        debug         = isTRUE(debug)
      )

      .self$.__log__("init", list(
        nrow_df = nrow(.self$df),
        metric  = .self$params$metric,
        bins    = .self$params$bins,
        bin_style = .self$params$bin_style
      ))
    },

    run = function() {
      .self$.__need__(c("dplyr","tidyr","sf","countrycode","rnaturalearth",
                        "rnaturalearthdata","classInt","patchwork"))
      if (.self$params$palette == "viridis") .self$.__need__("viridisLite")
      if (.self$params$label_top_n > 0) .self$.__need__("ggrepel")

      df <- .self$df
      cc <- .self$country_col; yc <- .self$year_col; tc <- .self$tc_col
      miss <- setdiff(c(cc,yc,tc), names(df))
      if (length(miss)) stop("[M4_M2] Missing cols: ", paste(miss, collapse=", "))

      if (!is.na(.self$params$year_min)) df <- df[df[[yc]] >= .self$params$year_min, , drop=FALSE]
      if (!is.na(.self$params$year_max)) df <- df[df[[yc]] <= .self$params$year_max, , drop=FALSE]
      .self$.__log__("filter_years", list(nrow_df = nrow(df)))

      countries_list <- .self$.__as_country_list__(df[[cc]])
      df[[tc]] <- suppressWarnings(as.numeric(df[[tc]])); df[[tc]][!is.finite(df[[tc]])] <- 0

      pid <- seq_len(nrow(df))
      long <- utils::stack(stats::setNames(countries_list, pid))
      names(long) <- c("Country","PaperID"); long$PaperID <- as.integer(long$PaperID)

      meta <- df[, c(tc, yc), drop=FALSE]
      meta$PaperID <- pid
      meta$n_countries <- vapply(countries_list, length, integer(1))
      meta$is_mcp <- meta$n_countries > 1L

      long <- merge(long, meta, by="PaperID", all.x=TRUE)
      long$Country <- trimws(as.character(long$Country))
      long <- long[nzchar(long$Country) & !is.na(long$Country), , drop=FALSE]
      .self$.__log__("long_build", list(n_long = nrow(long), unique_countries = length(unique(long$Country))))

      agg <- dplyr::group_by(long, Country) |>
        dplyr::summarise(
          TP  = dplyr::n_distinct(.data$PaperID),
          TC  = sum(.data[[tc]], na.rm=TRUE),
          SCP = sum(!.data$is_mcp, na.rm=TRUE),
          MCP = sum(.data$is_mcp,  na.rm=TRUE),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          CPP       = dplyr::if_else(TP > 0, TC/TP, NA_real_),
          MCP_ratio = dplyr::if_else(TP > 0, MCP/TP, NA_real_),
          iso3c     = countrycode::countrycode(Country, "country.name", "iso3c", warn = FALSE)
        ) |>
        (\(x) x[!is.na(x$iso3c), , drop=FALSE])()
      .self$agg <- as.data.frame(agg)
      .self$.__log__("agg_done", list(n_countries = nrow(.self$agg)))

      world  <- rnaturalearth::ne_countries(scale="medium", returnclass="sf")
      world  <- world[, c("iso_a3","name_long","continent","geometry")]
      joined <- dplyr::left_join(world, .self$agg, by = c("iso_a3"="iso3c"))
      joined <- .self$.__ensure_sf__(joined)
      .self$joined <- joined
      .self$.__log__("join_sf", list(n_features = nrow(joined)))

      metric     <- .self$params$metric
      metric_lab <- switch(metric,
        "TP"="Total Publications","TC"="Total Citations",
        "CPP"="Citations per Paper","MCP_ratio"="International Collaboration (MCP/TP)"
      )
      yr_min <- ifelse(is.na(.self$params$year_min),
                       suppressWarnings(min(.self$df[[.self$year_col]], na.rm=TRUE)),
                       .self$params$year_min)
      yr_max <- ifelse(is.na(.self$params$year_max),
                       suppressWarnings(max(.self$df[[.self$year_col]], na.rm=TRUE)),
                       .self$params$year_max)

      caption <- sprintf(
        "Data: Scopus via bibliometrix | Period: %s-%s | Counting: full | Projection: %s | Grey = No data",
        yr_min, yr_max, tools::toTitleCase(.self$params$projection)
      )

      p_base <- .self$.__build_map_base__(
        joined_sf   = joined,
        metric      = metric,
        metric_lab  = metric_lab,
        caption     = caption,
        bins        = .self$params$bins,
        bin_style   = .self$params$bin_style,
        palette     = .self$params$palette,
        projection  = .self$params$projection,
        legend_right= .self$params$legend_right,
        label_top_n = .self$params$label_top_n,
        show_ocean  = .self$params$show_ocean,
        ocean_fill  = .self$params$ocean_fill
      )

      if (.self$params$show_north) {
        comp <- .self$.__compass_plot__()
        cp <- .self$params$north_pos
        if (is.null(cp)) {
          w <- .self$params$north_scale * 1.6
          h <- .self$params$north_scale * 1.6
          cp <- c(left = 0.035, bottom = 0.74, width = w, height = h)
        }
        p_base <- p_base + patchwork::inset_element(
          comp,
          left = cp["left"], bottom = cp["bottom"],
          right = cp["left"] + cp["width"], top = cp["bottom"] + cp["height"],
          align_to = "panel", clip = TRUE
        )
        .self$.__log__("inset_north", as.list(cp))
      }

      if (.self$params$show_scale) {
        sc <- .self$.__scale_plot__(scale_km = .self$params$scale_km,
                                    cells    = .self$params$scale_cells)
        sp <- if (is.null(.self$params$scale_pos)) {
          c(left = 0.12, bottom = 0.06, width = 0.24, height = 0.07)
        } else .self$params$scale_pos
        p_base <- p_base + patchwork::inset_element(
          sc, left = sp["left"], bottom = sp["bottom"],
          right = sp["left"] + sp["width"], top = sp["bottom"] + sp["height"],
          align_to = "panel", clip = TRUE
        )
        .self$.__log__("inset_scale", as.list(sp))
      }

      .self$plots <- list(choropleth = p_base)

      vals <- .self$agg[[metric]]; vals[!is.finite(vals)] <- NA
      .self$results <- list(
        params = .self$params,
        globals = list(
          n_countries_with_data = sum(!is.na(vals)),
          metric = metric, year_min = yr_min, year_max = yr_max
        ),
        top    = utils::head(.self$agg[order(-vals), c("Country", metric)], 10),
        bottom = utils::head(.self$agg[order(vals),  c("Country", metric)], 10)
      )
      .self$.__log__("run_done", list(n_with_data = .self$results$globals$n_countries_with_data))
      invisible(.self)
    },

    save_plot = function(output_path, export = c("png","svg","eps"), basename = NULL) {
      if (!length(.self$plots)) stop("[M4_M2] run() first.")
      if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)
      if (is.null(basename)) basename <- paste0("m2_geo_map_", .self$params$metric)
      p_vec <- .self$.__vector_safe_plot__(.self$plots$choropleth)
      .self$.__save_png__(.self$plots$choropleth, file.path(output_path, basename, fsep = .Platform$file.sep))
      export <- unique(match.arg(export, several.ok = TRUE))
      if ("svg" %in% export) .self$.__save_svg__(p_vec, file.path(output_path, basename))
      if ("eps" %in% export) .self$.__save_eps__(p_vec, file.path(output_path, basename))
      .self$.__log__("save_plot", list(path = output_path, base = basename, export = paste(export, collapse=",")))
      invisible(TRUE)
    },

    save_json = function(output_path, filename = NULL) {
      if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)
      if (is.null(filename)) filename <- paste0("m2_geo_map_", .self$params$metric, ".json")
      jsonlite::write_json(.self$results,
        path = file.path(output_path, filename),
        pretty = TRUE, auto_unbox = TRUE, null = "null")
      .self$.__log__("save_json", list(path = output_path, file = filename))
      invisible(TRUE)
    },

    # ============================ HELPERS ====================================

    .__need__ = function(pkgs) {
      miss <- pkgs[!vapply(pkgs, requireNamespace, quietly=TRUE, FUN.VALUE=logical(1))]
      if (length(miss)) stop("[M4_M2] Please install: ", paste(miss, collapse=", "))
      invisible(TRUE)
    },

    .__as_country_list__ = function(x) {
      if (is.list(x)) return(x)
      if (is.character(x)) return(strsplit(x, "\\s*[,;]\\s*"))
      stop("[M4_M2] country_col must be list<chr> or character")
    },

    .__ensure_sf__ = function(x) {
      geo_cols <- grep("^geometry", names(x))
      if (length(geo_cols) > 1L) { x$geometry <- x[[geo_cols[1]]]; x$geometry.x <- NULL; x$geometry.y <- NULL }
      if (!inherits(x, "sf")) x <- sf::st_as_sf(x, sf_column_name="geometry", crs=4326)
      if (is.na(sf::st_crs(x))) sf::st_crs(x) <- sf::st_crs(4326)
      x
    },

    .__proj_crs__ = function(projection = c("robinson","eckert4")) {
      projection <- match.arg(projection)
      if (projection == "robinson") {
        sf::st_crs("+proj=robin +lon_0=0 +datum=WGS84 +units=m +no_defs")
      } else {
        sf::st_crs("+proj=eck4 +lon_0=0 +datum=WGS84 +units=m +no_defs")
      }
    },

    .__ieee_theme__ = function(base_size = 9, family = "Times") {
      theme_minimal(base_size = base_size, base_family = family) +
        theme(
          panel.grid  = element_blank(),
          axis.text   = element_blank(),
          axis.title  = element_blank(),
          axis.ticks  = element_blank(),
          legend.position   = "right",
          legend.direction  = "vertical",
          legend.box        = "vertical",
          legend.justification = "center",
          legend.key.height = grid::unit(4, "mm"),
          legend.key.width  = grid::unit(6, "mm"),
          legend.spacing.y  = grid::unit(2, "mm"),
          legend.text       = element_text(margin = margin(b = 2)),
          plot.title        = element_text(hjust = 0.5, face = "bold", margin = margin(b=4)),
          plot.caption      = element_text(margin = margin(t=6)),
          plot.margin       = margin(2, 2, 2, 2, "mm"),
          panel.border      = element_rect(color="black", fill=NA, linewidth=0.35)
        )
    },

    .__quantize__ = function(x, bins = 6,
                         style = c("jenks","quantile","equal","log_equal"),
                         pad_frac = 0.02) {
  style <- match.arg(style)
  x <- as.numeric(x); x[!is.finite(x)] <- NA_real_
  x_ok <- x[is.finite(x)]
  if (!length(x_ok)) {
    fac <- factor(rep(NA_character_, length(x)))
    return(list(levels = character(), factor = fac, breaks = numeric()))
  }

  # clamp to >=0 and pad the range a bit
  x_ok <- pmax(x_ok, 0)
  rng  <- range(x_ok)
  pad  <- diff(rng) * pad_frac
  lo   <- max(0, rng[1] - pad); hi <- rng[2] + pad

  # compute breaks
  raw_brks <- switch(style,
    "quantile"  = tryCatch(classInt::classIntervals(x_ok, n = bins, style = "quantile")$brks,
                           error = function(e) pretty(c(lo,hi), n = bins + 1)),
    "jenks"     = tryCatch(classInt::classIntervals(x_ok, n = bins, style = "jenks")$brks,
                           error = function(e) pretty(c(lo,hi), n = bins + 1)),
    "equal"     = seq(lo, hi, length.out = bins + 1),
    "log_equal" = {
      x_pos <- x_ok[x_ok > 0]
      if (length(x_pos) > 1) 10^seq(log10(min(x_pos)), log10(max(x_pos)), length.out = bins + 1)
      else pretty(c(lo,hi), n = bins + 1)
    }
  )

  # numeric breaks for cut(): strictly increasing, not rounded
  brks <- sort(unique(c(lo, hi, raw_brks)))
  if (length(brks) < 3L) brks <- pretty(c(lo, hi), n = max(3, bins + 1))
  brks[1] <- 0  # start at 0

  # do the binning
  fac <- cut(x, breaks = brks, include_lowest = TRUE, right = TRUE)

  # pretty labels (formatting only; does not affect the numeric breaks)
  fmt <- function(v) prettyNum(v, big.mark = ",", scientific = FALSE, digits = 6)
  labs <- paste(fmt(head(brks, -1)), fmt(brks[-1]), sep = "–")
  levels(fac) <- labs

  list(levels = labs, factor = fac, breaks = brks)
}
,

   .__build_map_base__ = function(joined_sf, metric, metric_lab, caption,
                               bins, palette, projection, legend_right,
                               label_top_n, bin_style = "jenks",
                               show_ocean = TRUE, ocean_fill = "#f6f7fa") {

          # --- bin & factor ---
          qz   <- .self$.__quantize__(joined_sf[[metric]], bins = bins, style = bin_style)
          lvls <- qz$levels
          joined_sf$bin_class <- factor(qz$factor, levels = lvls, ordered = TRUE)

          # palette (exact length of lvls)
          pal <- if (palette == "grayscale") {
            rev(gray.colors(length(lvls), start = 0.2, end = 0.9))
          } else {
            viridisLite::viridis(length(lvls))
          }

          crs_proj <- .self$.__proj_crs__(projection)
          p <- ggplot()

          # optional ocean
          if (isTRUE(show_ocean)) {
            bb <- sf::st_as_sfc(sf::st_bbox(c(xmin=-180,xmax=180,ymin=-90,ymax=90), crs=4326))
            p <- p + geom_sf(
              data = sf::st_transform(bb, crs_proj),
              inherit.aes = FALSE, fill = ocean_fill, color = NA
            )
          }

          # mapped countries
          p <- p + geom_sf(data = joined_sf, aes(fill = bin_class),
                          linewidth = 0.12, color = "grey60")

          # no-data
          p <- p + geom_sf(
            data = subset(joined_sf, is.na(bin_class)),
            fill = "grey90", color = "grey70", linewidth = 0.12,
            inherit.aes = FALSE, show.legend = FALSE
          )

          # --- key fix: force palette → levels, not dependent on data ---
          if (length(lvls) > 0) {
            p <- p + scale_fill_manual(
              values  = setNames(pal, lvls),  # map palette to *all* levels
              breaks  = lvls,                 # force all bins in legend
              labels  = lvls,                 # keep bin labels
              limits  = lvls,
              drop    = FALSE,
              na.translate = FALSE,
              name    = metric_lab
            )
          }

          # legend guide
          p <- p + guides(
            fill = guide_legend(
              ncol = 1, byrow = TRUE, title.position = "top",
              label.position = "right",
              override.aes = list(color = "grey60", alpha = 1) # force swatches filled
            )
          )

          # projection + theme
          p <- p + coord_sf(crs = crs_proj, expand = FALSE,
                            default_crs = sf::st_crs(4326),
                            datum = sf::st_crs(4326))
          p <- p + .self$.__ieee_theme__()
          if (!isTRUE(legend_right)) {
            p <- p + theme(legend.position = "bottom")
          }
          p <- p + labs(title = paste("Geographic Mapping -", metric_lab), caption = caption)
          p
    },

    .__compass_plot__ = function() {
      s  <- 0.42; w <- 0.14; s2 <- 0.28; w2 <- 0.10
      x0 <- 0.5;  y0 <- 0.5
      tri <- function(ax, ay, bx, by, cx, cy, g)
        data.frame(x = c(ax,bx,cx), y = c(ay,by,cy), grp = g)

      comp <- rbind(
        tri(x0, y0+s, x0-w, y0, x0+w, y0, "N"),
        tri(x0, y0-s, x0-w, y0, x0+w, y0, "S"),
        tri(x0+s, y0, x0, y0-w, x0, y0+w, "E"),
        tri(x0-s, y0, x0, y0-w, x0, y0+w, "W"),
        tri(x0+s2, y0+s2, x0+w2, y0, x0, y0+w2, "NE"),
        tri(x0+s2, y0-s2, x0+w2, y0, x0, y0-w2, "SE"),
        tri(x0-s2, y0-s2, x0-w2, y0, x0, y0-w2, "SW"),
        tri(x0-s2, y0+s2, x0-w2, y0, x0, y0+w2, "NW")
      )

      ggplot() +
        geom_polygon(data = comp, aes(x = x, y = y, group = grp),
                     fill = "white", color = "black", linewidth = 0.3) +
        geom_text(aes(x = 0.5, y = 0.95, label = "N"),
                  family = "Times", size = 2.4) +
        coord_fixed(xlim = c(0,1), ylim = c(0,1), expand = FALSE) +
        theme_void() +
        theme(plot.background = element_rect(fill = NA, color = NA))
    },

    .__scale_plot__ = function(scale_km = 1000, cells = 10) {
      cells <- max(2L, as.integer(cells))
      total_w <- 0.86
      cell_w  <- total_w / cells
      x_left  <- (1 - total_w)/2
      y_mid   <- 0.45
      h       <- 0.33

      df <- data.frame(
        xmin = x_left + (0:(cells-1)) * cell_w,
        xmax = x_left + (1:cells)     * cell_w,
        ymin = y_mid - h/2,
        ymax = y_mid + h/2,
        col  = rep(c("white","black"), length.out = cells)
      )

      ticks <- data.frame(
        x   = c(x_left, x_left + total_w/2, x_left + total_w),
        lab = c("0", format(round(scale_km/2), big.mark=","), paste0(format(round(scale_km), big.mark=","), " km"))
      )

      ggplot() +
        geom_rect(data = df[df$col=="white",],
                  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                  fill = "white", color = "black", linewidth = 0.35) +
        geom_rect(data = df[df$col=="black",],
                  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                  fill = "black", color = "black", linewidth = 0.35) +
        geom_text(data = ticks, aes(x = x, y = 0.88, label = lab),
                  family = "Times", size = 2.5, vjust = 0) +
        coord_fixed(xlim = c(0,1), ylim = c(0,1), expand = FALSE) +
        theme_void() +
        theme(plot.background = element_rect(fill = NA, color = NA))
    },

    .__vector_safe_plot__ = function(p) {
      safe <- function(s) {
        s <- as.character(s %||% "")
        s <- paste(s, collapse = " ")
        s <- gsub("\u2013|\u2014", "-", s)
        s <- gsub("\u00B7|\u2022", "|", s)
        s <- iconv(s, to = "ASCII//TRANSLIT")
        s
      }
      p + labs(
        title   = safe(p$labels$title),
        subtitle= safe(p$labels$subtitle),
        caption = safe(p$labels$caption)
      )
    },

    .__save_png__ = function(plot, filepath_base) {
      width_in  <- 7.16; height_in <- 3.9; dpi_r <- 600
      ggsave(paste0(filepath_base, ".png"),
             plot = plot, width = width_in, height = height_in,
             dpi = dpi_r, units = "in")
    },

    .__save_svg__ = function(plot, filepath_base) {
      if (!requireNamespace("svglite", quietly = TRUE)) {
        warning("[M4_M2] 'svglite' not installed; skipping SVG.")
        return(invisible())
      }
      width_in  <- 7.16; height_in <- 3.9
      tryCatch({
        ggsave(paste0(filepath_base, ".svg"),
               plot = plot, width = width_in, height = height_in,
               device = svglite::svglite, units = "in")
      }, error = function(e) {
        warning("[M4_M2] SVG export failed: ", conditionMessage(e))
      })
    },

    .__save_eps__ = function(plot, filepath_base) {
      width_in  <- 7.16; height_in <- 3.9
      dev_fun <- if (isTRUE(capabilities("cairo"))) grDevices::cairo_ps else grDevices::postscript
      fname   <- paste0(filepath_base, ".eps")
      tryCatch({
        ggsave(
          filename = fname,
          plot = plot + theme(text = element_text(family = "Times")),
          width = width_in, height = height_in, units = "in",
          device = function(...){
            dev_fun(..., family = "Times", paper = "special",
                    onefile = FALSE, horizontal = FALSE, fallback_resolution = 600)
          },
          fallback_resolution = 600, useDingbats = FALSE
        )
      }, error = function(e) {
        warning("[M4_M2] EPS export failed: ", conditionMessage(e),
                " — trying basic postscript.")
        tryCatch({
          ggsave(
            filename = fname,
            plot = plot + theme(text = element_text(family = "serif")),
            width = width_in, height = height_in, units = "in",
            device = grDevices::postscript, family = "serif",
            paper = "special", onefile = FALSE, horizontal = FALSE
          )
        }, error = function(e2) {
          warning("[M4_M2] EPS fallback also failed: ", conditionMessage(e2))
        })
      })
    },

    .__log__ = function(tag, payload = NULL) {
      if (!isTRUE(.self$params$debug)) return(invisible())
      ts <- format(Sys.time(), "%H:%M:%S")
      msg <- paste0("🧭 [", ts, "] ", sprintf("%-12s", paste0(tag, ":")))
      if (is.null(payload)) message(msg, " ok") else {
        kv <- tryCatch({
          paste(vapply(names(payload), function(k) paste0(k, "=", as.character(payload[[k]])), ""),
                collapse = " | ")
        }, error = function(e) "payload=<unprintable>")
        message(msg, " ", kv)
      }
      invisible(TRUE)
    }
  )
)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
