suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(cowplot)
  library(jsonlite)
  library(tidyr)
  library(stringr)
  library(igraph)
  library(geosphere)   # for distance Haversine
  library(text2vec)    # for thematic similarity (TF-IDF)
  library(broom)       # for model tidying
})

# ---------------- Normalizer for Country Arrays ----------------
normalize_country_array <- function(country_array_list) {
  lapply(country_array_list, function(countries) {
    if (is.null(countries) || all(is.na(countries))) return(NA_character_)
    countries <- trimws(toupper(countries))   # standardize
    countries <- gsub("\\s+", " ", countries) # clean double spaces
    
    # Map aliases → ISO-like names
    replacements <- c(
      "UK"        = "UNITED KINGDOM",
      "USA"       = "UNITED STATES",
      "UAE"       = "UNITED ARAB EMIRATES",
      "KOREA"     = "SOUTH KOREA",
      "IRAN"      = "IRAN",
      "HONG KONG" = "HONG KONG"
    )
    countries <- ifelse(countries %in% names(replacements), 
                        replacements[countries], countries)
    unique(countries)
  })
}

# ---------------- Class Definition ----------------
M4_M5_COUNTRY_META <- setRefClass(
  "M4_M5_COUNTRY_META",
  fields = list(
    df_papers    = "data.frame",  # main df with papers
    df_countries = "data.frame",  # metadata from CSV
    results      = "list",
    plots        = "list",
    font_ieee    = "character"    # font
  ),

  methods = list(

    # ---------- Initialization ----------
    initialize = function(df, clusters = NULL, country_meta) {
      message("[M4_M5] ==== INITIALIZATION ====")
      
      # Store
      .self$df_papers    <- as.data.frame(df)
      .self$df_countries <- as.data.frame(country_meta)
      .self$results      <- list()
      .self$plots        <- list()
      .self$font_ieee    <- "Times New Roman"

      # Expected columns
      required_cols <- c("CODE","LAT","LON","NAME","LANGUAGE","ISO3","CONTINENT","REGION")
      missing_cols  <- setdiff(required_cols, names(.self$df_countries))

      if (length(missing_cols) > 0) {
        warning("[M4_M5] country_meta is missing: ", paste(missing_cols, collapse = ", "))
        for (col in missing_cols) .self$df_countries[[col]] <- NA
      }

      # Normalize df_papers columns (important!)
      col_map <- c(
        "Year"               = "year",
        "Authors"            = "authors",
        "Keywords"           = "keywords",
        "Index Terms"        = "index_terms",
        "Author Affiliations"= "affiliations",
        "Source Title"       = "journal",
        "Language"           = "language",
        "Times Cited"        = "times_cited",
        "Title"              = "title",
        "Document Type"      = "doc_type",
        "Authors_Array"      = "authors_array",
        "Country_Array"      = "country_array"
      )

      # Renombrar SOLO si existe en df_papers
      for (old in names(col_map)) {
        if (old %in% names(.self$df_papers)) {
          names(.self$df_papers)[names(.self$df_papers) == old] <- col_map[[old]]
        }
      }

      # Filtrar solo las columnas que realmente existan en df_papers
      keep <- unique(unname(col_map))
      keep <- intersect(keep, names(.self$df_papers))
      .self$df_papers <- .self$df_papers[, keep, drop = FALSE]

      message("[M4_M5] df_papers columns normalized:")
      print(names(.self$df_papers))

      message("[M4_M5] df_countries_meta columns:")
      print(names(.self$df_countries))

      invisible(.self)
    },

    # ---------- Prepare Metadata ----------
    prepare_metadata = function() {
      message("[M4_M5] Preparing metadata...")

      if (!"country_array" %in% names(.self$df_papers)) {
        print("❌ ERROR: Missing 'country_array'")
        print(names(.self$df_papers))
        stop("[M4_M5] df_papers is missing 'country_array'")
      }

      # Expand papers → countries
      countries_expanded <- do.call(rbind, lapply(seq_len(nrow(.self$df_papers)), function(i) {
        if (is.null(.self$df_papers$country_array[[i]])) return(NULL)
        data.frame(
          paper_id = i,
          NAME     = toupper(trimws(unlist(strsplit(.self$df_papers$country_array[[i]], ";")))),
          stringsAsFactors = FALSE
        )
      }))

      # Merge with metadata (by NAME ↔ NAME)
      merged <- merge(
        countries_expanded,
        transform(.self$df_countries, NAME = toupper(trimws(NAME))),
        by = "NAME", all.x = TRUE
      )

      missing_meta <- sum(is.na(merged$CONTINENT))
      if (missing_meta > 0) {
        warning(sprintf("[M4_M5] %d countries could not be matched.", missing_meta))
      }

      .self$results$merged_meta <- merged
      message("[M4_M5] Metadata merged, nrow = ", nrow(merged))
      print(head(merged, 10))

      invisible(.self)
    },

    # ---------- EDA ----------
    run_eda = function() {
      message("[M4_M5] Running exploratory summaries...")

      if (is.null(.self$results$merged_meta)) {
        stop("[M4_M5] Run prepare_metadata() first")
      }

      df <- .self$results$merged_meta

      # Summaries
      papers_per_country   <- df %>% count(NAME, sort = TRUE)
      papers_per_continent <- df %>% count(CONTINENT, sort = TRUE)
      papers_per_language  <- df %>% count(LANGUAGE, sort = TRUE)

      # Time trends
      df_time <- merge(df, data.frame(paper_id = seq_len(nrow(.self$df_papers)),
                                      year = .self$df_papers$year), by = "paper_id", all.x = TRUE)

      yearly_by_continent <- df_time %>% count(year, CONTINENT)
      yearly_by_language  <- df_time %>% count(year, LANGUAGE)

      .self$results$eda <- list(
        papers_per_country   = papers_per_country,
        papers_per_continent = papers_per_continent,
        papers_per_language  = papers_per_language,
        yearly_by_continent  = yearly_by_continent,
        yearly_by_language   = yearly_by_language
      )

      message("[M4_M5] Top countries:")
      print(head(papers_per_country, 10))
      invisible(.self)
    },
plot_eda = function(out_dir = "results/M4_M5", dpi = 600) {
  message("[M4_M5] Plotting EDA (per continent, IEEE style)...")

  if (is.null(.self$results$merged_meta)) stop("[M4_M5] Run prepare_metadata() first")

  df <- .self$results$merged_meta

  # ---- Papers per continent (ordered biggest→smallest) ----
  continent_counts <- df %>%
    filter(!is.na(CONTINENT)) %>%
    count(CONTINENT, name="papers") %>%
    arrange(desc(papers))

  p1 <- ggplot(continent_counts, aes(x = reorder(CONTINENT, -papers), y = papers)) +
    geom_col(fill="black", width=0.7) +
    labs(x = NULL, y = "Number of Papers") +
    theme_minimal(base_family="Times New Roman") +
    theme(
      plot.title = element_text(family="Times New Roman", face="bold", size=9, hjust=0.5),
      axis.text.x = element_text(family="Times New Roman", size=8, angle=0, vjust=0.8),
      axis.text.y = element_text(family="Times New Roman", size=8),
      axis.title.y = element_text(family="Times New Roman", size=8),
      panel.grid.major = element_line(size=0.25, color="gray70"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color="black", fill=NA, linewidth=0.4),
      plot.margin = margin(5,5,5,5)
    )
# ---- Papers per language (ordered biggest→smallest) ----
# --- Expand LANGUAGE into individual entries ---
expand_languages <- function(df) {
  df %>%
    filter(!is.na(LANGUAGE)) %>%
    rowwise() %>%
    mutate(LANG_SPLIT = strsplit(LANGUAGE, "[/+,]")) %>%  # split by "/" or "+"
    unnest(LANG_SPLIT) %>%
    mutate(LANG_SPLIT = trimws(LANG_SPLIT)) %>%
    filter(LANG_SPLIT != "", !grepl("^[0-9]+$", LANG_SPLIT)) %>% # drop empty and "+9" parts
    ungroup()
}
# ---- Papers per language (Top 10) ----
df_lang_expanded <- expand_languages(df)

language_counts <- df_lang_expanded %>%
  count(LANG_SPLIT, name="papers") %>%
  arrange(desc(papers)) %>%
  head(10)

p2 <- ggplot(language_counts, aes(x = reorder(LANG_SPLIT, papers), y = papers)) +
  geom_col(fill="grey40", color="black", width=0.7) +
  coord_flip() +
  labs(x = NULL, y = "Number of Papers",
       title = "Top 10 Languages in Publications") +
  theme_minimal(base_family="serif") +
  theme(
    plot.title   = element_text(family="serif", face="bold", size=8, hjust=0.5),
    axis.text.x  = element_text(family="serif", size=7),
    axis.text.y  = element_text(family="serif", size=7),
    axis.title.y = element_text(family="serif", size=7),
    axis.title.x = element_text(family="serif", size=7),
    panel.grid.major = element_line(size=0.25, color="gray70"),
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(color="black", fill=NA, linewidth=0.4),
    plot.margin      = margin(5,5,5,5)
  )


# ---- Save IEEE single-column plots ----
if (!dir.exists(out_dir)) dir.create(out_dir, recursive=TRUE)

# Save p1 (per continent)
out_path1 <- file.path(out_dir, "M4_M5_per_continent.png")
ggsave(out_path1, plot=p1, width=8.8/2.54, height=6.5/2.54, dpi=dpi)
message("[M4_M5] Saved IEEE-ready plot: ", out_path1)

# Save p2 (per language)
# Save with IEEE single-column dimensions
out_path2 <- file.path(out_dir, "M4_M5_top_languages.png")
ggsave(out_path2, plot=p2, width=8.8/2.54, height=6.5/2.54, dpi=dpi)
message("[M4_M5] Saved IEEE-ready plot: ", out_path2)

# Store both plots in object
.self$plots$eda <- list(
  per_continent = p1,
  per_language  = p2
)

invisible(.self)
},

    # ---------- Plot EDA ----------
plot_trends = function(out_dir = "results/M4_M5", dpi = 600) {
  message("[M4_M5] Plotting trend comparison (last 20 years)...")

  if (is.null(.self$results$merged_meta)) stop("[M4_M5] Run prepare_metadata() first")
  if (!"year" %in% names(.self$df_papers)) stop("[M4_M5] df_papers is missing 'year'")

  df <- merge(.self$results$merged_meta,
              data.frame(paper_id = seq_len(nrow(.self$df_papers)),
                         year     = .self$df_papers$year),
              by = "paper_id", all.x = TRUE)

  current_year <- max(df$year, na.rm=TRUE)

  # Define periods
  df <- df %>%
    mutate(period = case_when(
      year >  current_year - 10             ~ "Now-10 to Now",
      year >  current_year - 20 & year <= current_year - 10 ~ "Now-20 to Now-10",
      TRUE                                  ~ "Earlier"
    ))

  # Aggregate counts
  trend_summary <- df %>%
    filter(!is.na(CONTINENT)) %>%
    count(period, CONTINENT, name="papers")

  # Normalize order
  trend_summary$period <- factor(trend_summary$period,
                                 levels=c("Earlier","Now-20 to Now-10","Now-10 to Now"))

  # Plot stacked bar: continents per period
  p_trend <- ggplot(trend_summary, aes(x=period, y=papers, fill=CONTINENT)) +
    geom_col(position="stack", width=0.7) +
    labs(x=NULL, y="Number of Papers", title="Continental Trends in Publications") +
    theme_minimal(base_family="Times New Roman") +
    theme(
      plot.title = element_text(family="Times New Roman", face="bold", size=9, hjust=0.5),
      axis.text.x = element_text(family="Times New Roman", size=8),
      axis.text.y = element_text(family="Times New Roman", size=8),
      axis.title.y = element_text(family="Times New Roman", size=8),
      legend.title = element_blank(),
      legend.text  = element_text(family="Times New Roman", size=7),
      panel.grid.major = element_line(size=0.25, color="gray70"),
      panel.border = element_rect(color="black", fill=NA, linewidth=0.4)
    )

  # Save
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive=TRUE)
  out_path <- file.path(out_dir, "M4_M5_trends.png")
  ggsave(out_path, plot=p_trend, width=8.8/2.54, height=6.5/2.54, dpi=dpi)

  .self$plots$trends <- p_trend
  message("[M4_M5] Saved trend plot: ", out_path)
  invisible(.self)
}
,

plot_trends_ieee = function(out_dir = "results/M4_M5", dpi = 600) {
  message("[M4_M5] Plotting IEEE-style trend comparison...")

  if (is.null(.self$results$merged_meta)) stop("[M4_M5] Run prepare_metadata() first")
  if (!"year" %in% names(.self$df_papers)) stop("[M4_M5] df_papers is missing 'year'")

  # Merge metadata with year info
  df <- merge(.self$results$merged_meta,
              data.frame(paper_id = seq_len(nrow(.self$df_papers)),
                         year     = .self$df_papers$year),
              by = "paper_id", all.x = TRUE)

  current_year <- max(df$year, na.rm=TRUE)

  # Define 2 exact bins: last 10 years and previous 10 years
  df <- df %>%
    mutate(period = case_when(
      year >= current_year - 9 & year <= current_year ~
        paste0(current_year - 9, "-", current_year, " "),
      year >= current_year - 19 & year <= current_year - 10 ~
        paste0(current_year - 19, "-", current_year - 10, " "),
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(period))

  # --- Absolute counts ---
  trend_summary <- df %>%
    filter(!is.na(CONTINENT)) %>%
    count(period, CONTINENT, name="papers")

  trend_summary$period <- factor(trend_summary$period,
                                 levels=c(
                                   paste0(current_year - 19, "-", current_year - 10, " "),
                                   paste0(current_year - 9, "-", current_year, " ")
                                 ))

  p_abs <- ggplot(trend_summary, aes(x=period, y=papers, fill=CONTINENT)) +
    geom_col(position="stack", width=0.65, color="black", linewidth=0.2) +
    labs(x=NULL, y="Number of Papers",
         title="Continental Trends in Publications (Absolute)") +
    scale_fill_grey(start=0.2, end=0.8) +
    theme_minimal(base_family="serif") +
    theme(
      plot.title   = element_text(family="serif", face="bold", size=8, hjust=0.5),
      axis.text.x  = element_text(family="serif", size=7),
      axis.text.y  = element_text(family="serif", size=7),
      axis.title.y = element_text(family="serif", size=7),
      legend.title = element_blank(),
      legend.text  = element_text(family="serif", size=6),
      legend.position = "right",
      panel.grid.major = element_line(size=0.25, color="gray70"),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(color="black", fill=NA, linewidth=0.4)
    )

  # --- Relative shares ---
  trend_share <- trend_summary %>%
    group_by(period) %>%
    mutate(share = papers / sum(papers))

  p_share <- ggplot(trend_share, aes(x=period, y=share, fill=CONTINENT)) +
    geom_col(position="stack", width=0.65, color="black", linewidth=0.2) +
    labs(x=NULL, y="Share of Papers (%)",
         title="Continental Share of Publications") +
    scale_y_continuous(labels=scales::percent) +
    scale_fill_grey(start=0.2, end=0.8) +
    theme_minimal(base_family="serif") +
    theme(
      plot.title   = element_text(family="serif", face="bold", size=8, hjust=0.5),
      axis.text.x  = element_text(family="serif", size=7),
      axis.text.y  = element_text(family="serif", size=7),
      axis.title.y = element_text(family="serif", size=7),
      legend.title = element_blank(),
      legend.text  = element_text(family="serif", size=6),
      legend.position = "right",
      panel.grid.major = element_line(size=0.25, color="gray70"),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(color="black", fill=NA, linewidth=0.4)
    )

  # Save both plots
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive=TRUE)
  ggsave(file.path(out_dir,"M4_M5_trends_absolute.png"), p_abs,
         width=8.8/2.54, height=6.5/2.54, dpi=dpi)
  ggsave(file.path(out_dir,"M4_M5_trends_share.png"), p_share,
         width=8.8/2.54, height=6.5/2.54, dpi=dpi)

  .self$plots$trends_ieee <- list(absolute=p_abs, share=p_share)
  message("[M4_M5] Saved IEEE-style trend plots (absolute + share) at: ", out_dir)
  invisible(.self)
}


,

plot_trend_change_ieee = function(out_dir = "results/M4_M5", dpi = 600) {
  message("[M4_M5] Plotting IEEE-style trend change (% growth)...")

  if (is.null(.self$results$merged_meta)) stop("[M4_M5] Run prepare_metadata() first")
  if (!"year" %in% names(.self$df_papers)) stop("[M4_M5] df_papers is missing 'year'")

  # Merge year info
  df <- merge(.self$results$merged_meta,
              data.frame(paper_id = seq_len(nrow(.self$df_papers)),
                         year = .self$df_papers$year),
              by = "paper_id", all.x = TRUE)

  current_year <- max(df$year, na.rm=TRUE)

  # Define periods: last 10y vs previous 10y
  df <- df %>%
    mutate(period = case_when(
      year >= current_year - 9 & year <= current_year ~ "Last 10y",
      year >= current_year - 19 & year <= current_year - 10 ~ "Prev 10y",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(period))

  # Aggregate
  summary <- df %>%
    filter(!is.na(CONTINENT)) %>%
    count(period, CONTINENT, name="papers") %>%
    tidyr::pivot_wider(names_from=period, values_from=papers, values_fill=0)

  # Compute % change
  summary <- summary %>%
    mutate(perc_change = ifelse(`Prev 10y` > 0,
                                (`Last 10y` - `Prev 10y`) / `Prev 10y` * 100,
                                NA))

  # Plot
  p <- ggplot(summary, aes(x=reorder(CONTINENT, perc_change), y=perc_change)) +
    geom_col(fill="grey40", color="black", width=0.65) +
    coord_flip() +
    geom_hline(yintercept=0, linetype="dashed", color="black", linewidth=0.3) +
    labs(x=NULL, y="% Change in Papers",
         title="Continental Growth in Publications (Last 20 Years)") +
    theme_minimal(base_family="serif") +
    theme(
      plot.title   = element_text(family="serif", face="bold", size=8, hjust=0.5),
      axis.text.x  = element_text(family="serif", size=7),
      axis.text.y  = element_text(family="serif", size=7),
      axis.title.y = element_text(family="serif", size=7),
      axis.title.x = element_text(family="serif", size=7),
      panel.grid.major = element_line(size=0.25, color="gray70"),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(color="black", fill=NA, linewidth=0.4),
      plot.margin      = margin(5,5,5,5)
    )

  # Save
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive=TRUE)
  out_path <- file.path(out_dir,"M4_M5_trend_change.png")
  ggsave(out_path, p, width=8.8/2.54, height=6.5/2.54, dpi=dpi)

  .self$plots$trend_change <- p
  message("[M4_M5] Saved IEEE-style trend-change plot: ", out_path)
  invisible(.self)
}
,
plot_trend_change_ieee_languages = function(out_dir = "results/M4_M5", dpi = 600) {
  message("[M4_M5] Plotting IEEE-style trend change (% growth) for languages...")

  if (is.null(.self$results$merged_meta)) stop("[M4_M5] Run prepare_metadata() first")
  if (!"year" %in% names(.self$df_papers)) stop("[M4_M5] df_papers is missing 'year'")

  # Merge year info
  df <- merge(.self$results$merged_meta,
              data.frame(paper_id = seq_len(nrow(.self$df_papers)),
                         year = .self$df_papers$year),
              by = "paper_id", all.x = TRUE)

  current_year <- max(df$year, na.rm=TRUE)

  # Expand multi-language entries into separate rows
  df_lang <- df %>%
    filter(!is.na(LANGUAGE)) %>%
    mutate(LANGUAGE = gsub("\\+\\d+", "", LANGUAGE)) %>%       # remove "+9" style suffix
    mutate(LANGUAGE = strsplit(as.character(LANGUAGE), "/")) %>%
    tidyr::unnest(LANGUAGE) %>%
    mutate(LANGUAGE = trimws(LANGUAGE))

  # Define periods: last 10y vs previous 10y
  df_lang <- df_lang %>%
    mutate(period = case_when(
      year >= current_year - 9 & year <= current_year ~ "Last 10y",
      year >= current_year - 19 & year <= current_year - 10 ~ "Prev 10y",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(period))

  # Aggregate by language and period
  summary <- df_lang %>%
    count(period, LANGUAGE, name="papers") %>%
    tidyr::pivot_wider(names_from=period, values_from=papers, values_fill=0)

  # Compute % change
  summary <- summary %>%
    mutate(perc_change = ifelse(`Prev 10y` > 0,
                                (`Last 10y` - `Prev 10y`) / `Prev 10y` * 100,
                                NA))

  # Keep only top 15 by absolute change to avoid clutter
  summary <- summary %>%
    arrange(desc(abs(perc_change))) %>%
    head(15)

  # Plot
  p <- ggplot(summary, aes(x=reorder(LANGUAGE, perc_change), y=perc_change)) +
    geom_col(fill="grey40", color="black", width=0.65) +
    coord_flip() +
    geom_hline(yintercept=0, linetype="dashed", color="black", linewidth=0.3) +
    labs(x=NULL, y="% Change in Papers",
         title="Language Growth in Publications (Last 20 Years)") +
    theme_minimal(base_family="serif") +
    theme(
      plot.title   = element_text(family="serif", face="bold", size=8, hjust=0.5),
      axis.text.x  = element_text(family="serif", size=7),
      axis.text.y  = element_text(family="serif", size=7),
      axis.title.y = element_text(family="serif", size=7),
      axis.title.x = element_text(family="serif", size=7),
      panel.grid.major = element_line(size=0.25, color="gray70"),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(color="black", fill=NA, linewidth=0.4),
      plot.margin      = margin(5,5,5,5)
    )

  # Save
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive=TRUE)
  out_path <- file.path(out_dir,"M4_M5_trend_change_languages.png")
  ggsave(out_path, p, width=8.8/2.54, height=6.5/2.54, dpi=dpi)

  .self$plots$trend_change_languages <- p
  message("[M4_M5] Saved IEEE-style language trend-change plot: ", out_path)
  invisible(.self)
}
,


  analyze_cooperation_factors = function(out_dir = "results/M4_M5", dpi = 600) {
  message("[M4_M5] Starting cooperation factor analysis...")
  if (is.null(.self$results$merged_meta)) stop("[M4_M5] Run prepare_metadata() first")
  if (!"year" %in% names(.self$df_papers)) stop("[M4_M5] df_papers is missing 'year'")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  # 1. Build MCP flag
  papers_expanded <- .self$df_papers %>%
    mutate(paper_id = row_number()) %>%
    tidyr::separate_rows(country_array, sep=";") %>%
    mutate(country_array = trimws(toupper(country_array)))

  paper_mcp <- papers_expanded %>%
    group_by(paper_id) %>%
    summarise(
      countries = list(unique(country_array)),
      MCP = as.integer(length(unique(country_array)) > 1),
      .groups="drop"
    )

  # 2. Country-country edges
  edges <- do.call(rbind, lapply(1:nrow(paper_mcp), function(i) {
    cc <- paper_mcp$countries[[i]]
    if (length(cc) < 2) return(NULL)
    expand.grid(from=cc, to=cc, stringsAsFactors = FALSE) %>%
      filter(from < to) %>%
      mutate(paper_id = paper_mcp$paper_id[i])
  }))

  edges_summary <- edges %>% count(from, to, name="co_pubs")

  # 3. Merge with metadata
  meta <- .self$df_countries %>% mutate(NAME = toupper(NAME))
  edges_meta <- edges_summary %>%
    left_join(meta, by=c("from"="NAME")) %>%
    rename(lat_from = LAT, lon_from = LON, lang_from = LANGUAGE) %>%
    left_join(meta, by=c("to"="NAME")) %>%
    rename(lat_to = LAT, lon_to = LON, lang_to = LANGUAGE)

  # (a) Geographic proximity
  edges_meta <- edges_meta %>%
    rowwise() %>%
    mutate(distance_km = geosphere::distHaversine(c(lon_from, lat_from),
                                                  c(lon_to, lat_to))/1000) %>%
    ungroup()

  cor_geo <- suppressWarnings(cor.test(edges_meta$distance_km, edges_meta$co_pubs, method="spearman"))
  message(sprintf("[Geo] Spearman rho = %.3f, p=%.3g", cor_geo$estimate, cor_geo$p.value))

  # (b) Shared language
  expand_langs <- function(x) {
    x <- gsub("\\+\\d+", "", x)  # remove "+9" suffixes
    unlist(strsplit(x, "[/+]")) %>% trimws()
  }
  edges_meta <- edges_meta %>%
    rowwise() %>%
    mutate(shared_lang = length(intersect(expand_langs(lang_from), expand_langs(lang_to))) > 0) %>%
    ungroup()
  chi_lang <- suppressWarnings(chisq.test(table(edges_meta$shared_lang, edges_meta$co_pubs > 0)))
  message(sprintf("[Lang] Chi-square test p=%.3g", chi_lang$p.value))

  # (c) Thematic similarity (keywords+title)
  docs <- .self$df_papers %>%
    mutate(doc = paste(title, keywords, sep=" ")) %>%
    tidyr::separate_rows(country_array, sep=";") %>%
    group_by(country_array) %>%
    summarise(text = paste(doc, collapse=" "), .groups="drop")

  if (nrow(docs) > 1) {
    it <- text2vec::itoken(docs$text, progressbar=FALSE)
    vectorizer <- text2vec::vocab_vectorizer(text2vec::create_vocabulary(it))
    dtm <- text2vec::create_dtm(it, vectorizer)
    tfidf <- text2vec::TfIdf$new()$fit_transform(dtm)

    # Ensure rownames = country names
    rownames(tfidf) <- docs$country_array
    sim_mat <- as.matrix(text2vec::sim2(tfidf, method="cosine", norm="l2"))

    edges_meta <- edges_meta %>%
        rowwise() %>%
        mutate(thematic_sim = ifelse(
          from %in% rownames(sim_mat) & to %in% colnames(sim_mat),
          sim_mat[from, to], 
          NA_real_
        )) %>%
        ungroup()

    cor_them <- suppressWarnings(cor.test(edges_meta$thematic_sim, edges_meta$co_pubs, method="spearman"))
    message(sprintf("[Thematic] Spearman rho = %.3f, p=%.3g", cor_them$estimate, cor_them$p.value))
  } else {
    edges_meta$thematic_sim <- NA
    cor_them <- NULL
    message("[Thematic] Not enough documents to compute similarity")
  }

  # Combined regression
  model <- tryCatch(
    lm(co_pubs ~ distance_km + shared_lang + thematic_sim, data=edges_meta),
    error = function(e) { message("[Combined Model] Error: ", e$message); NULL }
  )

  if (!is.null(model)) {
    message("[Combined Model] Coefficients:")
    print(summary(model)$coefficients)
  }

  # Store results
# Store results (clean summaries, not full htest objects)
.self$results$coop_analysis <- list(
  edges = edges_meta,
  cor_geo = if (!is.null(cor_geo)) {
    list(
      estimate = unname(cor_geo$estimate),
      p.value  = cor_geo$p.value,
      method   = cor_geo$method
    )
  } else NULL,
  chi_lang = if (!is.null(chi_lang)) {
    list(
      statistic = unname(chi_lang$statistic),
      p.value   = chi_lang$p.value,
      method    = chi_lang$method
    )
  } else NULL,
  cor_them = if (!is.null(cor_them)) {
    list(
      estimate = unname(cor_them$estimate),
      p.value  = cor_them$p.value,
      method   = cor_them$method
    )
  } else NULL,
  model = if (!is.null(model)) {
    broom::tidy(model)   # requires library(broom)
  } else NULL
)


  invisible(.self)
}

,
plot_thematic_vs_coop <- function(out_dir = "results/M4_M5", dpi = 600) {
  message("[M4_M5] Plotting IEEE-style scatter: thematic similarity vs cooperation...")

  if (is.null(.self$results$coop_analysis)) 
    stop("[M4_M5] Run analyze_cooperation_factors() first")

  edges <- .self$results$coop_analysis$edges %>%
    filter(!is.na(thematic_sim), !is.na(co_pubs))

  if (nrow(edges) == 0) {
    warning("[M4_M5] No valid edges for plotting thematic similarity vs cooperation.")
    return(invisible(.self))
  }

  # Extract Spearman correlation
  cor_val <- .self$results$coop_analysis$cor_them$estimate
  cor_p   <- .self$results$coop_analysis$cor_them$p.value
  cor_label <- sprintf("Spearman rho = %.2f (p=%.1e)", cor_val, cor_p)

  # Base plot
  p <- ggplot(edges, aes(x = thematic_sim, y = co_pubs)) +
    geom_point(shape = 21, color = "black", fill = "grey60", size = 1.8, alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.4, linetype = "solid") +
    geom_text(aes(x = max(thematic_sim, na.rm=TRUE) * 0.7,
                  y = max(co_pubs, na.rm=TRUE) * 0.95,
                  label = cor_label),
              hjust = 0, vjust = 1, size = 2.5, family = "serif") +
    labs(x = "Thematic Similarity (cosine, TF-IDF)", 
         y = "Co-publications between Countries",
         title = "Thematic Similarity vs Cooperation Intensity") +
    theme_minimal(base_family = "serif") +
    theme(
      plot.title   = element_text(family="serif", face="bold", size=8, hjust=0.5),
      axis.text.x  = element_text(family="serif", size=7),
      axis.text.y  = element_text(family="serif", size=7),
      axis.title.x = element_text(family="serif", size=7),
      axis.title.y = element_text(family="serif", size=7),
      panel.grid.major = element_line(size=0.25, color="gray70"),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(color="black", fill=NA, linewidth=0.4),
      plot.margin      = margin(5,5,5,5)
    )

  # Save outputs
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive=TRUE)

  # IEEE single-column (8.8 cm wide)
  ggsave(file.path(out_dir,"M4_M5_thematic_vs_coop_single.png"), p,
         width=8.8/2.54, height=6.5/2.54, dpi=dpi)
  ggsave(file.path(out_dir,"M4_M5_thematic_vs_coop_single.svg"), p,
         width=8.8/2.54, height=6.5/2.54, dpi=dpi)

  # IEEE double-column (18.1 cm wide)
  ggsave(file.path(out_dir,"M4_M5_thematic_vs_coop_double.png"), p,
         width=18.1/2.54, height=6.5/2.54, dpi=dpi)
  ggsave(file.path(out_dir,"M4_M5_thematic_vs_coop_double.svg"), p,
         width=18.1/2.54, height=6.5/2.54, dpi=dpi)

  .self$plots$thematic_vs_coop <- p
  message("[M4_M5] Saved IEEE-style thematic vs cooperation plots at: ", out_dir)
  invisible(.self)
}


,
        # ---------- Save plot ----------
    save_plot = function(out_dir,
                         filename_base = "M4_M5_EDA",
                         width_cm = 18, height_cm = 12, dpi = 600) {
      if (is.null(.self$plots$eda)) stop("[M4_M5] run_eda() and plot_eda() first")
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
      w_in <- width_cm / 2.54
      h_in <- height_cm / 2.54
      ggsave(file.path(out_dir, paste0(filename_base,".png")), plot=.self$plots$eda, width=w_in, height=h_in, dpi=dpi)
      ggsave(file.path(out_dir, paste0(filename_base,".svg")), plot=.self$plots$eda, width=w_in, height=h_in, device="svg", dpi=dpi)
      ggsave(file.path(out_dir, paste0(filename_base,".eps")), plot=.self$plots$eda, width=w_in, height=h_in, device=cairo_ps, dpi=dpi)
      message("[M4_M5] Saved plots at: ", out_dir)
      invisible(.self)
    },

        # ---------- Save JSON ----------
    save_json = function(out_dir,
                         filename = "M4_M5_COUNTRY_META.json") {
      if (is.null(.self$results)) stop("[M4_M5] No results to save. Run run_eda() or prepare_metadata() first.")
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
      
      out_path <- file.path(out_dir, filename)
      writeLines(
        jsonlite::toJSON(.self$results, auto_unbox = TRUE, pretty = TRUE),
        con = out_path
      )
      message("[M4_M5] Saved JSON at: ", out_path)
      invisible(.self)
    }

  )
)





