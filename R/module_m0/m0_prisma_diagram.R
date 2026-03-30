# ============================================================================
# m0_prisma_diagram.R - PRISMA 2020 Flow Diagram rendering with ggplot2
# ============================================================================

# Null-coalescing operator
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Render PRISMA 2020 flow diagram
#'
#' Creates a PRISMA flow diagram using ggplot2 with the standard 4-box layout.
#'
#' @param prisma_data A list with PRISMA flow counts.
#' @param config Configuration list.
#' @return A ggplot object.
#' @export
m0_render_prisma_diagram <- function(prisma_data, config = biblio_config()) {
  config <- merge_biblio_config(config)

  id <- prisma_data$identification %||% list()
  sc <- prisma_data$screening %||% list()
  el <- prisma_data$eligibility %||% list()
  inc <- prisma_data$included %||% list()

  # Extract counts
  db_records   <- id$records_database %||% 0
  other_records <- id$records_other %||% 0
  duplicates   <- id$duplicates_removed %||% 0
  screened     <- sc$records_screened %||% 0
  excl_screen  <- sc$excluded_screening %||% 0
  fulltext     <- el$fulltext_assessed %||% 0
  excl_full    <- el$excluded_fulltext %||% 0
  included     <- inc$studies_included %||% 0

  # Build exclusion reason labels
  excl_reasons <- ""
  if (!is.null(el$excluded_reasons)) {
    parts <- sapply(names(el$excluded_reasons), function(r) {
      paste0(r, ": ", el$excluded_reasons[[r]])
    })
    excl_reasons <- paste(parts, collapse = "\n")
  }

  # Box definitions: x, y, label, count
  boxes <- data.frame(
    x = c(0.25, 0.25, 0.25,
          0.25, 0.25,
          0.25, 0.85,
          0.25, 0.85,
          0.25),
    y = c(4.5, 4.0, 3.5,
          3.0, 2.5,
          2.0, 2.0,
          1.5, 1.5,
          1.0),
    label = c(
      "Records from\ndatabases",
      "Records from\nother sources",
      "Duplicates\nremoved",
      "Records\nscreened",
      "Excluded at\ntitle/abstract",
      "Full-text\nassessed",
      "Excluded at\nfull-text",
      "Studies included\nin review",
      "",
      ""
    ),
    count = c(
      db_records, other_records, duplicates,
      screened, excl_screen,
      fulltext, excl_full,
      included, 0, 0
    ),
    type = c("db", "other", "remove",
             "screen", "exclude",
             "eligibility", "exclude",
             "included", "reasons", "extra"),
    stringsAsFactors = FALSE
  )

  # Use base R graphics for a clean PRISMA diagram (ggplot2 is heavy for boxes)
  p <- m0_prisma_base_plot(prisma_data)

  p
}

#' Draw PRISMA diagram using base R graphics
#'
#' @param prisma_data PRISMA data list.
#' @return An environment with plot state (for export via recordPlot).
#' @keywords internal
m0_prisma_base_plot <- function(prisma_data) {
  id <- prisma_data$identification %||% list()
  sc <- prisma_data$screening %||% list()
  el <- prisma_data$eligibility %||% list()
  inc <- prisma_data$included %||% list()
  qa <- prisma_data$quality %||% NULL

  # Counts
  db_rec       <- id$records_database %||% 0
  other_rec    <- id$records_other %||% 0
  duplicates    <- id$duplicates_removed %||% 0
  # PRISMA 2020: total identified = db + other sources
  # After dedup: (db + other) - duplicates
  # Note: duplicates_removed should represent ALL duplicates (internal + cross-database)
  # If internal duplicates per source are tracked separately, use:
  # after_dedup = (db_rec - internal_db_dups) + (other_rec - internal_other_dups) - cross_db_dups
  after_dedup <- db_rec + other_rec - duplicates
  screened    <- sc$records_screened %||% 0
  excl_scr    <- sc$excluded_screening %||% 0
  fulltext    <- el$fulltext_assessed %||% 0
  excl_ft     <- el$excluded_fulltext %||% 0
  included    <- inc$studies_included %||% 0

  # Exclusion reasons text
  excl_text <- ""
  if (!is.null(el$excluded_reasons)) {
    parts <- sapply(names(el$excluded_reasons), function(r) {
      paste0(r, ": ", el$excluded_reasons[[r]])
    })
    excl_text <- paste(parts, collapse = "\n")
  }

  # Colors
  col_db       <- "#4A90D9"
  col_screen   <- "#7B68EE"
  col_elig     <- "#50C878"
  col_include  <- "#FF8C00"
  col_exclude  <- "#DC143C"
  col_remove   <- "#808080"

  # Create the plot
  grDevices::dev.new(width = 8, height = 12, noRStudioGD = TRUE)
  op <- par(mar = c(1, 1, 2, 1))

  plot(NULL, xlim = c(0, 10), ylim = c(0, 12),
       xlab = "", ylab = "", axes = FALSE, main = "PRISMA 2020 Flow Diagram")

  # Box drawing helper
  draw_box <- function(x, y, w, h, label, count, bg_col, text_col = "white") {
    rect(x - w/2, y - h/2, x + w/2, y + h/2,
         col = bg_col, border = "black", lwd = 1.5)
    txt <- if (count > 0) paste0(label, "\n(n = ", count, ")") else label
    text(x, y, txt, cex = 0.8, col = text_col, font = 2)
  }

  # Arrow helper
  draw_arrow <- function(x1, y1, x2, y2, label = "") {
    arrows(x1, y1, x2, y2, length = 0.1, lwd = 1.5)
    if (nzchar(label)) {
      mid_x <- (x1 + x2) / 2 + 2.5
      mid_y <- (y1 + y2) / 2
      text(mid_x, mid_y, label, cex = 0.7, col = col_exclude, font = 3)
    }
  }

  # === IDENTIFICATION ===
  draw_box(2.5, 11, 3, 0.8, "Records from databases", db_rec, col_db)
  draw_box(7.5, 11, 3, 0.8, "Records from other sources", other_rec, col_db)

  # Arrow down to dedup
  draw_arrow(2.5, 10.6, 2.5, 10.1)
  draw_arrow(7.5, 10.6, 5, 10.1)

  # Duplicates removed (right side box)
  draw_box(8, 10, 3, 0.7, "Duplicates removed", duplicates, col_remove)

  # Dedup box
  # Use after_dedup which is properly calculated as (db + other) - duplicates
  # Ensure non-negative (dedup should not exceed total)
  after_dedup <- max(0, after_dedup)
  draw_box(2.5, 10, 3, 0.7, "Records after dedup", after_dedup, col_db)

  draw_arrow(8, 9.65, 8, 9.2, "")

  # === SCREENING ===
  draw_arrow(2.5, 9.65, 2.5, 9.2)
  draw_box(2.5, 8.8, 3, 0.8, "Records screened\n(title/abstract)", screened, col_screen)

  # Excluded box (right)
  draw_box(8, 8.8, 3, 0.8, "Excluded at screening", excl_scr, col_exclude)
  draw_arrow(4, 8.8, 6.5, 8.8)

  # === ELIGIBILITY ===
  draw_arrow(2.5, 8.4, 2.5, 7.9)
  draw_box(2.5, 7.5, 3, 0.8, "Full-text articles\nassessed", fulltext, col_elig)

  # Excluded with reasons (right)
  excl_label <- paste0("Excluded at full-text\n(n = ", excl_ft, ")")
  if (nzchar(excl_text)) {
    excl_label <- paste0(excl_label, "\n\n", excl_text)
  }
  draw_box(8, 7.5, 3.5, 1.5, excl_label, 0, col_exclude)
  draw_arrow(4, 7.5, 6.25, 7.5)

  # Quality assessment (optional)
  if (!is.null(qa)) {
    draw_arrow(2.5, 7.1, 2.5, 6.6)
    qa_label <- paste0("Quality assessment\n",
                       "Low risk: ", qa$low_risk %||% "N/A", "\n",
                       "High risk: ", qa$high_risk %||% "N/A")
    draw_box(2.5, 6.2, 3, 1, qa_label, 0, col_screen)
    next_y <- 5.5
  } else {
    next_y <- 6.8
  }

  # === INCLUDED ===
  draw_arrow(2.5, next_y + 0.3, 2.5, next_y - 0.1)
  draw_box(2.5, next_y - 0.5, 3, 0.8, "Studies included\nin review", included, col_include)

  # Included by type (expand if available)
  if (!is.null(inc$by_type) && length(inc$by_type) > 0) {
    types <- names(inc$by_type)
    n_types <- length(types)
    spacing <- 3 / max(n_types, 1)
    start_x <- 2.5 - (n_types - 1) * spacing / 2

    for (i in seq_along(types)) {
      bx <- start_x + (i - 1) * spacing
      by <- next_y - 1.5
      draw_arrow(2.5, next_y - 0.9, bx, by + 0.3)
      draw_box(bx, by, 2, 0.6,
               paste0(types[i], "\n(n = ", inc$by_type[[i]], ")"),
               0, col_include)
    }
  }

  par(op)

  # Return recorded plot
  grDevices::recordPlot()
}
