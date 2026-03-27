# ============================================================================
# m3_render_similarity.R - Country profile/cluster plots for M3
# ============================================================================

#' Render country profile similarity plots
#'
#' Renders PCA biplot and cluster visualization when available.
#'
#' @param profiles_data Output from \code{m3_compute_country_profiles}
#' @param config A configuration list (see \code{biblio_config})
#' @return A list with \code{status} and \code{plots}
#' @export
m3_render_similarity <- function(profiles_data, config = biblio_config()) {
  stub <- list(status = "stub", plots = list())

  if (!is.list(profiles_data) || nrow(profiles_data$country_profiles) == 0) {
    return(stub)
  }

  profiles <- profiles_data$country_profiles
  plots    <- list()

  # PCA biplot (PC1 vs PC2) requires both columns to be present
  has_pc <- all(c("PC1", "PC2") %in% names(profiles))

  if (has_pc) {
    has_cluster <- "cluster" %in% names(profiles)

    p_pca <- ggplot2::ggplot(
      profiles,
      ggplot2::aes(x = PC1, y = PC2, label = country)
    )

    if (has_cluster) {
      p_pca <- p_pca +
        ggplot2::geom_point(ggplot2::aes(color = cluster), size = 2.0) +
        ggplot2::scale_color_manual(
          values = unlist(ieee_colors)[seq_len(
            dplyr::n_distinct(profiles$cluster)
          )],
          name = "Cluster"
        )
    } else {
      p_pca <- p_pca +
        ggplot2::geom_point(color = ieee_colors$blue, size = 2.0)
    }

    # Country labels – use ggrepel if available, fall back to geom_text
    p_pca <- tryCatch(
      p_pca + ggrepel::geom_text_repel(size = 2.0, max.overlaps = 20),
      error = function(e)
        p_pca + ggplot2::geom_text(vjust = -0.8, size = 1.8)
    )

    p_pca <- p_pca +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                          color = ieee_colors$ltgray, linewidth = 0.3) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                          color = ieee_colors$ltgray, linewidth = 0.3) +
      ggplot2::labs(
        title = "Country Profile: PCA Biplot (PC1 vs PC2)",
        x     = "PC1",
        y     = "PC2"
      ) +
      ieee_theme()

    plots$pca_biplot <- p_pca
  }

  # Cluster summary bar chart (cluster size)
  if ("cluster" %in% names(profiles)) {
    cluster_sizes <- profiles %>%
      dplyr::count(cluster) %>%
      dplyr::mutate(cluster = as.character(cluster))

    plots$cluster_sizes <- ggplot2::ggplot(
      cluster_sizes,
      ggplot2::aes(x = cluster, y = n, fill = cluster)
    ) +
      ggplot2::geom_col(color = "black", linewidth = 0.2, show.legend = FALSE) +
      ggplot2::scale_fill_manual(
        values = unlist(ieee_colors)[seq_len(nrow(cluster_sizes))]
      ) +
      ggplot2::labs(
        title = "Country Cluster Sizes",
        x     = "Cluster",
        y     = "Number of Countries"
      ) +
      ieee_theme()
  }

  if (length(plots) == 0) return(stub)
  list(status = "success", plots = plots)
}
