# ============================================================================
# m3_table_advanced_journal.R - Advanced M3 journal tables
# ============================================================================

m3_table_advanced_journal <- function(result, config = biblio_config()) {
  if (!is.list(result)) {
    return(list(status = "stub", table = tibble::tibble(), tables = list()))
  }

  tables <- list(
    collaboration_premium = result$collaboration_premium$table %||% m3_empty_collaboration_premium_table(),
    collaboration_premium_bootstrap = result$collaboration_premium$bootstrap %||% tibble::tibble(),
    rank_mobility = result$mobility$rank_windows %||% m3_empty_rank_mobility_table(),
    rank_volatility = result$mobility$rank_volatility %||% tibble::tibble(),
    leadership_persistence = result$mobility$leadership_persistence %||% tibble::tibble(),
    rank_stability = result$mobility$rank_stability %||% tibble::tibble(),
    country_trajectories = result$trajectories$table %||% m3_empty_country_trajectory_table(),
    geo_concentration = result$geo_concentration$table %||% m3_empty_geo_concentration_table(),
    lorenz_curve = result$geo_concentration$lorenz %||% tibble::tibble(),
    regional_decomposition = result$regional_decomposition$table %||% m3_empty_regional_decomposition_table(),
    regional_year_share = result$regional_decomposition$region_year %||% tibble::tibble(),
    normalization_optional = result$normalization_optional$table %||% tibble::tibble(),
    country_advanced_metrics = result$country_metrics$country_table %||% tibble::tibble(),
    country_year_metrics = result$country_metrics$country_year %||% tibble::tibble(),
    country_contribution_decomposition = result$country_metrics$contribution %||% tibble::tibble(),
    country_collaboration_premium = result$country_metrics$country_premium %||% tibble::tibble(),
    country_windowed_mcp = result$country_metrics$windowed_mcp %||% tibble::tibble(),
    world_map_metrics = result$country_metrics$world_map_metrics %||% tibble::tibble(),
    collaboration_network_nodes = result$collaboration_network$nodes %||% tibble::tibble(),
    collaboration_network_edges = result$collaboration_network$edges %||% tibble::tibble(),
    scp_mcp_trend = result$scp_mcp_trends$annual %||% tibble::tibble(),
    scp_mcp_trend_model = result$scp_mcp_trends$trend %||% tibble::tibble(),
    uncertainty_intervals = result$uncertainty$intervals %||% tibble::tibble(),
    share_change_bootstrap = result$uncertainty$share_change_bootstrap %||% tibble::tibble(),
    dominance_bootstrap = result$uncertainty$dominance_bootstrap %||% tibble::tibble(),
    robustness_rank_sensitivity = result$robustness$rank_sensitivity %||% tibble::tibble(),
    robustness_topn_sensitivity = result$robustness$topn_sensitivity %||% tibble::tibble(),
    quadrant_robustness = result$robustness$quadrant_robustness %||% tibble::tibble(),
    inequality_window_metrics = result$inequality_decomposition$window_metrics %||% tibble::tibble(),
    inequality_year_metrics = result$inequality_decomposition$year_metrics %||% tibble::tibble(),
    regional_concentration = result$inequality_decomposition$regional_concentration %||% tibble::tibble(),
    regional_inequality_contribution = result$inequality_decomposition$regional_inequality_contribution %||% tibble::tibble(),
    country_trend_models = result$trend_models$table %||% tibble::tibble(),
    geographic_outliers = result$outliers$table %||% tibble::tibble(),
    spatial_morans_i = result$spatial_autocorrelation$global %||% tibble::tibble(),
    spatial_lisa_clusters = result$spatial_autocorrelation$lisa %||% tibble::tibble(),
    collaboration_backbone_nodes = result$collaboration_backbone$nodes %||% tibble::tibble(),
    collaboration_backbone_edges = result$collaboration_backbone$edges %||% tibble::tibble(),
    collaboration_backbone_communities = result$collaboration_backbone$communities %||% tibble::tibble(),
    gravity_model_table = result$gravity_model$table %||% tibble::tibble(),
    gravity_model_coefficients = result$gravity_model$coefficients %||% tibble::tibble(),
    country_role_labels = result$country_roles$table %||% tibble::tibble(),
    hypotheses = result$hypotheses$table %||% tibble::tibble()
  )

  main_table <- dplyr::bind_rows(lapply(names(tables), function(nm) {
    tbl <- tables[[nm]]
    tibble::tibble(
      table_name = nm,
      rows = if (is.data.frame(tbl)) nrow(tbl) else 0L,
      status = if (is.data.frame(tbl) && nrow(tbl) > 0) "available" else "empty"
    )
  }))

  list(
    status = result$status %||% "success",
    reason = result$reason %||% NA_character_,
    table = main_table,
    tables = tables
  )
}
