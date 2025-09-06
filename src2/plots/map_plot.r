# src/plots/map_plot.r
map_plot <- function(df, metric = "TP", title=NULL) {
  world <- rnaturalearth::ne_countries(scale="medium", returnclass="sf")
  df2 <- df %>% rename(iso_a3 = country)  # asegúrate de tener ISO3
  joined <- left_join(world, df2, by="iso_a3")

  ggplot(joined) +
    geom_sf(aes(fill=.data[[metric]]), color=NA) +
    scale_fill_viridis_c(option="C") +
    labs(fill=metric, title=title) +
    theme_void()
}
