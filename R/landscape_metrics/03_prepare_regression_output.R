source('R/setup.R')


landscape_metrics_sf <- st_read("../../data/urbanformbr/landscape_metrics/landscape_metrics.gpkg") %>%
  filter(year == 2014)
cell_dist_metrics_df <- read_csv("../../data/urbanformbr/landscape_metrics/cell_distance_metrics_2014.csv")

combined_sf <- left_join(landscape_metrics_sf, cell_dist_metrics_df, by = c("city" = "name_uca_case") )


combined_sf <- combined_sf %>%
  select(code_muni, name_uca_case = city,
         n_patches, n_large_patches,
         proportion_largest_patch,
         ratio_circle, ratio_circle_large,
         avg_cell_distance = avg_cell_distance.y, avg_cell_distance_w_pop)
combined_df <- st_set_geometry(combined_sf, NULL)


st_write(combined_sf, "../../data/urbanformbr/landscape_metrics/fragmentation_compacity.gpkg")
write_csv(combined_df, "../../data/urbanformbr/landscape_metrics/fragmentation_compacity.csv")

