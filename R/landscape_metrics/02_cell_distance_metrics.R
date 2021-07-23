source('R/setup.R')

grid_uca <- read_rds("../../data/urbanformbr/ghsl/results/grid_uca_2014_cutoff20.rds")

data <- grid_uca
muni <- 3550308 # São Paulo

calculate_metrics <- function(muni, data) {
  muni_sf <- filter(data, code_muni == muni)
  pop_df <- muni_sf %>% st_set_geometry(NULL) %>% setDT()

  ## Distance matrix
  cells_centroids_sf <- sf::st_centroid(muni_sf) %>% sf::st_transform(4326)

  distance_matrix <- geodist::geodist(cells_centroids_sf %>% sf::st_coordinates(), measure = "geodesic")
  distance_matrix <- distance_matrix / 1000
  colnames(distance_matrix) <- cells_centroids_sf$cell

  distance_matrix <- as.data.frame(distance_matrix)
  distance_matrix$from_cell <- cells_centroids_sf$cell
  distance_matrix <- pivot_longer(distance_matrix, !from_cell,
                                  names_to = "to_cell", values_to = "distance") %>%
    mutate(to_cell = as.numeric(to_cell))
  setDT(distance_matrix)

  avg_distances <- distance_matrix[from_cell != to_cell, .(avg_distance = mean(distance)), by = from_cell]
  avg_distances[pop_df, on = .(from_cell = cell), pop := i.pop]

  avg_cell_distance <- mean(avg_distances$avg_distance)
  avg_cell_distance_w <- weighted.mean(avg_distances$avg_distance, avg_distances$pop)

  pop_df <- pop_df[, .(built = sum(built, na.rm = T),
                       pop = sum(pop, na.rm = T)),
                   by = .(code_muni, name_uca_case)]


  pop_df$avg_cell_distance <- avg_cell_distance
  pop_df$avg_cell_distance_w_pop <- avg_cell_distance_w


  return(pop_df)
}

codes <- unique(grid_uca$code_muni)
metrics_df <- map(codes, calculate_metrics, data = grid_uca)
metrics_df <- rbindlist(metrics_df)

write_csv(metrics_df, "../../data/urbanformbr/landscape_metrics/cell_distance_metrics_2014.csv")



