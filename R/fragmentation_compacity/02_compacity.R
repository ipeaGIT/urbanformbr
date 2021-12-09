#' Script para calcular medidas de compacidade baseadas na distância média entre
#' as células urbanas
#'
source('R/fun_support/setup.R')


# functions ---------------------------------------------------------------


## on compact grid ---------------------------------------------------------

# data <- grid_uca
# muni <- 3550308 # São Paulo
# max_cells = 2500

build_compact_grid <- function(muni, data, max_cells = 2500) {
  ## Input data
  muni_sf <- filter(data, code_muni == muni, year=="2014") %>% sf::st_transform(31983)
  cells_centroids_sf <- sf::st_centroid(muni_sf)

  # build regular grid covering study area
  grid_points <- st_make_grid(cells_centroids_sf, cellsize = 1000, what = "centers")
  grid_sf <- data.frame(id = 1:length(grid_points), geometry = grid_points) %>%
    st_as_sf(crs = 31983) %>%
    st_transform(4326)
  center_cell <- grid_sf %>% slice(nrow(.)/2)

  # calculate distances to center cell
  distance_matrix <- geodist::geodist(center_cell %>%  sf::st_coordinates(),
                                      grid_sf %>% sf::st_coordinates(),
                                      measure = "geodesic")

  grid_sf$distance <- t(distance_matrix)

  # build return grid
  grid_filtered_sf <- grid_sf %>%
    arrange(distance) %>%
    slice(1:max_cells) %>%
    mutate(cell = row_number()) %>%
    select(cell, distance, geometry)
  # grid_filtered_sf %>% mapview()

  return(grid_filtered_sf)
}

calculate_compact_cell_dist <- function(compact_grid, n_cells = 2500) {

  filtered_grid <- compact_grid %>%
    arrange(distance) %>%
    slice(1:n_cells)

  ## Distance matrix
  distance_matrix <- geodist::geodist(filtered_grid %>% sf::st_coordinates(), measure = "geodesic")
  distance_matrix <- distance_matrix / 1000
  colnames(distance_matrix) <- filtered_grid$cell

  distance_matrix <- as.data.frame(distance_matrix)
  distance_matrix$from_cell <- filtered_grid$cell
  distance_matrix <- pivot_longer(distance_matrix, !from_cell,
                                  names_to = "to_cell", values_to = "distance") %>%
    mutate(to_cell = as.numeric(to_cell))
  setDT(distance_matrix)

  distance_matrix[from_cell == to_cell, distance := 0.5]
  avg_distances <- distance_matrix[, .(avg_distance = mean(distance)), by = from_cell]
  avg_cell_distance <- mean(avg_distances$avg_distance)

  avg_closenesses <- distance_matrix[, .(avg_closeness = sum(1/distance)), by = from_cell]
  avg_cell_closeness <- mean(avg_closenesses$avg_closeness)

  dist_df <- data.frame(size = n_cells,
                        compact_cell_distance = avg_cell_distance,
                        compact_cell_closeness = avg_cell_closeness)

  return(dist_df)
}


## on real grid ------------------------------------------------------------

calculate_real_cell_dist <- function(data, muni, ano, compact_grid) {
  message(paste0("working on city ", muni, ", year ", ano))

    # filter muni
  muni_sf <- data %>% filter(code_muni == muni, year == ano)

  # Compacity metrics

  ## centroids
  cells_centroids_sf <- sf::st_centroid(muni_sf) %>% sf::st_transform(4326)
  # mapview(cells_centroids_sf, crs = 4326)

  ## Distance matrix
  distance_matrix <- geodist::geodist(cells_centroids_sf %>% sf::st_coordinates(), measure = "geodesic")
  distance_matrix <- distance_matrix / 1000
  colnames(distance_matrix) <- cells_centroids_sf$cell

  distance_matrix <- as.data.frame(distance_matrix)
  distance_matrix$from_cell <- cells_centroids_sf$cell
  distance_matrix <- pivot_longer(distance_matrix, !from_cell,
                                  names_to = "to_cell", values_to = "distance") %>%
    mutate(to_cell = as.numeric(to_cell))
  setDT(distance_matrix)

  ## Distance and Closeness metrics
  distance_matrix[from_cell == to_cell, distance := 0.5]
  avg_distances <- distance_matrix[, .(avg_distance = mean(distance)), by = from_cell]
  avg_cell_distance <- mean(avg_distances$avg_distance)

  avg_closenesses <- distance_matrix[, .(avg_closeness = sum(1/distance)), by = from_cell]
  avg_cell_closeness <- mean(avg_closenesses$avg_closeness)

  ## Distance and Closeness on compact grid
  compact_df <- calculate_compact_cell_dist(compact_grid, nrow(muni_sf))

  ## Consolidate results
  muni_sf <- muni_sf %>%
    group_by(code_muni, name_uca_case, year) %>%
    summarise(city_size = n(),
              avg_cell_distance = avg_cell_distance,
              avg_cell_closeness = avg_cell_closeness,
              .groups = "drop") %>%
    sf::st_transform(4326)

  muni_sf$avg_cell_distance_norm <- muni_sf$avg_cell_distance / compact_df$compact_cell_distance
  muni_sf$avg_cell_closeness_norm <- muni_sf$avg_cell_closeness / compact_df$compact_cell_closeness

  muni_sf <- muni_sf %>% select(code_muni, name_uca_case, year, city_size,
                     compacity_abs = avg_cell_closeness,
                     compacity_norm = avg_cell_closeness_norm, geometry) %>%
    mutate(compacity_norm = if_else(compacity_norm <= 1, compacity_norm, 1))

  return(muni_sf)
}


# calculate metrics -------------------------------------------------------


## load data
to_be_removed <- c(4322400, 4108304, 5003207, 4316808)

grid_uca <- rbind(
  read_rds("../../data/urbanformbr/ghsl/results/grid_uca_1990_cutoff20.rds") %>% mutate(year = 1990),
  read_rds("../../data/urbanformbr/ghsl/results/grid_uca_2000_cutoff20.rds") %>% mutate(year = 2000),
  read_rds("../../data/urbanformbr/ghsl/results/grid_uca_2014_cutoff20.rds") %>% mutate(year = 2014)
) %>%
  filter(code_muni %nin% to_be_removed) %>%
  select(code_muni, name_uca_case, year, cell, built, pop, geometry)

## build compact grid using São Paulo as a reference
compact_grid_sf <- build_compact_grid(data = grid_uca, muni = 3550308)

## calculate metrics for each UCA and year
uca_codes_years <- grid_uca %>%
  select(code_muni, year) %>%
  st_set_geometry(NULL) %>%
  distinct()

compacity_df <- map2_df(uca_codes_years$code_muni,
                        uca_codes_years$year,
                        calculate_real_cell_dist,
                        data = grid_uca, compact_grid = compact_grid_sf)

## save results
sf::st_write(compacity_df, "../../data/urbanformbr/fragmentation_compacity/compacity.gpkg")

compacity_df %>% filter(year == "1990") %>% sf::st_write("../../data/urbanformbr/fragmentation_compacity/compacity_1990.gpkg")
compacity_df %>% filter(year == "2000") %>% sf::st_write("../../data/urbanformbr/fragmentation_compacity/compacity_2000.gpkg")
compacity_df %>% filter(year == "2014") %>% sf::st_write("../../data/urbanformbr/fragmentation_compacity/compacity_2014.gpkg")

compacity_df %>%
  st_set_geometry(NULL) %>%
  write_csv("../../data/urbanformbr/fragmentation_compacity/compacity_metrics.csv")

## compacity on compact grid
max_compacity_df <- map_df(1:2500, calculate_compact_cell_dist, compact_grid = compact_grid_sf)
max_compacity_df %>% write_csv("../../data/urbanformbr/fragmentation_compacity/max_compacity_metrics.csv")
max_compacity_df %>%
  ggplot(aes(x=size, y=compact_cell_distance)) +
  geom_point()

max_compacity_df %>%
  ggplot(aes(x=size, y=compact_cell_closeness)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()
