source('R/setup.R')


# data <- grid_uca
# muni <- 3550308 # São Paulo

build_distance_grid <- function(muni, data, n_cells = 2500) {
  ## Input data
  muni_sf <- filter(data, code_muni == muni)
  # muni_sf %>% mapview()

  cells_centroids_sf <- sf::st_centroid(muni_sf) %>% sf::st_transform(31983)
  # cells_centroids_sf %>% mapview()

  grid_points <- st_make_grid(cells_centroids_sf, cellsize = 1000, what = "centers")
  grid_sf <- data.frame(id = 1:length(grid_points), geometry = grid_points) %>%
    st_as_sf(crs = 31983) %>%
    st_transform(4326)
  # grid_sf %>% mapview() + center_cell

  center_cell <- grid_sf %>% slice(nrow(.)/2)

  distance_matrix <- geodist::geodist(center_cell %>%  sf::st_coordinates(),
                                      grid_sf %>% sf::st_coordinates())

  grid_sf$distance <- t(distance_matrix)
  grid_filtered_sf <- grid_sf %>% arrange(distance) %>% slice(1:n_cells)
  # grid_filtered_sf %>% mapview()

  return(grid_filtered_sf)
}

calculate_avg_cell_dist <- function(size_threshold, compact_grid) {

  filtered_grid <- compact_grid %>%
    arrange(distance) %>%
    slice(1:size_threshold) %>%
    mutate(cell = row_number())

  cells_centroids_sf <- sf::st_centroid(filtered_grid) %>% sf::st_transform(4326)
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

  distance_matrix[from_cell == to_cell, distance := 0.5]
  avg_distances <- distance_matrix[, .(avg_distance = mean(distance)), by = from_cell]
  avg_cell_distance <- mean(avg_distances$avg_distance)

  dist_df <- data.frame(size = size_threshold, compact_cell_distance = avg_cell_distance)

  return(dist_df)
}

grid_uca <- read_rds("../../data/urbanformbr/ghsl/results/grid_uca_2014_cutoff20.rds")
grid_uca_1975 <- read_rds("../../data/urbanformbr/ghsl/results/grid_uca_1975_cutoff20.rds")

uca_sizes <-
  rbind(grid_uca %>% mutate(year = 2014),
        grid_uca_1975 %>% mutate(year = 1975)) %>%
  st_set_geometry(NULL) %>%
  group_by(code_muni, name_uca_case, year) %>%
  summarise(size = n())


# build compact grid using São Paulo as a reference
compact_grid_sf <- build_distance_grid(muni = 3550308, data = grid_uca, n_cells = 2500)


compact_cell_dist <- map_df(unique(uca_sizes$size),
                            calculate_avg_cell_dist, compact_grid = compact_grid_sf)


metrics_1975_df <- read_csv("../../data/urbanformbr/landscape_metrics/cell_distance_metrics_1975.csv") %>%
  replace_na(list(avg_cell_distance = 0.5, avg_cell_distance_w_pop = 0.5))
metrics_2014_df <- read_csv("../../data/urbanformbr/landscape_metrics/cell_distance_metrics_2014.csv")


metrics_1975_df <-
  metrics_1975_df %>%
  left_join(uca_sizes %>% filter(year == 1975)) %>%
  left_join(compact_cell_dist) %>%
  mutate(avg_cell_distance_norm = avg_cell_distance / compact_cell_distance)
  # View()


metrics_2014_df <- metrics_2014_df %>%
  left_join(uca_sizes %>% filter(year == 2014)) %>%
  left_join(compact_cell_dist) %>%
  mutate(avg_cell_distance_norm = avg_cell_distance / compact_cell_distance)


cities_df <- metrics_2014_df %>%
  filter(pop > 3100000 | avg_cell_distance_norm > 4)

metrics_2014_df %>%
  ggplot(aes(x=pop, y=avg_cell_distance)) +
  geom_point(aes(size = pop)) +
  geom_text(data = cities_df, aes(label=name_uca_case)) +
  scale_x_log10() +
  scale_y_log10()

metrics_2014_df %>%
  ggplot(aes(x=size, y=avg_cell_distance_norm)) +
  geom_point(aes(size = pop)) +
  geom_text(data = cities_df, aes(label=name_uca_case)) +
  scale_x_log10()
  # scale_y_log10()

metrics_2014_df %>%
  ggplot(aes(y=pop, x=avg_cell_distance_norm)) +
  geom_point(aes(size = pop)) +
  geom_text(data = cities_df, aes(label=name_uca_case)) +
  scale_y_log10()

grid_uca %>%
  group_by(code_muni) %>%
  summarise() %>%
  st_centroid() %>%
  left_join(metrics_2014_df, by = c("code_muni")) %>%
  mapview(zcol = "avg_cell_distance_norm")




