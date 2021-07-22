library(raster)
library(tidyverse)
library(data.table)

source('R/setup.R')
library(landscapemetrics)


data_folder <- "../../data/urbanformbr/ghsl/BUILT/urban_extent_cutoff_20"

raster_files <- list.files(path = data_folder, full.names = TRUE, pattern = "1K_raster.tif$")

apply_metrics <- function(file) {
  city_year <- stringr::str_remove(file, pattern = "../../data/urbanformbr/ghsl/BUILT/urban_extent_cutoff_20/GHS_BUILT_LDS")
  city_year <- stringr::str_remove(city_year, pattern = "_1K_raster.tif")
  city_year <- stringr::str_remove(city_year, pattern = "urban_extent_cutoff_20_")

  city_year <- stringr::str_split(city_year, pattern = "_", simplify = TRUE)

  city <- paste(city_year[-1], collapse = "_")
  year <- city_year[1]

  ## load and reclassify raster
  rst <- raster(file)
  m <- c(0, 20, 0,  20, 100, 1)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  rst <- reclassify(rst, rclmat)

  if (cellStats(rst, stat = "sum") > 1) {

    patches <- landscapemetrics::get_patches(rst)
    patches_sf <- rasterToPolygons(patches[[1]]) %>%
      sf::st_as_sf() %>%
      mutate(id = row_number())

    # mapview(patches_sf, zcol = "layer")

    cells_sf <- sf::st_centroid(patches_sf) %>% sf::st_transform(4326)
    distance_matrix <- geodist::geodist(cells_sf %>% sf::st_coordinates(), measure = "geodesic")
    distance_matrix <- distance_matrix / 1000
    colnames(distance_matrix) <- cells_sf$id

    distance_matrix <- as.data.frame(distance_matrix)
    distance_matrix$from_id <- cells_sf$id
    distance_matrix <- pivot_longer(distance_matrix, !from_id,
                                    names_to = "to_id", values_to = "distance") %>%
      mutate(to_id = as.numeric(to_id))
    setDT(distance_matrix)

    cell_patches_dt <- sf::st_set_geometry(cells_sf, NULL)
    setDT(cell_patches_dt)

    distance_matrix[cell_patches_dt, on = .(from_id = id), from_patch := i.layer]
    distance_matrix[cell_patches_dt, on = .(to_id = id), to_patch := i.layer]

    closeness_cell <- distance_matrix[from_id != to_id, .(closeness = sum(1/distance)), by = .(from_id)]
    closeness_patch <- distance_matrix[from_id != to_id, .(closeness = sum(1/distance)), by = .(from_patch)]

    distance_patch <- distance_matrix[from_id != to_id, .(mean_distance = mean(distance)), by = .(from_patch, to_patch)]

    if (nrow(distance_w) == 0) {
      compactness <- 1
    } else {
      distance_w[, gravity := 1 / gravity]
      distance_w <- distance_w[, .(gravity = sum(gravity)), by = .(from_patch)]
      compactness <- sum(distance_w$gravity)
    }

    circle_radius <- max(distance_matrix$distance) / 2
    area_circle <- pi * circle_radius^2
    area_city <- nrow(cells_sf)
    ratio_city_circle <- area_city / area_circle

    city_metrics_df <- calculate_lsm(rst, level = "patch") %>%
      pivot_wider(names_from = metric, values_from = value)

    n_patches <- nrow(city_metrics_df)
    n_large_patches <- nrow(city_metrics_df %>% filter(core > 0))

   # mapview::mapview(cells_sf)

    urban_sf <- rasterToPolygons(rst) %>%
      sf::st_as_sf() %>%
      summarise() %>%
      mutate(city, year, n_patches, n_large_patches, area_city, ratio_city_circle, compactness) %>%
      select(city:compactness, geometry)

    # mapview::mapview(urban_sf)


    # city_metrics_df <- calculate_lsm(rst, level = "patch") %>%
    #   pivot_wider(names_from = metric, values_from = value) %>%
    #   mutate(city, year)
    #
    #
    # urban_metrics_sf <- urban_sf %>%
    #   left_join(city_metrics_df)

    return(urban_sf)

    # sf::st_write(urban_metrics_sf, sprintf("lsm_%s_%s.gpkg", city, year))

  } else { return(NULL)}





  # View(list_lsm())
  # View(raster_files)

  # return(city_metrics_df)
}

file = raster_files[735]
file = raster_files[156]
file = rio_files[1]
apply_metrics(raster_files[156])

ls_metrics_df <- map_df(raster_files, apply_metrics)

df_all <- do.call(rbind, df)
df_all <- df_all %>% filter(!is.na(class))
sf::st_write(df_all, "landscape_metrics.gpkg")

df_all %>% filter(year == "1975") %>% sf::st_write("landscape_metrics_1975.gpkg")
df_all %>% filter(year == "1990") %>% sf::st_write("landscape_metrics_1990.gpkg")
df_all %>% filter(year == "2000") %>% sf::st_write("landscape_metrics_2000.gpkg")
df_all %>% filter(year == "2014") %>% sf::st_write("landscape_metrics_2014.gpkg")



View(list_lsm())
df_area <- df %>% filter(class != 0, metric == "area") %>%
  group_by(city, year) %>% View()
  summarise(largest_patch = max(value), total_patch = sum(value)) %>%
  mutate(prop_largest = largest_patch / total_patch)

View(df_area)

df_area %>%
  ggplot(aes(x=year, y=prop_largest, group = city)) +
  geom_point() +
  geom_path()

df_area %>%
  ggplot(aes(x=total_patch, y=prop_largest, group=city)) +
  geom_path() +
  geom_point()
  scale_x_log10() +
  scale_y_log10()

  df %>% filter(metric == "ncore") %>%
    ggplot(aes(value)) + geom_histogram()

unique(df$metric)

df %>% write.csv("landscape_metrics.csv")
df <- read_csv("landscape_metrics.csv")


View(stringr::str_remove(raster_files, pattern = "../../data/urbanformbr/ghsl/BUILT/urban_extent_cutoff_20/GHS_BUILT_LDS"))
