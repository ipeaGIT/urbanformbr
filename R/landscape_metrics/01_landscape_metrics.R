source('R/setup.R')
library("landscapemetrics")

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

    # Prepare geometries: Patches and Cells
    patches <- landscapemetrics::get_patches(rst)

    patches_sf <- map_df(patches, function(p) {
      rasterToPolygons(p) %>%
        sf::st_as_sf()
    })

    patches_sf <- patches_sf %>%
      mutate(cell_id = row_number()) %>%
      select(patch_id = layer, cell_id, geometry)

    cells_centroids_sf <- sf::st_centroid(patches_sf) %>% sf::st_transform(4326)

    # mapview(patches_sf, zcol = "patch_id")

    # Calculate patch metrics
    patch_metrics_df <- calculate_lsm(rst, level = "patch") %>%
      pivot_wider(names_from = metric, values_from = value) %>%
      mutate(patch_type = case_when(core > 0 ~ "core",
                                    area >= 500 ~ "large",
                                    TRUE ~ "small"))

    # Fragmentation metrics - # of Patches
    n_patches <- nrow(patch_metrics_df)
    n_core_patches <- nrow(patch_metrics_df %>% filter(core > 0))
    n_large_patches <- nrow(patch_metrics_df %>% filter(area >= 500))

    proportion_largest_patch <- max(patch_metrics_df$area) / sum(patch_metrics_df$area)

    # Compacity metrics

    ## Distance matrix
    distance_matrix <- geodist::geodist(cells_centroids_sf %>% sf::st_coordinates(), measure = "geodesic")
    distance_matrix <- distance_matrix / 1000
    colnames(distance_matrix) <- cells_centroids_sf$cell_id

    distance_matrix <- as.data.frame(distance_matrix)
    distance_matrix$from_cell <- cells_centroids_sf$cell_id
    distance_matrix <- pivot_longer(distance_matrix, !from_cell,
                                    names_to = "to_cell", values_to = "distance") %>%
      mutate(to_cell = as.numeric(to_cell))
    setDT(distance_matrix)

    cell_patches_dt <- sf::st_set_geometry(cells_centroids_sf, NULL)
    setDT(cell_patches_dt)

    distance_matrix[cell_patches_dt, on = .(from_cell = cell_id), from_patch := i.patch_id]
    distance_matrix[cell_patches_dt, on = .(to_cell = cell_id), to_patch := i.patch_id]

    ## Average cell and patch distances
    avg_cell_distance <- mean(distance_matrix[from_cell != to_cell, ]$distance)

    large_patches <- patch_metrics_df %>% filter(patch_type %in% c("large", "core")) %>% .$id
    core_patches <- patch_metrics_df %>% filter(patch_type == "core") %>% .$id

    patch_distances <- distance_matrix[from_cell != to_cell, .(distance = mean(distance)), by = .(from_patch, to_patch)]
    avg_patch_distance <- mean(patch_distances$distance)

    avg_large_patch_distance <- mean(patch_distances[from_patch %in% large_patches & to_patch %in% large_patches, ]$distance)
    avg_core_patch_distance <- mean(patch_distances[from_patch %in% core_patches & to_patch %in% core_patches, ]$distance)


    ## Ratio Area / Circle
    circle_radius <- max(distance_matrix$distance) / 2
    large_circle_radius <- max(distance_matrix[from_patch %in% large_patches & to_patch %in% large_patches, ]$distance) / 2
    core_circle_radius <- max(distance_matrix[from_patch %in% core_patches & to_patch %in% core_patches, ]$distance) / 2

    area_circle <- pi * circle_radius^2
    area_circle_large <- pi * large_circle_radius^2
    area_circle_core <- pi * core_circle_radius^2

    area_city <- sum(patch_metrics_df$area) / 100
    area_city_large <- sum(patch_metrics_df %>% filter(patch_type %in% c("large", "core")) %>% .$area) / 100
    area_city_core <- sum(patch_metrics_df %>% filter(patch_type == "core") %>% .$area) / 100

    ratio_city_circle <- area_city / area_circle
    ratio_city_circle_large <- area_city_large / area_circle_large
    ratio_city_circle_core <- area_city_core / area_circle_core

    ## Entropy of patch sizes
    patch_sizes <- patch_metrics_df$area
    patch_prop <- patch_sizes / sum(patch_sizes)
    patch_size_entropy <- sum(patch_prop * log(1/patch_prop))

    max_entropy <- log(length(patch_sizes))
    patch_size_entropy_norm <- patch_size_entropy / max_entropy


    # Consolidate results
    patches_sf <- patches_sf %>%
      left_join(patch_metrics_df, by = c("patch_id" = "id")) %>%
      mutate(city, year) %>%
      select(city, year, patch_id, cell_id, patch_type, geometry) %>%
      group_by(city, year, patch_id, patch_type)


    gpkg_file <- paste0("../../data/urbanformbr/landscape_metrics/", city, "_", year, ".gpkg")
    if (!file.exists(gpkg_file)) {
      st_write(patches_sf, gpkg_file)
    }


    # mapview(patches_sf, zcol = "patch_type")

    urban_sf <- rasterToPolygons(rst) %>%
      sf::st_as_sf() %>%
      summarise() %>%
      mutate(city, year,
             area_city,
             area_large_patches = area_city_large,
             area_core_patches = area_city_core,
             n_patches, n_large_patches, n_core_patches,
             proportion_largest_patch,
             patch_size_entropy,
             patch_size_entropy_norm,
             ratio_circle = ratio_city_circle,
             ratio_circle_large = ratio_city_circle_large,
             ratio_circle_core = ratio_city_circle_core,
             avg_cell_distance,
             avg_patch_distance,
             avg_large_patch_distance,
             avg_core_patch_distance) %>%
      select(city:avg_core_patch_distance, geometry)

    # mapview::mapview(urban_sf)


    return(urban_sf)

    # sf::st_write(urban_metrics_sf, sprintf("lsm_%s_%s.gpkg", city, year))

  } else { return(NULL)}





  # View(list_lsm())
  # View(raster_files)

  # return(city_metrics_df)
}

# file = raster_files[735]
# file = raster_files[156]
#
# rio_files <- raster_files[str_detect(raster_files, "rio_de_janeiro")]
# file = rio_files[4]
#
# df <- apply_metrics(rio_files[4])


# apply function ----------------------------------------------------------

ls_metrics_df <- map_df(raster_files, apply_metrics)

sf::st_write(ls_metrics_df, "../../data/urbanformbr/landscape_metrics/landscape_metrics.gpkg")

ls_metrics_df %>% filter(year == "1975") %>% sf::st_write("../../data/urbanformbr/landscape_metrics/landscape_metrics_1975.gpkg")
ls_metrics_df %>% filter(year == "1990") %>% sf::st_write("../../data/urbanformbr/landscape_metrics/landscape_metrics_1990.gpkg")
ls_metrics_df %>% filter(year == "2000") %>% sf::st_write("../../data/urbanformbr/landscape_metrics/landscape_metrics_2000.gpkg")
ls_metrics_df %>% filter(year == "2014") %>% sf::st_write("../../data/urbanformbr/landscape_metrics/landscape_metrics_2014.gpkg")

ls_metrics_df %>%
  st_set_geometry(NULL) %>%
  write_csv("../../data/urbanformbr/landscape_metrics/landscape_metrics.csv")


