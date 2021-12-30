#' Script para calcular medidas de fragmentação baseadas em Ecologia de Paisagem,
#' utilizando o pacote landscape_metrics
#' https://github.com/r-spatialecology/landscapemetrics


source('R/fun_support/setup.R')
library("landscapemetrics")


data_folder <- "../../data/urbanformbr/ghsl/BUILT/urban_extent_cutoff_20_raster"

raster_files <- list.files(path = data_folder, full.names = TRUE, pattern = "1K_raster.tif$")
# remove data from 1975
raster_files <- raster_files[str_detect(raster_files, "1975", negate = TRUE)]

# function ----------------------------------------------------------------

calculate_fragmentation <- function(file) {
  city_year <- stringr::str_remove(file, pattern = "../../data/urbanformbr/ghsl/BUILT/urban_extent_cutoff_20_raster/GHS_BUILT_LDS")
  city_year <- stringr::str_remove(city_year, pattern = "_1K_raster.tif")
  city_year <- stringr::str_remove(city_year, pattern = "_urban_extent_cutoff_20")

  city_year <- stringr::str_split(city_year, pattern = "_", simplify = TRUE)

  city <- paste(city_year[-1], collapse = "_")
  year <- city_year[1]

  ## load and reclassify raster
  rst <- raster(file)
  m <- c(0, 20, 0,  20, 100, 1)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  rst <- raster::reclassify(rst, rclmat)

  if (cellStats(rst, stat = "sum") > 1) {
    # Prepare geometries: Patches and Cells
    patches <- landscapemetrics::get_patches(rst)

    patches_sf <- map_df(patches$layer_1, function(p) {
      raster::rasterToPolygons(p) %>%
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

    # Consolidate results
    patches_sf <- patches_sf %>%
      left_join(patch_metrics_df, by = c("patch_id" = "id")) %>%
      mutate(city, year) %>%
      select(city, year, patch_id, cell_id, patch_type, geometry) %>%
      group_by(city, year, patch_id, patch_type)


    gpkg_file <- paste0("../../data/urbanformbr/fragmentation_compacity/patches/", city, "_", year, ".gpkg")
    if (!file.exists(gpkg_file)) {
      st_write(patches_sf, gpkg_file)
    }

    area_city <- sum(patch_metrics_df$area) / 100
    area_city_large <- sum(patch_metrics_df %>% filter(patch_type %in% c("large", "core")) %>% .$area) / 100
    area_city_core <- sum(patch_metrics_df %>% filter(patch_type == "core") %>% .$area) / 100


    # mapview(patches_sf, zcol = "patch_type")

    urban_sf <- rasterToPolygons(rst) %>%
      sf::st_as_sf() %>%
      summarise() %>%
      mutate(city, year,
             area_city,
             area_large_patches = area_city_large,
             area_core_patches = area_city_core,
             n_patches, n_large_patches, n_core_patches,
             proportion_largest_patch) %>%
      select(city:proportion_largest_patch, geometry)

    # mapview::mapview(urban_sf)


    return(urban_sf)

    # sf::st_write(urban_metrics_sf, sprintf("lsm_%s_%s.gpkg", city, year))

  } else { return(NULL)}

}



# apply function ----------------------------------------------------------



## calculate fragmentation
fragmentation_metrics_df <- map_df(raster_files, calculate_fragmentation)

## save results
sf::st_write(fragmentation_metrics_df, "../../data/urbanformbr/fragmentation_compacity/fragmentation.gpkg")

fragmentation_metrics_df %>% filter(year == "1990") %>% sf::st_write("../../data/urbanformbr/fragmentation_compacity/fragmentation_1990.gpkg")
fragmentation_metrics_df %>% filter(year == "2000") %>% sf::st_write("../../data/urbanformbr/fragmentation_compacity/fragmentation_2000.gpkg")
fragmentation_metrics_df %>% filter(year == "2014") %>% sf::st_write("../../data/urbanformbr/fragmentation_compacity/fragmentation_2014.gpkg")

fragmentation_metrics_df %>%
  st_set_geometry(NULL) %>%
  write_csv("../../data/urbanformbr/fragmentation_compacity/fragmentation_metrics.csv")

