library(raster)
library(landscapemetrics)
library(tidyverse)

data_folder <- "../../data/urbanformbr/ghsl/BUILT/urban_extent_cutoff_20"

raster_files <- list.files(path = data_folder, full.names = TRUE, pattern = "1K_raster.tif$")

apply_metrics <- function(file) {
  city_year <- stringr::str_remove(file, pattern = "../../data/urbanformbr/ghsl/BUILT/urban_extent_cutoff_20/GHS_BUILT_LDS")
  city_year <- stringr::str_remove(city_year, pattern = "_1K_raster.tif")
  city_year <- stringr::str_remove(city_year, pattern = "urban_extent_cutoff_20_")

  city_year <- stringr::str_split(city_year, pattern = "_", simplify = TRUE)

  city <- paste(city_year[-1], collapse = " ")
  year <- city_year[1]

  ## load and reclassify raster
  rst <- raster(file)
  m <- c(0, 20, 0,  20, 100, 1)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  rst <- reclassify(rst, rclmat)

  if (cellStats(rst, stat = "sum") > 1) {
    urban_sf <- rasterToPolygons(rst) %>%
      sf::st_as_sf() %>%
      summarise() %>%
      mutate(area = sf::st_area(geometry)) %>%
      mutate(city, year)

    # mapview::mapview(urban_sf)


    city_metrics_df <- calculate_lsm(rst, level = "class") %>%
      pivot_wider(names_from = metric, values_from = value) %>%
      mutate(city, year)


    urban_metrics_sf <- urban_sf %>%
      left_join(city_metrics_df)

    return(urban_metrics_sf)

    # sf::st_write(urban_metrics_sf, sprintf("lsm_%s_%s.gpkg", city, year))

  } else { return(NULL)}





  # View(list_lsm())
  # View(raster_files)

  # return(city_metrics_df)
}

file = raster_files[156]
apply_metrics(raster_files[156])
df <- map(raster_files, apply_metrics)

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



View(stringr::str_remove(raster_files, pattern = "../../data/urbanformbr/ghsl/BUILT/urban_extent_cutoff_20/GHS_BUILT_LDS"))
