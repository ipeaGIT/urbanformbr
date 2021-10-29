#' Script para preparar os resultados para a base de regressão
#'

source('R/setup.R')
mapviewOptions(platform = "leaflet")



# function ----------------------------------------------------------------

# city = "bage_rs"
# data <- urban_extent

process_city <- function(data, city) {

  message(paste("working on city", city))

  years <- c(1990, 2000, 2014)

  cells_prior <- data %>% filter(name_uca_case == city, year == 1975) %>%
    mutate(status = "consolidated")

  points_prior <- cells_prior %>% st_centroid() %>%
    filter(name_uca_case == city, year == 1975)

  points_list <- list(points_prior)
  cells_list <- list(cells_prior)

  for (y in years) {
    cells_current <- data %>% filter(name_uca_case == city, year == y)
    points_current <- cells_current %>% st_centroid()

    convex_hull <- st_union(points_prior) %>% st_convex_hull()

    mapview(convex_hull)

    points_current$intersect <- st_intersects(points_current, convex_hull, sparse = FALSE) %>% .[, 1]
    points_current$consolidated <- st_intersects(points_current, cells_prior, sparse = FALSE) %>% rowSums() %>% as.logical()


    points_current <- points_current %>%
      mutate(status = case_when(consolidated == TRUE ~ "consolidated",
                                intersect == TRUE ~ "inward",
                                TRUE ~ "outward")) %>%
      select(-intersect, -consolidated)


    points_current %>% mapview(zcol = "status") + convex_hull


    points_list <- list.append(points_list, points_current)
    cells_list <- list.append(cells_list, cells_current)

    data_prior <- points_current
    cells_prior <- cells_current
  }

  points_processed <- do.call(rbind, points_list)
  cells_processed <- do.call(rbind, cells_list)


  city_processed <- cells_processed %>%
    left_join(points_processed %>% st_set_geometry(NULL),
              by = c("code_muni", "name_uca_case", "cell", "built", "pop", "year")) %>%
    select(code_muni, name_uca_case, year, cell, status = status.y, built, pop, geometry)

  return(city_processed)

}


# load input data ---------------------------------------------------------


urban_extent <-
  rbind(
    read_rds("../../data/urbanformbr/ghsl/results/grid_uca_1975_cutoff20.rds") %>% mutate(year = 1975),
    read_rds("../../data/urbanformbr/ghsl/results/grid_uca_1990_cutoff20.rds") %>% mutate(year = 1990),
    read_rds("../../data/urbanformbr/ghsl/results/grid_uca_2000_cutoff20.rds") %>% mutate(year = 2000),
    read_rds("../../data/urbanformbr/ghsl/results/grid_uca_2014_cutoff20.rds") %>% mutate(year = 2014)
  ) %>%
  mutate(status = "")




# apply function - urban growth status  -------------------------------------------

cities <- unique(urban_extent$name_uca_case)

urban_extent_processed <- map_df(cities, process_city, data = urban_extent)

write_rds(urban_extent_processed, "../../data/urbanformbr/urban_growth/grid_uca_growth_status.rds")



# calculate population by status ------------------------------------------
urban_growth_df <- urban_extent_processed %>%
  st_set_geometry(NULL) %>%
  group_by(code_muni, name_uca_case, year, status) %>%
  summarise(pop = round(sum(pop)), .groups = "drop_last") %>%
  mutate(total_pop = sum(pop)) %>%
  pivot_wider(names_from = status, values_from = pop) %>%
  group_by(code_muni, name_uca_case) %>%
  arrange(year) %>%
  mutate(upward = consolidated - lag(total_pop),
         total_growth = total_pop - lag(total_pop)) %>%
  select(code_muni, name_uca_case, year, total_pop, total_growth,
         consolidated, upward, inward, outward)

write_rds(urban_growth_df, "../../data/urbanformbr/urban_growth/urban_growth_uca.rds")



urban_extent_processed %>%
  filter(name_uca_case == "porto_alegre_rs") %>%
  ggplot() +
  geom_sf(aes(fill=status)) +
  facet_wrap(~year)
