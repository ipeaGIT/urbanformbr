#' Script para preparar os resultados para a base de regressão
#'

source('R/setup.R')
mapviewOptions(platform = "leaflet")



# function ----------------------------------------------------------------

# city = "bage_rs"
# data <- urban_extent

process_city <- function(data, city) {

  message(paste("working on city", city))

  years_start <- c(1975, 1990, 2000, 1975)
  years_end <- c(1990, 2000, 2014, 2014)


  city_processed <- map2_df(years_start, years_end, function(y1, y2) {
    cells_prior <- data %>% filter(name_uca_case == city, year == y1) %>%
      mutate(status = "consolidated")

    points_prior <- cells_prior %>% st_centroid() %>%
      filter(name_uca_case == city, year == y1)

    cells_current <- data %>% filter(name_uca_case == city, year == y2)
    points_current <- cells_current %>% st_centroid()

    convex_hull <- st_union(points_prior) %>% st_convex_hull()

    mapview(convex_hull)

    points_current$intersect <- st_intersects(points_current, convex_hull, sparse = FALSE) %>% .[, 1]
    points_current$consolidated <- st_intersects(points_current, cells_prior, sparse = FALSE) %>% rowSums() %>% as.logical()


    points_current <- points_current %>%
      mutate(period_start = y1, period_end = y2) %>%
      mutate(status = case_when(consolidated == TRUE ~ "consolidated",
                                intersect == TRUE ~ "inward",
                                TRUE ~ "outward")) %>%
      select(-intersect, -consolidated)


    points_current %>% mapview(zcol = "status") + convex_hull

    pop_start <- sum(cells_prior$pop, na.rm = T)
    pop_end <- sum(cells_current$pop, na.rm = T)

    city_processed <- cells_current %>%
      left_join(points_current %>% st_set_geometry(NULL),
                by = c("code_muni", "name_uca_case", "cell", "built", "pop", "year")) %>%
      mutate(pop_start, pop_end) %>%
      select(code_muni, name_uca_case, period_start, period_end,
             pop_start, pop_end,
             cell, status = status.y, built, pop, geometry)


    return(city_processed)
  })


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


# calculate population growth by status ------------------------------------------


### absolute growth ---------------------------------------------------------

urban_growth_df <- urban_extent_processed %>%
  st_set_geometry(NULL) %>%
  group_by(code_muni, name_uca_case, period_start, period_end,
           pop_start, pop_end, status) %>%
  summarise(pop = round(sum(pop)), .groups = "drop_last") %>%
  mutate(pop_start = round(pop_start), pop_end = round(pop_end)) %>%
  pivot_wider(names_from = status, values_from = pop) %>%
  mutate(upward = consolidated - round(pop_start),
         total_growth = pop_end - pop_start) %>%
  select(code_muni, name_uca_case, period_start, period_end,
         pop_start, pop_end, pop_consolidated = consolidated,
         total_growth,
         upward_growth = upward, inward_growth = inward, outward_growth = outward)

write_rds(urban_growth_df, "../../data/urbanformbr/urban_growth/urban_growth_absolute.rds")


# geometric growth --------------------------------------------------------
geometric_growth_df <- urban_growth_df %>%
  mutate(period = period_end - period_start) %>%
  mutate(growth_total = (pop_end / pop_start) ^ (1/period) - 1,
         growth_upward = (upward_growth / total_growth) * growth_total,
         growth_inward = (inward_growth / total_growth) * growth_total,
         growth_outward = (outward_growth / total_growth) * growth_total) %>%
  ungroup() %>%
  select(code_muni, name_uca_case, period_start, period_end, growth_total, growth_upward, growth_inward, growth_outward) %>%
  replace_na(list(growth_upward = 0,
                  growth_inward = 0,
                  growth_outward = 0))

write_rds(geometric_growth_df, "../../data/urbanformbr/urban_growth/urban_growth_geometric.rds")

write_rds(geometric_growth_df, "../../data/urbanformbr/pca_regression_df/urban_growth.rds")


# check results -----------------------------------------------------------

urban_extent_processed <- read_rds(file = "../../data/urbanformbr/urban_growth/grid_uca_growth_status.rds")


urban_extent_processed %>%
  filter(name_uca_case == "porto_alegre_rs") %>%
  ggplot() +
  geom_sf(aes(fill=status)) +
  facet_wrap(~period_start + period_end)

urban_growth_df <- read_rds(file = "../../data/urbanformbr/urban_growth/urban_growth_absolute.rds")

urban_growth_df %>%
  filter(name_uca_case == "porto_alegre_rs") %>%
  View()

geometric_growth_df <- read_rds("../../data/urbanformbr/pca_regression_df/urban_growth.rds")


geometric_growth_df %>%
  ggplot() +
  geom_point(aes(x=growth_outward, y=growth_inward)) +
  scale_x_log10() +
  scale_y_log10()


urban_extent_processed %>%
  filter(period_start == 1975, period_end == 2014) %>%
  mapview()
