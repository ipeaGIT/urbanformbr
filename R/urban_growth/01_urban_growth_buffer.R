#' Script para preparar os resultados para a base de regressão
#'

source('R/fun_support/setup.R')
mapviewOptions(platform = "leaflet")



# function ----------------------------------------------------------------

# city = "porto_alegre_rs"
# data <- urban_extent

process_city <- function(data, city) {

  message(paste("working on city", city))

  years_start <- c(1990, 2000, 1990)
  years_end <- c(2000, 2014, 2014)

  city_processed <- map2_df(years_start, years_end, function(y1, y2) {
    message(paste0("working on city ", city, ", period ", y1, " to ", y2))
    # filter cells of current city and year
    # cells for initial state/year
    cells_prior <- data %>% filter(name_uca_case == city, year == y1) %>%
      mutate(status = "consolidated")

    points_prior <- cells_prior %>% st_centroid() %>%
      filter(name_uca_case == city, year == y1)

    # cells for current state/year
    cells_current <- data %>% filter(name_uca_case == city, year == y2)
    points_current <- cells_current %>% st_centroid()

    # consolidated area (y1) buffer
    buffer_dist <- 1000
    units(buffer_dist) <- "m"
    consolidated_buffer <- st_buffer(cells_prior, dist = buffer_dist) %>%
      group_by(code_urban_concentration, name_uca_case, year) %>%
      summarise(.groups = "drop") %>%
      st_cast("POLYGON") %>%
      mutate(patch_number = row_number())


    mapview(consolidated_buffer)

    points_current$adjacent <- st_intersects(points_current, consolidated_buffer, sparse = FALSE) %>% rowSums() %>% as.logical()
    points_current$consolidated <- st_intersects(points_current, cells_prior, sparse = FALSE) %>% rowSums() %>% as.logical()
    ## adjacent points are always consolidated, so we need to separate them
    points_current$adjacent <- xor(points_current$adjacent, points_current$consolidated)

    expansion_points <- subset(points_current, consolidated == FALSE)
    if (nrow(expansion_points) > 0) {
      expansion_buffer <- st_buffer(expansion_points, dist = buffer_dist) %>%
        group_by(code_urban_concentration, name_uca_case, year) %>%
        summarise(.groups = "drop") %>%
        st_cast("POLYGON") %>%
        mutate(patch_number = row_number())

      # identify expansion points that are in contiguous patches vs
      # points that are in fragmented patches
      expansion_buffer$contiguous <- st_intersects(expansion_buffer, consolidated_buffer, sparse = FALSE) %>% rowSums() %>% as.logical()

      mapview(expansion_buffer, zcol = "contiguous")# + consolidated_buffer
      mapview(expansion_buffer, zcol = "patch_number")# + consolidated_buffer

      points_current$leapfrog <- st_intersects(points_current,
                                               subset(expansion_buffer, contiguous == FALSE),
                                               sparse = FALSE) %>% rowSums() %>% as.logical()

      # identify expansion points that are surrounded by the
      # consolidated area
      points_adjacent <- subset(points_current, adjacent == TRUE)
      points_consolidated <- subset(points_current, consolidated == TRUE)
      dist_matrix <- st_distance(points_adjacent, points_consolidated)

      # remove units from distance matrix
      dist_matrix <-units::drop_units(dist_matrix)
      # count how many consolidated cells there are in a 'queen' neighborhood
      # using 1500 as a shortcut for a square root of 2
      dist_matrix[dist_matrix <= 1500] <- 1
      dist_matrix[dist_matrix > 1500] <- 0
      points_adjacent$n_neighbors <-  rowSums(dist_matrix)
      points_adjacent <- st_set_geometry(points_adjacent, NULL)

      # put information on number of consolidated neighbors back into points_current
      points_current <- left_join(points_current, points_adjacent,
                                  by = c("code_urban_concentration", "name_uca_case", "cell",
                                         "built", "pop", "year", "status",
                                         "adjacent", "consolidated", "leapfrog"))

    } else {
      points_current$leapfrog = FALSE
      points_current$n_neighbors = 0
    }

    points_current <- points_current %>%
      mutate(period_start = y1, period_end = y2) %>%
      mutate(status = case_when(consolidated == TRUE ~ "consolidated",
                                leapfrog == TRUE ~ "leapfrog",
                                adjacent == TRUE & n_neighbors >= 4  ~ "infill",
                                TRUE ~ "extension")) %>%
      select(-adjacent, -consolidated, -leapfrog, -n_neighbors)


      points_current %>% mapview(zcol = "status",
                                 col.regions = brewer_pal(type = "qual") )# + consolidated_buffer

    pop_start <- sum(cells_prior$pop, na.rm = T)
    pop_end <- sum(cells_current$pop, na.rm = T)
    built_start <- sum(cells_prior$built, na.rm = T)
    built_end <- sum(cells_current$built, na.rm = T)

    city_processed <- cells_current %>%
      left_join(points_current %>% st_set_geometry(NULL),
                by = c("code_urban_concentration", "name_uca_case", "cell", "built", "pop", "year")) %>%
      mutate(pop_start, pop_end, built_start, built_end) %>%
      select(code_urban_concentration, name_uca_case, period_start, period_end,
             pop_start, pop_end, built_start, built_end,
             cell, status = status.y, built, pop, geometry)


    return(city_processed)
  })


  return(city_processed)

}


# load input data ---------------------------------------------------------

urban_extent <-
  rbind(
    read_rds("../../data/urbanformbr/ghsl/results/grid_uca_1990_cutoff20.rds") %>% mutate(year = 1990),
    read_rds("../../data/urbanformbr/ghsl/results/grid_uca_2000_cutoff20.rds") %>% mutate(year = 2000),
    read_rds("../../data/urbanformbr/ghsl/results/grid_uca_2014_cutoff20.rds") %>% mutate(year = 2014)
  ) %>%
  mutate(status = "")


# apply function - urban growth status  -------------------------------------------

# poa_df <- process_city(data = urban_extent, city = "porto_alegre_rs")
# aba_df <- process_city(data = urban_extent, city = "itapipoca")

cities <- unique(urban_extent$name_uca_case)

urban_extent_processed <- map_df(cities, process_city, data = urban_extent)

write_rds(urban_extent_processed, "../../data/urbanformbr/urban_growth/grid_uca_growth.rds")


# calculate population growth by status ------------------------------------------


### absolute growth ---------------------------------------------------------

# remove cell's geometry
urban_growth_df <- st_set_geometry(urban_extent_processed, NULL)

# calculate total population and built area
urban_growth_df <- urban_growth_df %>%
  group_by(code_urban_concentration, name_uca_case, period_start, period_end,
           pop_start, pop_end, built_start, built_end, status) %>%
  summarise(pop = round(sum(pop)), built = sum(built), .groups = "drop_last") %>%
  mutate(pop_start = round(pop_start), pop_end = round(pop_end))

# cleanup some fields
urban_growth_df <- urban_growth_df %>%
  rename(growth_type = status) %>%
  mutate(growth_type = if_else(growth_type == "consolidated",
                               "upward",
                               growth_type))

# calculate growth components
urban_growth_df <- urban_growth_df %>%
  mutate(pop_abs_growth = if_else(growth_type == "upward",
                                  pop - pop_start,
                                  pop),
         built_abs_growth = if_else(growth_type == "upward",
                                    built - built_start,
                                    built))

# calculate geometric growth
urban_growth_df <- urban_growth_df %>%
  mutate(period_length = period_end - period_start,
         pop_total_growth = pop_end - pop_start,
         built_total_growth = built_end - built_start,
         pop_total_geo_growth = (pop_end / pop_start) ^ (1/period_length) - 1,
         built_total_geo_growth = (built_end / built_start) ^ (1/period_length) - 1) %>%
  mutate(pop_geo_growth = (pop_abs_growth / pop_total_growth) * pop_total_geo_growth,
         built_geo_growth = (built_abs_growth / built_total_growth) * built_total_geo_growth)

# drop excess columns
urban_growth_df <- urban_growth_df %>%
  select(code_urban_concentration:built_abs_growth, pop_geo_growth, built_geo_growth) %>%
  rename(pop_by_growth_type = pop, built_by_growth_type = built)


write_rds(urban_growth_df, "../../data/urbanformbr/urban_growth/urban_growth.rds")


# check results -----------------------------------------------------------

urban_extent_processed <- read_rds(file = "../../data/urbanformbr/urban_growth/grid_uca_growth.rds")

# poa_df %>%
urban_extent_processed %>%
  filter(name_uca_case == "porto_alegre_rs") %>%
  ggplot() +
  geom_sf(aes(fill=status)) +
  facet_wrap(~period_start + period_end)

urban_growth_df <- read_rds("../../data/urbanformbr/urban_growth/urban_growth.rds")

urban_growth_df %>%
  filter(name_uca_case == "porto_alegre_rs") %>%
  View()


mapviewOptions(platform = "leaflet")
mv_growth <- urban_extent_processed %>%
  filter(period_start == 1975, period_end == 2014) %>%
  mapview(zcol = "status", layer.name = "1975 - 2014",
          col.regions = brewer_pal(type = "qual", palette = "Set1"))
mv_growth
mapshot(mv_growth, url = paste0(getwd(), "/map_urban_growth.html"))


save_html_map <- function(data, start, end) {
  mv_growth <- data %>%
    filter(period_start == start, period_end == end) %>%
    mapview(zcol = "status", layer.name = paste(start, end, sep = " - "),
            col.regions = brewer_pal(type = "qual", palette = "Set1"))
  mv_growth
  mapshot(mv_growth, url = paste0(getwd(), "/map_urban_growth_", start, "_", end, ".html"))

}
save_html_map(urban_extent_processed, 1990, 2014)
save_html_map(urban_extent_processed, 1990, 2000)
save_html_map(urban_extent_processed, 2000, 2014)


