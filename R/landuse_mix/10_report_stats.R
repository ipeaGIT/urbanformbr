# description -------------------------------------------------------------

#' Este script calcula estatísticas sobre a cobertura da base de dados de
#' faces de logradouros e também a quantidade de endereços que puderam
#' ser corretamente georreferenciados

# setup -------------------------------------------------------------------

source('R/fun_support/setup.R')

# list of Brazilian municipalities
munis_df <- geobr::lookup_muni("all")
ucas_df <- geobr::read_urban_concentrations() %>% st_set_geometry(NULL)
urban_areas_grid <- read_rds("../../data/urbanformbr/ghsl/results/grid_uca_2014_cutoff20.rds")
urbanform_metrics <- read_csv("../../data/urbanformbr/consolidated_data/urbanformbr_metrics_full.csv")

# muni <- 4301602 # Bagé
# muni <- 4314407 # Pelotas
# muni <- 2304400 # Fortaleza
# muni <- 1101401 # Monte Negro
# muni <- 2927408 # Salvador
muni <- 4322400

# function ----------------------------------------------------------------

calculate_coverage <- function(muni) {
  # extract urban area name and uf from data
  muni_name <- unique(subset(munis_df, code_muni == muni)$name_muni)
  muni_uf <- unique(subset(munis_df, code_muni == muni)$abrev_state)

  message(sprintf("Calculating coverage for city %s - %s / %s", muni, muni_name, muni_uf))

  # extract codes of the municipalities in the UCA
  muni_codes <- subset(ucas_df, code_urban_concentration == muni)$code_muni

  # extract grid sf of urban area
  muni_grid <- subset(urban_areas_grid, code_urban_concentration == muni)
  sf::st_transform(urban_areas_grid, csr = 4326)

  grid_files <- list.files("../../data/urbanformbr/ghsl/BUILT/UCA/", pattern = "2014")
  grid_codes <- str_sub(grid_files, 19, 25)

  grid_files_filtered <- grid_files[grid_codes %in% muni]

  if (length(grid_files_filtered) == 0) return (NULL)

  grid_rst <- raster(paste0("../../data/urbanformbr/ghsl/BUILT/UCA/", grid_files_filtered))
  grid_sf <- rasterToPolygons(grid_rst)
  grid_sf <- st_as_sf(grid_sf)
  grid_sf <- st_transform(grid_sf, crs = 4326)

  names(grid_sf) <- c("built", "geometry")
  grid_sf <- subset(grid_sf, built >= 20)

  # load faces de logradouros da UCA
  faces_files <- list.files(paste0("../../data/urbanformbr/faces_de_logradouros/2010/", muni_uf),
                            full.names = F)
  faces_codes <- str_sub(faces_files, 1, 7)

  faces_files_filtered <- faces_files[faces_codes %in% muni_codes]
  faces_files_filtered <-paste0("../../data/urbanformbr/faces_de_logradouros/2010/", muni_uf, "/", faces_files_filtered)

  faces_sf <- map(faces_files_filtered, st_read)
  valid_sfs <- (map(faces_sf, nrow) %>% unlist()) > 0
  faces_sf <- faces_sf[valid_sfs]
  faces_sf <- map(faces_sf, st_cast, to = "MULTILINESTRING")

  faces_sf <- rbindlist(faces_sf)
  faces_sf <- st_as_sf(faces_sf) %>% st_transform(crs = 4326)

  # intersect faces and grid
  intersect_matrix <- st_intersects(grid_sf, faces_sf)
  intersect_vector <- map(intersect_matrix, function(x) return(!is_empty(x))) %>% unlist()


  proportion <- sum(intersect_vector) / length(intersect_vector)

  return_df <- data.frame(code_urban_concentration = muni,
                          total_cells = length(intersect_vector),
                          covered_cells = sum(intersect_vector),
                          proportion)

  return(return_df)

}

# apply function ----------------------------------------------------------

# process_urban_area(4301602)
codes <- unique(ucas_df$code_urban_concentration)
cnefe_coverage_stats <- map_df(codes, calculate_coverage)

write_csv(cnefe_coverage_stats, file = "../../data/urbanformbr/cnefe/coverage_stats.csv")



cnefe_coverage_stats %>%
  summarise(total_cells = sum(total_cells), covered_cells = sum(covered_cells)) %>%
  mutate(proportion_covered = covered_cells / total_cells)


cnefe_coverage_stats %>%
  left_join(munis_df, by = c("code_urban_concentration" = "code_muni")) %>%
  View()



# Geocoding Stats ---------------------------------------------------------


geo_stats <- read_csv(file = "../../data/urbanformbr/cnefe/geocoding_stats.csv")

geo_stats <- geo_stats %>%
  inner_join(ucas_df, by = c("muni_code"="code_muni")) %>%
  filter(code_urban_concentration %in% urbanform_metrics$i_code_urban_concentration) %>%
  group_by(code_urban_concentration, name_urban_concentration) %>%
  summarise(across(.cols = n_addresses:n_missing, sum, na.rm = T))

geo_summary <- geo_stats %>%
  ungroup() %>%
  summarise(across(.cols = n_addresses:n_missing, sum, na.rm = T)) %>%
  mutate(p_success = (lat_lon + logradouros + streetbase) / n_addresses,
         p_logradouros = (lat_lon + logradouros) / n_addresses,
         p_streetbase = streetbase / n_addresses,
         p_failure = n_missing / n_addresses)









