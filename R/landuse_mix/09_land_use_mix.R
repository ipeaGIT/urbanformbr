# description -------------------------------------------------------------

# this script processes the geocoded land use data and calculates the land use
# mix metric, which is based on the Dissimilarity index

# setup -------------------------------------------------------------------

source('R/fun_support/setup.R')
library("segregr")

# list of Brazilian municipalities
munis_df <- geobr::lookup_muni("all")
ucas_df <- geobr::read_urban_concentrations() %>% st_set_geometry(NULL)

urban_areas <- read_rds("../../data/urbanformbr/ghsl/results/grid_uca_2014_cutoff20.rds")


# Grid 1km ----------------------------------------------------------------

# uca <- 4314902 # Porto Alegre
# uca <- 4317103
merge_uca_grid <- function(uca) {
  uca_name = unique(subset(urban_areas, code_urban_concentration == uca)$name_uca_case)

  if (!is_empty(uca_name)) {
    input_file <- paste0("../../data/urbanformbr/cnefe/uca/", uca, "_", uca_name, ".gpkg")
    output_file <- paste0("../../data/urbanformbr/cnefe/grid/", uca, "_", uca_name, ".gpkg")

    if (!file.exists(output_file)) {
      message(sprintf("Processing urban concentration %s - %s", uca, uca_name))

      cnefe_uca_sf <- st_read(input_file)
      grid_uca_sf <- urban_areas %>% filter(code_urban_concentration == uca) %>% st_transform(crs = 4674)

      cnefe_uca_sf <- st_join(cnefe_uca_sf, grid_uca_sf)

      uses_by_cell_df <- cnefe_uca_sf %>%
        st_set_geometry(NULL) %>%
        group_by(code_urban_concentration, name_uca_case, cell, landuse_id) %>%
        summarise(count = n(), .groups = "drop") %>%
        filter(!is.na(cell)) %>%
        pivot_wider(names_from = landuse_id,
                    names_prefix = "use_",
                    names_sort = TRUE,
                    values_from = count,
                    values_fill = 0) %>%
        inner_join(grid_uca_sf) %>%
        st_as_sf()

      if (!dir.exists("../../data/urbanformbr/cnefe/grid/")) {
        dir.create("../../data/urbanformbr/cnefe/grid/")
      }

      st_write(uses_by_cell_df, output_file)
    }

  }
}

ucas <- unique(urban_areas$code_urban_concentration)
walk(ucas, merge_uca_grid)




# Calculate diversity grid ------------------------------------------------

# uca <- 4314902 # Porto Alegre

calculate_diversity <- function(uca) {
  if (!dir.exists("../../data/urbanformbr/cnefe/grid_diversity/")) {
    dir.create("../../data/urbanformbr/cnefe/grid_diversity/")
  }

  uca_name = unique(subset(urban_areas, code_urban_concentration == uca)$name_uca_case)

  if (!is_empty(uca_name)) {
    input_file <- paste0("../../data/urbanformbr/cnefe/grid/", uca, "_", uca_name, ".gpkg")
    output_file <- paste0("../../data/urbanformbr/cnefe/grid_diversity/", uca, "_", uca_name, ".gpkg")
    output_file_5km <- paste0("../../data/urbanformbr/cnefe/grid_diversity/", uca, "_", uca_name, "_5km.gpkg")
    output_file_10km <- paste0("../../data/urbanformbr/cnefe/grid_diversity/", uca, "_", uca_name, "_10km.gpkg")
    output_file_15km <- paste0("../../data/urbanformbr/cnefe/grid_diversity/", uca, "_", uca_name, "_15km.gpkg")

      message(sprintf("Processing urban concentration %s - %s", uca, uca_name))

      ## load grid with landuse data
      grid_uca_sf <- st_read(input_file)

      ## prepare data and calculate segregation metrics
      segreg_input_sf <- grid_uca_sf %>% select(id = cell, use_01:use_06)
      segreg_output <- segregr::measure_segregation(segreg_input_sf, bandwidths = c(0, 5000, 10000, 15000))

      ## prepare metrics outputs
      global_df <- global_metrics_to_df(segreg_output) %>%
        mutate(code_urban_concentration = uca, name_uca_case = uca_name) %>%
        select(code_urban_concentration, name_uca_case, bw, dissimilarity, entropy, theil_h = h) %>%
        mutate(bw = factor(bw, levels = c(0, 5000, 10000, 15000), labels = c("0km", "5km", "10km", "15km"))) %>%
        pivot_wider(names_from = bw, values_from = dissimilarity:theil_h) %>%
        select(code_urban_concentration, name_uca_case,
               dissimilarity = dissimilarity_0km, dissimilarity_5km, dissimilarity_10km, dissimilarity_15km,
               entropy = entropy_0km, theil_h = theil_h_0km)

      local_sf <- local_metrics_to_sf(segreg_output) %>%
        st_set_geometry(NULL) %>%
        mutate(code_urban_concentration = uca, name_uca_case = uca_name) %>%
        select(code_urban_concentration, name_uca_case, bw, cell = id, dissimilarity, entropy, theil_h = h)
      grid_uca_sf <- grid_uca_sf %>% left_join(local_sf)

      avg_entropy <- mean(local_sf$entropy)
      global_df$avg_entropy <- avg_entropy

      if (!file.exists(output_file)) {
        ## save local diversity metrics
        st_write(grid_uca_sf %>% filter(bw == 0), output_file)
        st_write(grid_uca_sf %>% filter(bw == 5000), output_file_5km)
        st_write(grid_uca_sf %>% filter(bw == 10000), output_file_10km)
        st_write(grid_uca_sf %>% filter(bw == 15000), output_file_15km)
      }

      ## return global diversity metrics
      return(global_df)

  }
}

ucas <- unique(urban_areas$code_urban_concentration)
diversity_df <- map_df(ucas, calculate_diversity)


landuse_mix_df <- diversity_df %>%
  mutate(land_use_mix = 1 - dissimilarity) %>%
  select(code_urban_concentration, name_uca_case, land_use_mix)


write_csv(diversity_df, "../../data/urbanformbr/cnefe/dissimilarity_metrics.csv")
write_csv(landuse_mix_df, "../../data/urbanformbr/consolidated_data/landuse_mix.csv")



# diversity_df %>%
#   left_join(ucas_df, by = c("code_urban_concentration" = "code_urban_concentration")) %>%
#   select(pop_total_2010, dissimilarity) %>%
#   cor()
#
#
#
#   ggplot() + geom_point(aes(pop_total_2010, dissimilarity)) +
#   scale_x_log10() +
#   scale_y_log10()
#
#   urban_areas %>% group_by(code_urban_concentration) %>%
#     summarise(built = sum(built))



