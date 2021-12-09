# load packages and data
source("R/setup.R")


### Built Area

builtarea_raw_df <- read_rds("../../data/urbanformbr/pca_regression_df/area.rds")


builtarea_df <- builtarea_raw_df %>% select(code_muni, name_uca_case,
                                            urbanextent_1975 = urban_extent_size_1975,
                                            urbanextent_2014 = urban_extent_size_2014,
                                            saturation_1975 = saturation_total_area_fixed_1975,
                                            saturation_2014 = saturation_total_area_fixed_2014) %>%
  mutate(urbanextent_1975 = as.numeric(urbanextent_1975),
         urbanextent_2014 = as.numeric(urbanextent_2014)) %>%
  pivot_longer(cols = urbanextent_1975:saturation_2014, names_to = "metric", values_to = "value") %>%
  separate(metric, into = c("metric", "year"))

rm(builtarea_raw_df)

### Population

pop_df <- read_rds("../../data/urbanformbr/pca_regression_df/1970-2015_pop.rds") %>%
  select(code_muni = code_urban_concentration, year = ano, population = pop)

density_raw_df <- read_rds("../../data/urbanformbr/pca_regression_df/exp_density_ghsl.rds")

density_df <- density_raw_df %>%
  mutate(density01km_1975 = pop_total_total_1975 / built_total_total_1975,
         density01km_2014 = pop_total_total_2014 / built_total_total_2014) %>%
  select(code_muni, name_uca_case,
         density05km_1975 = density_pop_05km_total_1975,
         density10km_1975 = density_pop_10km_total_1975,
         density05km_2014 = density_pop_05km_total_2014,
         density10km_2014 = density_pop_10km_total_2014,
         density01km_1975, density01km_2014) %>%
  pivot_longer(cols = starts_with("density"), names_to = "metric", values_to = "value") %>%
  separate(metric, into = c("metric", "year"))

rm(density_raw_df)

### Fragmentation and Compacity

fragmentation_df <- read_csv("../../data/urbanformbr/fragmentation_compacity/fragmentation_metrics.csv")
compacity_df <- read_csv("../../data/urbanformbr/fragmentation_compacity/compacity_metrics.csv")

urbanform_df <- left_join(fragmentation_df, compacity_df, by = c("city" = "name_uca_case", "year")) %>%
  select(code_muni, name_uca_case = city, year, n_large_patches, proportion_largest_patch, avg_cell_closeness_norm) %>%
  mutate(year = as.character(year),
         compacity = if_else(avg_cell_closeness_norm > 1, 1, avg_cell_closeness_norm))

rm(fragmentation_df)
rm(compacity_df)

### Regression
regression_df <- read_rds("../../data/urbanformbr/pca_regression_df/pca_regression_df_ready_to_use.rds")

### Combine dataframes
builtarea_wide_df <- builtarea_df %>% pivot_wider(names_from = metric, values_from = value)
pop_wide_df <- pop_df %>% mutate(year = case_when(year == 1970 ~ "1975",
                                                  year == 2015 ~ "2014"))
density_wide_df <- density_df %>% pivot_wider(names_from = metric, values_from = value)

metrics_df <- builtarea_wide_df %>% left_join(pop_wide_df) %>%
  left_join(density_wide_df) %>%
  left_join(urbanform_df) %>%
  mutate(porte = cut(population,
                     breaks = c(0, 300000, 1000000, 25000000),
                     labels = c("pequeno", "médio", "grande")))


