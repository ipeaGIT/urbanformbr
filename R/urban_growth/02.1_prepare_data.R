#' Script para preparar plots para o relatório

source('R/fun_support/setup.R')
source("R/fun_support/colours.R")
library("ggtern")
library("PupillometryR")
mapviewOptions(platform = "leaflet")


# spatial data - cells classified by growth type
urban_extent_processed <- read_rds(file = "../../data/urbanformbr/urban_growth/grid_uca_growth.rds") %>%
  mutate(growth_type = if_else(status == "consolidated", "upward", status)) %>%
  mutate(growth_type = factor(growth_type,
                              levels = c("upward", "infill", "extension", "leapfrog")))

# taxas de crescimento por cidade, período e tipo de crescimento
urban_growth_df <- read_rds("../../data/urbanformbr/urban_growth/urban_growth.rds") %>%
  ungroup() %>%
  mutate(growth_type = factor(growth_type,
                              levels = c("upward", "infill", "extension", "leapfrog")))


# porte da cidade
city_size_df <- urban_growth_df %>%
  ungroup() %>%
  filter(period_start == 2000, period_end == 2014) %>%
  select(code_muni, name_uca_case, pop = pop_end, built = built_end) %>%
  distinct() %>%
  mutate(size = if_else(pop >= 1000000, "grande", "médio")) %>%
  select(code_muni, name_uca_case, size)

urban_growth_df <- left_join(urban_growth_df, city_size_df,
                             by = c("code_muni", "name_uca_case"))

# sinaliza observações como outliers
urban_growth_df <- urban_growth_df %>%
  group_by(period_start, period_end) %>%
  mutate(is_outlier_pop = (pop_geo_growth <= quantile(pop_geo_growth, 0.02, na.rm = T) |
                             pop_geo_growth >= quantile(pop_geo_growth, 0.95, na.rm = T)),
         is_outlier_built = (built_geo_growth <= quantile(built_geo_growth, 0.02, na.rm = T) |
                               built_geo_growth >= quantile(built_geo_growth, 0.95, na.rm = T)))

