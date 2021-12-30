#' Script para preparar os resultados para a base de regressão
#'

source('R/fun_support/setup.R')

urban_growth_df <- read_rds("../../data/urbanformbr/urban_growth/urban_growth.rds") %>%
  ungroup()


# Population Growth -------------------------------------------------------

urban_growth_clean_df <- urban_growth_df %>%
  filter(period_start == 1990, period_end == 2014) %>%
  select(code_urban_concentration, name_uca_case, period_start, period_end,
         growth_type, pop_geo_growth) %>%
  mutate(growth_type = paste0(growth_type, "_growth_1990_2014")) %>%
  pivot_wider(names_from = growth_type, values_from = pop_geo_growth, values_fill = 0) %>%
  select(-period_start, -period_end)

write_csv(urban_growth_clean_df,
          file = "../../data/urbanformbr/consolidated_data/urban_growth_population.csv")


# Built Area Growth -------------------------------------------------------

urban_growth_clean_df <- urban_growth_df %>%
  filter(period_start == 1990, period_end == 2014) %>%
  select(code_urban_concentration, name_uca_case, period_start, period_end,
         growth_type, built_geo_growth) %>%
  mutate(growth_type = paste0(growth_type, "_growth_1990_2014")) %>%
  pivot_wider(names_from = growth_type, values_from = built_geo_growth, values_fill = 0) %>%
  select(-period_start, -period_end)

write_csv(urban_growth_clean_df,
          file = "../../data/urbanformbr/consolidated_data/urban_growth_builtarea.csv")



