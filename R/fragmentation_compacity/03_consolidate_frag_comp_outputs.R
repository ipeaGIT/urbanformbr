#' Script para preparar os resultados para a base de regressão
#'

source('R/fun_support/setup.R')



fragmentation_df <- read_csv("../../data/urbanformbr/fragmentation_compacity/fragmentation_metrics.csv") %>%
  select(code_urban_concentration = city, year, contiguity = proportion_largest_patch) %>%
  pivot_wider(names_from = year, values_from = contiguity, names_prefix = "contiguity_")

compacity_df <- read_csv("../../data/urbanformbr/fragmentation_compacity/compacity_metrics.csv") %>%
  select(code_urban_concentration, year, compacity = compacity_norm) %>%
  mutate(compacity = if_else(compacity > 1, 1, compacity)) %>%
  pivot_wider(names_from = year, values_from = compacity, names_prefix = "compacity_")


metrics_df <- left_join(fragmentation_df, compacity_df)

write_csv(metrics_df, file = "../../data/urbanformbr/consolidated_data/fragmentation_compacity.csv")

# read_csv("../../data/urbanformbr/consolidated_data/fragmentation_compacity.csv") %>% View()

