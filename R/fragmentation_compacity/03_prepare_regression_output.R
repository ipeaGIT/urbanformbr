#' Script para preparar os resultados para a base de regressão
#'

source('R/setup.R')



fragmentation_df <- read_csv("../../data/urbanformbr/fragmentation_compacity/fragmentation_metrics.csv") %>%
  select(name_uca_case = city, year, n_patches, n_large_patches, proportion_largest_patch) %>%
  filter(year == 2014)

compacity_df <- read_csv("../../data/urbanformbr/fragmentation_compacity/compacity_metrics.csv") %>%
  filter(year == 2014) %>%
  select(code_muni, name_uca_case, year, compacity = avg_cell_closeness_norm) %>%
  mutate(compacity = if_else(compacity > 1, 1, compacity))

metrics_df <- compacity_df %>% left_join(fragmentation_df)

write_csv(metrics_df, file = "../../data/urbanformbr/pca_regression_df/fragmentation_compacity.csv")
read_csv("../../data/urbanformbr/pca_regression_df/fragmentation_compacity.csv") %>% View()

