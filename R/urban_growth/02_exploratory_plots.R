#' Script para preparar os resultados para a base de regressão
#'

source('R/setup.R')
mapviewOptions(platform = "leaflet")



# load data ---------------------------------------------------------------

urban_extent_processed <- read_rds(file = "../../data/urbanformbr/urban_growth/grid_uca_growth_status.rds")
geometric_growth_df <- read_rds("../../data/urbanformbr/pca_regression_df/urban_growth.rds")

geometric_growth_df %>%
  # filter(!(period_start == 1975 & period_end == 2014)) %>%
  group_by(period_start, period_end) %>%
  summarise(across(.cols = starts_with("growth"), .fns = mean), .groups = "drop") %>%
  pivot_longer(cols = starts_with("growth"), names_to = "metric", values_to = "growth") %>%

  mutate(period = paste(period_start, period_end, sep = " - ")) %>%

  mutate(metric = factor(metric,
                         levels = c("growth_total", "growth_upward", "growth_inward", "growth_outward"),
                         labels = c("Total", "Upward", "Inward", "Outward")),
         period = factor(period,
                         levels = c("1975 - 1990", "1990 - 2000", "2000 - 2014", "1975 - 2014"))
         ) %>%

  ggplot(aes(x=period, y = growth, fill = metric)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_percent() +
  labs(fill = "Growth",
       x = "Period",
       y = "Growth Rate")


  ggplot(aes(x=period, group = code_muni)) +
  geom_path(aes(y=growth_inward), color = "red") +
  geom_path(aes(y=growth_outward), color = "blue")
  scale_y_log10()

