source("R/setup.R")

### Load regression data
regression_df <- read_rds("../../data/urbanformbr/pca_regression_df/pca_regression_df_ready_to_use.rds")




source("R/landscape_metrics/urban_form_analysis/01_prepare_data.R")

metrics_df %>%
  filter(year == "2014") %>%
  summary()

metrics_2014_df <- metrics_df %>% filter(year == "2014")

metrics_df %>%
  filter(year == "2014") %>%
  ggplot(aes(x=avg_cell_distance, y = urbanextent, color = proportion_largest_patch, size = population)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

regression_df %>%
  mutate(x_urban_extent_size_2014 = as.numeric(x_urban_extent_size_2014)) %>%
  ggplot(aes(x=x_urban_extent_size_2014, y=y_wghtd_mean_commute_time)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

regression_df %>%
  ggplot(aes(x=x_pop_2015, y=y_wghtd_mean_commute_time)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

regression_df %>%
  select(x_wghtd_mean_household_income_per_capita, x_urban_extent_size_2014, x_pop_2015,
         y_wghtd_mean_commute_time, y_fuel_consumption_per_capita_2010) %>%
  cor() %>%
  View()

regression_df <- regression_df %>%
  mutate(pop_log = log(x_pop_2015),
         urbanextent_log = log(x_urban_extent_size_2014),
         commute_log = log(y_wghtd_mean_commute_time),
         fuel_log = log(y_fuel_consumption_per_capita_2010),
         income_log = log(x_wghtd_mean_household_income_per_capita))

lm(y_fuel_consumption_per_capita_2010 ~ x_wghtd_mean_household_income_per_capita + x_prop_autos_dom, regression_df) %>% summary()
lm(fuel_log ~ income_log + x_prop_autos_dom, regression_df) %>% summary()


lm(y_wghtd_mean_commute_time ~ x_pop_2015 * urbanextent_log, regression_df) %>% summary()
lm(commute_log ~ pop_log + urbanextent_log, regression_df) %>% summary()
lm(y_wghtd_mean_commute_time ~ pop_log * urbanextent_log, regression_df) %>% summary()



lm(avg_cell_distance ~ urbanextent, metrics_2014_df)
metrics_2014_df %>%
  select(avg_cell_distance, urbanextent) %>%
  cor()

lm(avg_cell_distance ~ density05km, metrics_2014_df)
metrics_2014_df %>%
  select(avg_cell_distance, density05km) %>%
  cor()

lm(urbanextent ~ density05km, metrics_2014_df)
metrics_2014_df %>%
  select(urbanextent, density05km) %>%
  cor()


urbanform_reg <- lm(density10km ~ avg_cell_distance * urbanextent, metrics_2014_df)
summary(urbanform_reg)


## Densidade vs Espraiamento, Fragmentação e População

metrics_df %>%
  filter(year == "2014") %>%
  ggplot(aes(y=density05km, x = avg_cell_distance, color = proportion_largest_patch, size = population)) +
  # geom_path(aes(x=density10km, y = avg_cell_distance, group = code_muni)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_distiller(palette = "Spectral", direction = 1) +
  geom_hline(yintercept = 461.19) +
  geom_vline(xintercept = 5.629) +
  labs(title = "Densidade vs Espraiamento, Fragmentação e População") +
  theme_light()



metrics_df %>%
  filter(year == "2014") %>%
  select(urbanextent, avg_cell_distance) %>%
  drop_na() %>%
  cor()


plot_vars <- function(data, var1, var2) {
  data %>%
    ggplot() +
    geom_path(aes(x=get(var1), y = get(var2), group = code_muni)) +
    geom_point(aes(x=get(var1), y = get(var2), color = year)) +
    # scale_x_log10() +
    # scale_y_log10() +
    theme(legend.position = "none") +
    labs(title = paste(var1, var2, sep = " x "),
         x = var1, y = var2)
}

plot_vars_log <- function(data, var1, var2) {
  data %>%
    ggplot() +
    geom_path(aes(x=get(var1), y = get(var2), group = code_muni)) +
    geom_point(aes(x=get(var1), y = get(var2), color = year)) +
    scale_x_log10() +
    scale_y_log10() +
    theme(legend.position = "none") +
    labs(title = paste(var1, var2, sep = " x "),
         x = var1, y = var2)
}

plot_vars(metrics_df, "urbanextent", "saturation")
plot_vars(metrics_df, "urbanextent", "population")
plot_vars(metrics_df, "urbanextent", "density05km")
plot_vars(metrics_df, "urbanextent", "density10km")
plot_vars(metrics_df, "urbanextent", "n_large_patches")
plot_vars(metrics_df, "urbanextent", "proportion_largest_patch")
plot_vars(metrics_df, "urbanextent", "avg_cell_distance")

plot_vars_log(metrics_df, "urbanextent", "saturation")
plot_vars_log(metrics_df, "urbanextent", "population")
plot_vars_log(metrics_df, "urbanextent", "density05km")
plot_vars_log(metrics_df, "urbanextent", "density10km")
plot_vars_log(metrics_df, "urbanextent", "n_large_patches")
plot_vars_log(metrics_df, "urbanextent", "proportion_largest_patch")
plot_vars_log(metrics_df, "urbanextent", "avg_cell_distance")

plot_vars(metrics_df, "n_large_patches", "avg_cell_distance")
plot_vars_log(metrics_df, "n_large_patches", "avg_cell_distance")


plot_vars_log(metrics_df, "avg_cell_distance", "density05km")

variables <- names(metrics_df)[4:11]

for (v1 in 1:(length(variables) - 1) ) {
  for (v2 in v1:length(variables)) {
    fname <- paste0(paste(variables[v1], variables[v2], sep = "_x_"), ".png")
    p <- plot_vars(metrics_df, variables[v1], variables[v2])
    ggsave(p, filename = fname, width = 21, height = 21, dpi = 300, units = "cm")

    fname <- paste0("log_", paste(variables[v1], variables[v2], sep = "_x_"), ".png")
    p <- plot_vars_log(metrics_df, variables[v1], variables[v2])
    ggsave(p, filename = fname, width = 21, height = 21, dpi = 300, units = "cm")
  }
}








