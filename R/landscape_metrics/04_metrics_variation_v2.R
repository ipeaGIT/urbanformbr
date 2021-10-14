source("R/setup.R")

### Load regression data
regression_df <- read_rds("../../data/urbanformbr/pca_regression_df/pca_regression_df_ready_to_use.rds")

### clusters
clusters_df <- read_csv("../../data/urbanformbr/pca_regression_df/cluster_output/cluster_1.csv")


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
  data <- metrics_df %>%
    mutate(v1 = log(get(var1)) / log(max(get(var1), na.rm = TRUE)),
           v2 = log(get(var2)) / log(max(get(var2), na.rm = TRUE))) %>%
    group_by(year) %>%
    mutate(avg_v1 = mean(v1, na.rm = TRUE), avg_v2 = mean(v2, ra.rm = TRUE)) %>%
    group_by(name_uca_case) %>%
    arrange(year) %>%
    mutate(vec1 = last(v1) - first(v1),
           vec2 = last(v2) - first(v2)) %>%
    # mutate(v1 = last(get(var1) - first(get(var1))),
    #        v2 = last(get(var2) - first(get(var2)))) %>%
    mutate(magnitude = sqrt(vec1^2 + vec2^2)) %>%
    mutate(direction = atan(vec1 / vec2)) %>%
    # ungroup() %>%
    # arrange(magnitude) %>%
    # ggplot(aes(magnitude)) + geom_histogram()

    # View()
    mutate(direction = round(direction, digits = 1)) %>%
    mutate(direction_class = case_when(direction <= -0.5 ~ "left",
                                       direction <= 0.5 ~ "top",
                                       TRUE ~ "right")) %>%
    mutate(magnitude_class = case_when(magnitude <= 0.1 ~ "weak",
                                       magnitude <= 0.2 ~ "medium",
                                       TRUE ~ "strong")) %>%
    mutate(direction_class = factor(direction_class,
                                    levels = c("left", "top", "right"),
                                    labels = c("+ compacta", "estável", "+ espraiada")),
           magnitude_class = factor(magnitude_class,
                                    levels = c("strong", "medium", "weak"),
                                    labels = c("+++ densa", "++ densa", "+ densa")))
    # filter(direction == 0.6) %>%

  cities <- data %>%
    filter(year == "2014") %>%
    group_by(direction_class, magnitude_class) %>%
    arrange(desc(population)) %>%
    sample_n(3, replace = TRUE) %>%
    # slice(1:3) %>%
    .$name_uca_case %>%
    unique()

  main_data <- data %>% filter(name_uca_case %in% cities)

  data %>%
    ggplot() +
    # geom_point(aes(x=v1, y = v2)) +
    geom_path(aes(x=get(var1), y = get(var2), group = code_muni),
              arrow = arrow(length = unit(0.25, "cm")), alpha = 0.7, color = "grey20") +
    geom_path(data = main_data, aes(x=get(var1), y = get(var2), group = code_muni),
              arrow = arrow(length = unit(0.25, "cm")),
              alpha = 0.7, color = "blue", size = 1) +
    geom_text(data = main_data %>% filter(year == "2014"),
              aes(label = name_uca_case, x=get(var1), y = get(var2)),
              vjust = 0) +
    # geom_path(aes(x=avg_v1, y = avg_v2),
    #           arrow = arrow(length = unit(0.25, "cm")), alpha = 0.7, color = "red") +

    scale_x_log10() +
    scale_y_log10() +
    # gghighlight(magnitude > 0.3, label_key = name_uca_case) +
    # scale_color_distiller() +
    scale_color_distiller(palette = "Spectral") +
    scale_size_continuous(range = c(0.1, 2)) +
    theme_light() +
    theme(legend.position = "bottom") +
    labs(title = "Espraiamento vs Adensamento (1975 - 2015)",
         x = "espraiamento (avg_cell_distance)",
         y = "densidade experienciada 5 km") +
      # coord_equal()
    facet_grid(magnitude_class ~ direction_class)
}

plot_vars_log_cluster <- function(data, var1, var2) {
  data %>%
    left_join(clusters_df, by = c("code_muni" = "i_code_urban_concentration")) %>%
    ggplot() +
    geom_path(aes(x=get(var1), y = get(var2), group = code_muni)) +
    geom_point(aes(x=get(var1), y = get(var2), color = year)) +
    scale_x_log10() +
    scale_y_log10() +
    theme(legend.position = "none") +
    labs(title = paste(var1, var2, sep = " x "),
         x = var1, y = var2) +
    facet_wrap( ~ cluster4)
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
plot_vars_log_cluster(metrics_df, "avg_cell_distance", "density05km")

plot_vars(metrics_df, "avg_cell_distance", "density05km")

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

metrics_df %>%
  pivot_wider(names_from = year, values_from = urbanextent:avg_cell_distance) %>%
  View()


plot_vars_group <- function(data, var1, var2) {
  data %>%
    group_by(name_uca_case) %>%
    arrange(year) %>%
    mutate(diff_var1 = last(get(var1)) - first(get(var1)),
           diff_var2 = last(get(var2)) - first(get(var2)),
           magnitude = max( max(get(var1)) / min(get(var1)),
                            max(get(var2)) / min(get(var2))), # max(abs(diff_var1), abs(diff_var2)),
           sign_var1 = sign(diff_var1),
           sign_var2 = sign(diff_var2),
           group = paste(sign_var1, sign_var2)) %>%
    ungroup() %>%
    mutate(mag_var1 = diff_var1 / max(diff_var1, na.rm = TRUE),
           mag_var2 = diff_var2 / max(diff_var2, na.rm = TRUE)) %>%
    mutate(magnitude = map2_dbl(mag_var1, mag_var2, function(a, b) max(a, b))) %>%

    ggplot() +
    geom_segment(aes(x=0, y=0, xend = diff_var1, yend = diff_var2,
                     size = magnitude, alpha = magnitude)) +
    # geom_path(aes(x=get(var1), y = get(var2), group = code_muni)) +
    # geom_point(aes(x=get(var1), y = get(var2), color = year)) +
    # scale_x_log10() +
    # scale_y_log10() +
    scale_size_continuous(range = c(0.1, 1), trans = "exp") +
    theme(legend.position = "none") +
    labs(title = paste(var1, var2, sep = " x "),
         x = var1, y = var2)
    # facet_wrap( ~ group)
}

plot_vars_group(metrics_df, "avg_cell_distance", "density05km")




