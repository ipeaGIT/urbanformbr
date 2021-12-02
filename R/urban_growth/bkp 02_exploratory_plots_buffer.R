#' Script para preparar os resultados para a base de regressão
#'

source('R/setup.R')
mapviewOptions(platform = "leaflet")

library("ggtern")

# load data ---------------------------------------------------------------

urban_extent_processed <- read_rds(file = "../../data/urbanformbr/urban_growth/grid_uca_growth_status_buffer.rds")
geometric_growth_df <- read_rds("../../data/urbanformbr/pca_regression_df/urban_growth_buffer.rds")
population_growth_df <- read_rds(file = "../../data/urbanformbr/urban_growth/population_growth_geometric_status.rds") %>%
  mutate(status = factor(status,
                         levels = c("consolidated", "extension", "infill", "leapfrog"),
                         labels = c("Upward", "Outward", "Inward", "Leapfrog")),
         period = factor(period,
                         levels = c("1975 - 1990", "1990 - 2000", "2000 - 2014", "1975 - 2014"))) %>%
  group_by(code_muni, period) %>%
  mutate(population = sum(pop_end)) %>%
  mutate(porte = cut(population,
                     breaks = c(0, 1000000, 25000000),
                     labels = c("médio", "grande"))) %>%
  ungroup()


population_growth_df %>%
  write_csv("pop_growth.csv")

metrics_2014_df <- metrics_df %>% filter(year == 2014)

# Taxas de crescimento (Boxplots) -----------------------------------------

metrics_2014_df %>%
  count(porte)


### Período 1975 - 2014

population_growth_df %>%
  filter(period == "1975 - 2014") %>%
  ggplot(aes(x=status, y = share_growth, fill = status)) +
  geom_boxplot(position = "dodge") +

  geom_hline(yintercept = 0) +

  scale_fill_brewer(palette = "Set1") +
  scale_y_percent(breaks = seq(-0.05, 0.08, 0.01)) +

  labs(fill = "",
       x = "",
       y = "Growth Rate",
       title = "Taxas de crescimento populacional (geométrico, por ano)",
       subtitle = "Densificação (Upward), Espalhamento (Outward), Preenchimento (Infill) e \nFragmentação (Leapfrog)") +
  theme_light() +
  theme(axis.text.x = element_blank(), legend.position = "bottom")

ggsave(filename = "growth_by_type_buff.png", width = 16, height = 12, units = "cm",
       dpi = 300)

### Por Período

population_growth_df %>%
  ggplot(aes(x=status, y = share_growth, fill = status)) +
  geom_boxplot(position = "dodge") +

  geom_hline(yintercept = 0) +

  scale_fill_brewer(palette = "Set1") +
  scale_y_percent(breaks = seq(-0.01, 0.2, 0.01)) +

  labs(fill = "",
       x = "",
       y = "Growth Rate",
       title = "Taxas de crescimento populacional (geométrico, por ano)",
       subtitle = "Densificação (Upward), Espalhamento (Outward), Preenchimento (Infill) e \nFragmentação (Leapfrog)") +
  theme_light() +
  theme(axis.text.x = element_blank(),
        legend.position = "bottom") +
  facet_wrap(~period, nrow = 1)

ggsave(filename = "growth_by_type_period_buff.png", width = 16, height = 12, units = "cm",
       dpi = 300)

### Por Porte, 1975 - 2014
population_growth_df %>%
  filter(period == "1975 - 2014") %>%
  ggplot(aes(x=status, y = share_growth, fill = status)) +
  geom_boxplot(position = "dodge") +

  geom_hline(yintercept = 0) +

  scale_fill_brewer(palette = "Set1") +
  scale_y_percent(breaks = seq(-0.05, 0.08, 0.01)) +

  labs(fill = "",
       x = "",
       y = "Growth Rate",
       title = "Taxas de crescimento populacional (geométrico, por ano)",
       subtitle = "Densificação (Upward), Espalhamento (Outward), Preenchimento (Infill) e \nFragmentação (Leapfrog)") +
  theme_light() +
  theme(axis.text.x = element_blank(), legend.position = "bottom") +
  facet_wrap(~porte)

ggsave(filename = "growth_by_type_size_buff.png", width = 16, height = 12, units = "cm",
       dpi = 300)

### Por período e porte
population_growth_df %>%
  ggplot(aes(x=status, y = share_growth, fill = status)) +
  geom_boxplot(position = "dodge") +

  geom_hline(yintercept = 0) +

  scale_fill_brewer(palette = "Set1") +
  scale_y_percent(breaks = seq(-0.05, 0.2, 0.01)) +

  labs(fill = "",
       x = "",
       y = "Growth Rate",
       title = "Taxas de crescimento populacional (geométrico, por ano)",
       subtitle = "Densificação (Upward), Espalhamento (Outward), Preenchimento (Infill) e \nFragmentação (Leapfrog)") +
  theme_light() +
  theme(axis.text.x = element_blank(), legend.position = "bottom") +
  facet_grid(porte~period, scales = "free_y")

ggsave(filename = "growth_by_type_size_period_buff.png", width = 16, height = 12, units = "cm",
       dpi = 300)




geometric_growth_df %>%
  filter(period_start == 1975 & period_end == 2014) %>%
  # group_by(period_start, period_end) %>%
  # summarise(across(.cols = starts_with("growth"), .fns = mean), .groups = "drop") %>%
  pivot_longer(cols = starts_with("growth"), names_to = "metric", values_to = "growth") %>%

  filter(metric != "growth_total") %>%

  mutate(period = paste(period_start, period_end, sep = " - ")) %>%
  mutate(metric = factor(metric,
                         levels = c("growth_upward", "growth_infill", "growth_extension", "growth_leapfrog"),
                         labels = c("Upward", "Infill", "Extension", "Leapfrog"))
  ) %>%
  left_join(metrics_2014_df, by = c("code_muni", "name_uca_case")) %>%

  ggplot(aes(x=period, y = growth, fill = metric)) +
  geom_boxplot(position = "dodge") +

  geom_vline(xintercept = 3.5) +

  scale_fill_brewer(palette = "Set1") +
  scale_y_percent() +
  labs(fill = "Growth",
       x = "Period",
       y = "Growth Rate",
       title = "Taxas de crescimento urbano por Densificação (Upward),\nEspalhamento (Extension), Preenchimento (Infill) e \nFragmentação (Leapfrog)") +
  theme_light() +
  facet_wrap(~porte)

ggsave(filename = "growth_by_type_and_size_buff.png", width = 16, height = 12, units = "cm",
       dpi = 300)

# predominant growth ------------------------------------------------------

predominant_growth_df <- geometric_growth_df %>%
  mutate(period = paste(period_start, period_end, sep = " - ")) %>%
  mutate(predominant_growth = case_when(
    growth_upward > growth_expansion & growth_upward > growth_leapfrog ~ "upward",
    growth_expansion > growth_upward & growth_expansion > growth_leapfrog ~ "expansion",
    growth_leapfrog > growth_expansion & growth_leapfrog > growth_upward ~ "leapfrog"
  )) %>%
  filter(period == "1975 - 2014")




# ternary plot ------------------------------------------------------------
geometric_growth_df %>%
  left_join(metrics_2014_df, by = c("code_muni", "name_uca_case")) %>%
  filter(period_start == 1975, period_end == 2014) %>%
  rename(upward = growth_upward,
         infill = growth_infill,
         extension = growth_extension,
         leapfrog = growth_leapfrog) %>%
  ggtern(aes(y=upward, z=leapfrog, x=extension,
             color = population,  size = population )) +
  geom_point() +
  scale_size_continuous(range = c(2, 6)) +
  scale_color_viridis_c(trans = "log") +
  theme_rgbw() +
  theme(legend.position = "none")

ggsave(filename = "ternary_growth_plot.png", width = 26, height = 21,
       units = "cm", dpi = 300)


# densificação vs espraiamento -----------------------------------------------

urbanform_data_df <- left_join(metrics_df, predominant_growth_df, by = c("code_muni", "name_uca_case")) %>%
  select(code_muni, name_uca_case, year, urbanextent, population, density01km,
         n_large_patches, proportion_largest_patch, compacity,
         period, growth_total, growth_upward, growth_outward, growth_inward, predominant_growth)

setDT(urbanform_data_df)

growth_quantiles = quantile(urbanform_data_df$growth_total, probs = c(0, 0.333, 0.666, 1 ))

urbanform_data_df[, growth_speed := cut(urbanform_data_df$growth_total,
                                        growth_quantiles,
                                        include_lowest = TRUE,
                                        labels = c("slow", "medium", "fast"))]


arrow_plot_simple <- function(data, var1, var2, var_facet, t) {
  data <- copy(data)

  data[, v1 := get(var1)]
  data[, v2 := get(var2)]

  data %>%
    drop_na() %>%
    ggplot() +
    geom_path(aes(x=v1, y = v2, group = code_muni),
              arrow = arrow(length = unit(0.25, "cm")), alpha = 0.7, color = "grey20") +

    scale_color_distiller(palette = "Spectral") +
    scale_size_continuous(range = c(0.1, 2)) +
    theme_light() +
    theme(legend.position = "bottom") +
    labs(title = t,
         x = var1,
         y = var2,
         by = var_facet) +
    facet_wrap(~get(var_facet))
}

arrow_plot_simple(data = urbanform_data_df,
                  var1 = "proportion_largest_patch",
                  var2 = "compacity",
                  var_facet = "predominant_growth",
                  t = "Fragmentação vs Compacidade (1975 - 2014)")

ggsave(filename = "frag_vs_comp.png", width = 21, height = 12, units = "cm",
       dpi = 300)

arrow_plot_simple(data = urbanform_data_df,
                  var1 = "proportion_largest_patch",
                  var2 = "density01km",
                  var_facet = "predominant_growth",
                  t = "Fragmentação vs Adensamento (1975 - 2014")

ggsave(filename = "frag_vs_dens.png", width = 21, height = 12, units = "cm",
       dpi = 300)

arrow_plot_simple(data = urbanform_data_df,
                  var1 = "compacity",
                  var2 = "density01km",
                  var_facet = "predominant_growth",
                  t = "Compacidade vs Adensamento (1975 - 2014)")

ggsave(filename = "comp_vs_dens.png", width = 21, height = 12, units = "cm",
       dpi = 300)

arrow_plot_simple(data = urbanform_data_df,
                  var1 = "proportion_largest_patch",
                  var2 = "compacity",
                  var_facet = "growth_speed",
                  t = "Compacidade vs Adensamento (1975 - 2014)")


arrow_plot_faceted <- function(data, var1, var2, t) {
  data <- copy(data)

  data[, v1 := get(var1)]
  data[, v2 := get(var2)]

  data[, v1_range := last(v1) - first(v1), by = code_muni ]
  data[, v2_range := last(v2) - first(v2), by = code_muni ]

  # group variable 1
  v_quantiles = quantile(data$v1_range, probs = c(0, 0.333, 0.666, 1), na.rm = T)
  data[, v1_group := cut(data$v1_range,
                         v_quantiles,
                         include_lowest = TRUE,
                         labels = c("+", "++", "+++"))]

  # group variable 2
  v_quantiles = quantile(data$v2_range, probs = c(0, 0.333, 0.666, 1), na.rm = T)
  data[, v2_group := cut(data$v2_range,
                         v_quantiles,
                         include_lowest = TRUE,
                         labels = c("+", "++", "+++"))]

  data %>%
    drop_na() %>%
    ggplot() +
    geom_path(aes(x=v1, y = v2, group = code_muni),
              arrow = arrow(length = unit(0.25, "cm")), alpha = 0.7, color = "grey20") +

    scale_color_distiller(palette = "Spectral") +
    scale_size_continuous(range = c(0.1, 2)) +
    theme_light() +
    theme(legend.position = "bottom") +
    labs(title = t,
         x = var1,
         y = var2) +
    facet_grid(v2_group~v1_group)
}

arrow_plot_faceted(data = urbanform_data_df,
                  var1 = "proportion_largest_patch",
                  var2 = "compacity",
                  t = "Fragmentação vs Compacidade (1975 - 2014)")
arrow_plot_faceted(data = urbanform_data_df,
                   var1 = "compacity",
                   var2 = "density01km",
                   t = "Fragmentação vs Compacidade (1975 - 2014)")


urbanform_data_df %>%
  ggplot() +
  geom_path(aes(x=get(var1), y = get(var2), group = code_muni),
            arrow = arrow(length = unit(0.25, "cm")), alpha = 0.7, color = "grey20") +

  scale_color_distiller(palette = "Spectral") +
  scale_size_continuous(range = c(0.1, 2)) +
  theme_light() +
  theme(legend.position = "bottom") +
  labs(title = "Espraiamento vs Adensamento (1975 - 2014)",
       x = var1,
       y = var2) +
  facet_wrap(~get(var_facet))
# coord_equal()







  data <- metrics_df %>%
    # mutate(v1 = log(get(var1)) / log(max(get(var1), na.rm = TRUE)),
           # v2 = log(get(var2)) / log(max(get(var2), na.rm = TRUE))) %>%
    mutate(v1 = get(var1), v2 = get(var2)) %>%
    drop_na() %>%
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
         x = "compacidade",
         y = "densidade (exp. 1 km)")
    # coord_equal()
    facet_grid(magnitude_class ~ direction_class)


metrics_df %>%
  ggplot(aes(x=density01km, y=compacity, group = name_uca_case)) +
  geom_path() +
  geom_point(aes(color = year, size = population))



geometric_growth_df


