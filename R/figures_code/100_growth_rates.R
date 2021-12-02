#' Script para preparar plots para o relatório

source('R/fun_support/setup.R')
source("R/fun_support/colours.R")
mapviewOptions(platform = "leaflet")



# prepare data ------------------------------------------------------------


# taxas de crescimento por cidade, período e tipo de crescimento
urban_growth_df <- read_rds("../../data/urbanformbr/urban_growth/urban_growth.rds") %>%
  ungroup() %>%
  mutate(growth_type = factor(growth_type,
                              levels = c("upward", "infill", "extension", "leapfrog"),
                              labels = c("Adensamento", "Preenchimento", "Extensão", "Leapfrog")))


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


# plot figure -------------------------------------------------------------


# figure
plot_growth_rates_by_period <- function(data, variable, start, end, remove_outliers = T) {

  # filter by period
  data <- subset(data, period_start == start & period_end == end)

  if (remove_outliers == T) {
    if (variable == "pop") {
      data <- base::subset(x = data, is_outlier_pop == F)
    }

    if (variable == "built") {
      data <- base::subset(x = data, is_outlier_built == F)
    }
  }

  # choose growth rate variable
  if (variable == "pop") {
    data$geo_growth <- data$pop_geo_growth
    t = "Taxas de crescimento populacional das cidades brasileiras"
  }
  if (variable == "built") {
    data$geo_growth <- data$built_geo_growth
    t = "Taxas de crescimento físico das cidades brasileiras"
  }
  s <- sprintf("Período de %d a %d", start, end)

  # plot
  data$growth_type <- fct_rev(data$growth_type)
  data$size <- factor(data$size,
                      levels = c("médio", "grande"),
                      labels = c("Cidades Médias", "Cidades Grandes"))

  ggplot(data, aes(x=growth_type , y=geo_growth ,fill= growth_type, color=growth_type)) +
    geom_flat_violin(alpha=.5, position = position_nudge(x = .05, y = 0)) +
    geom_jitter( alpha=.2,  position = position_nudge(x = -.1, y = 0)) +
    geom_boxplot( fill=NA,
                  alpha = 0.3,
                  width = .1,
                  colour = "black",
                  outlier.shape = NA,
                  position = position_dodge(width = 0.9)) +
    scale_y_continuous(labels = percent) +
    scale_fill_aop(palette = "blue_red", reverse = F) +
    scale_colour_aop(palette = "blue_red", reverse = F) +
    facet_wrap(~size, ncol = 2) +
    labs(title = t, subtitle = s) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_rect(colour = "grey40", fill = NA),
          axis.title = element_blank())

}


plot_growth_rates_by_period(data = urban_growth_df,
                            variable = "pop",
                            start = 1975, end = 2014)
ggsave(filename = here::here("figures", "growth_rates_pop.png"),
       width = 12, height = 10, units = "cm", dpi = 300, scale = 1.5)


plot_growth_rates_by_period(data = urban_growth_df,
                            variable = "built",
                            start = 1975, end = 2014)

ggsave(filename = here::here("figures", "growth_rates_built.png"),
       width = 12, height = 10, units = "cm", dpi = 300, scale = 1.5)

