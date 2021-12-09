#' Script para preparar plots para o relatório de forma urbana


# Carregar pacotes e dados-------------------------------------------------
source("R/urban_growth/02.1_prepare_data.R")


# Mapa - Tipos de Crescimento ---------------------------------------------

#' Mapa descritivo dos tipos de crescimento urbano: upward, infill, extension
#' e leapfrog

urban_extent_processed %>%
  filter(name_uca_case == "porto_alegre_rs") %>%
  filter(period_start == 1990, period_end == 2014) %>%
  ggplot() +
  geom_sf(aes(fill=growth_type), size = 0.2) +
  # scale_fill_brewer(palette = "Set1") +
  scale_fill_aop(palette = "blue_red", reverse = TRUE) +
  theme_minimal() +
  labs(fill = "tipo",
       title = "Porto Alegre / RS - expansão urbana entre 1990 e 2014")

ggsave(filename = here::here("plots/urbangrowth", "growth_types.png"),
       width = 10, height = 8, units = "cm", dpi = 300, scale = 1.8)

# Taxas de crescimento por tamanho de cidade


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
    coord_flip() +
    labs(title = t, subtitle = s) +
    theme_minimal() +
    theme(legend.position = "none",
          panel.border = element_rect(colour = "grey40", fill = NA),
          axis.title = element_blank())

}


plot_growth_rates_by_period(data = urban_growth_df,
                            variable = "pop",
                            start = 1990, end = 2014)
ggsave(filename = here::here("plots/urbangrowth", "growth_rates_pop.png"),
       width = 14, height = 10, units = "cm", dpi = 300, scale = 1.5)


plot_growth_rates_by_period(data = urban_growth_df,
                            variable = "built",
                            start = 1990, end = 2014)

ggsave(filename = here::here("plots/urbangrowth", "growth_rates_built.png"),
       width = 14, height = 10, units = "cm", dpi = 300, scale = 1.5)


urban_growth_df %>%
  group_by(name_uca_case, period_start, period_end) %>%
  summarise(pop_growth = sum(pop_geo_growth, na.rm = T), built_growth = sum(built_geo_growth, na.rm = T)) %>%
  View()

