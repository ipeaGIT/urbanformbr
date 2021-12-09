#' Script para preparar plots para o relatório

source('R/fun_support/setup.R')
source("R/fun_support/colours.R")
mapviewOptions(platform = "leaflet")

library(ggtern)

# prepare data ------------------------------------------------------------


# taxas de crescimento por cidade, período e tipo de crescimento
urban_growth_df <- read_rds("../../data/urbanformbr/urban_growth/urban_growth.rds") %>%
  ungroup() %>%
  mutate(growth_type = factor(growth_type,
                              levels = c("upward", "infill", "extension", "leapfrog"))) %>%
  select(code_muni, name_uca_case, period_start, period_end,
         growth_type, pop_geo_growth, population = pop_end) %>%
  pivot_wider(names_from = growth_type, values_from = pop_geo_growth, values_fill = 0) %>%
  mutate(adensamento = upward + infill, extensao = extension)


# ternary plot ------------------------------------------------------------
urban_growth_df %>%
  filter(period_start == 1990, period_end == 2014) %>%
  ggtern(aes(y=adensamento, x=leapfrog, z=extensao,
             color = population,  size = population )) +
  geom_point() +
  scale_size_continuous(range = c(2, 6)) +
  scale_color_viridis_c(trans = "log") +
  theme_rgbw() +
  theme(legend.position = "none") +
  labs(title = "Participação de cada tipo de crescimento urbano no total",
       subtitle = "período de 1990 a 2014")



ggsave(filename = "figures/ternary_growth_plot.png", width = 26, height = 21,
       units = "cm", dpi = 300)



# scatterplot -------------------------------------------------------------
urban_growth_df %>%
  mutate(period = paste(period_start, period_end, sep = " - ")) %>%
  mutate(period = factor(period, levels = c('1990 - 2000' , '2000 - 2014', '1990 - 2014'))) %>%
  mutate(extensao = extensao + leapfrog) %>%
  arrange(population) %>%
  ggplot(aes(x=adensamento, y=extensao)) +
  geom_point(size = 0.8) +
  geom_abline() +
  coord_fixed(xlim=c(0,0.11), ylim=c(0,0.11)) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  # scale_size_continuous(range = c(2, 6)) +
  theme(aspect.ratio=1) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Taxas de crescimeto populacional (1990 - 2014",
       x = "Crescimento por Adensamento + Preenchimento",
       y = "Crescimento por\n Extensão + Leapfrog") +
  facet_wrap(~period, nrow = 1)

ggsave(filename = "figures/scatterplot_growth_type.png", width = 20, height = 10,
       units = "cm", dpi = 300)


