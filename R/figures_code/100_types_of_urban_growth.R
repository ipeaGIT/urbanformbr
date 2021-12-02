#' Script para preparar plots para o relatório

source('R/fun_support/setup.R')
source("R/fun_support/colours.R")
mapviewOptions(platform = "leaflet")


# spatial data - cells classified by growth type
urban_extent_processed <- read_rds(file = "../../data/urbanformbr/urban_growth/grid_uca_growth.rds") %>%
  mutate(growth_type = if_else(status == "consolidated", "upward", status)) %>%
  mutate(growth_type = factor(growth_type,
                              levels = c("upward", "infill", "extension", "leapfrog")))



# Mapa - Tipos de Crescimento ---------------------------------------------

#' Mapa descritivo dos tipos de crescimento urbano: upward, infill, extension
#' e leapfrog

urban_extent_processed %>%
  filter(name_uca_case == "porto_alegre_rs") %>%
  filter(period_start == 1975, period_end == 2014) %>%
  ggplot() +
  geom_sf(aes(fill=growth_type), size = 0.2) +
  # scale_fill_brewer(palette = "Set1") +
  scale_fill_aop(palette = "blue_red", reverse = TRUE) +
  theme_minimal() +
  labs(fill = "tipo",
       title = "Porto Alegre / RS - expansão urbana entre 1974 e 2014")

ggsave(filename = here::here("figures", "types_of_growth.png"),
       width = 10, height = 8, units = "cm", dpi = 300, scale = 1.8)


