#' Script para preparar plots para o relatório

source('R/fun_support/setup.R')
source("R/fun_support/colours.R")
mapviewOptions(platform = "leaflet")


# spatial data - cells classified by growth type
urban_extent_processed <- read_rds(file = "../../data/urbanformbr/urban_growth/grid_uca_growth.rds") %>%
  mutate(growth_type = if_else(status == "consolidated", "upward", status)) %>%
  mutate(growth_type = factor(growth_type,
                              levels = c("upward", "infill", "extension", "leapfrog"))) %>%
  filter(name_uca_case == "porto_alegre_rs") %>%
  filter(period_start == 1975, period_end == 2014)

water_bodies_sf <- st_read("../../data/urbanformbr/urban_growth/rios_porto_alegre_wsg84.gpkg")
land_sf <- geobr::read_state(code_state = "RS")
roads_sf <- st_read("../../data/urbanformbr/urban_growth/rodovias_porto_alegre_wgs84.gpkg") %>%
  filter(LAYER %in% c(1, 6))

# Mapa - Tipos de Crescimento ---------------------------------------------

#' Mapa descritivo dos tipos de crescimento urbano: upward, infill, extension
#' e leapfrog

b_box <- st_bbox(urban_extent_processed)

urban_extent_processed %>%
  ggplot() +
  geom_sf(data = water_bodies_sf, fill = "#d7dfe6", color = "#d7dfe6") +
  geom_sf(data = roads_sf, size = 1, color = "grey40") +
  geom_sf(aes(fill=growth_type), size = 0.2) +
  coord_sf(xlim = c(b_box["xmin"], b_box["xmax"]),
           ylim = c(b_box["ymin"], b_box["ymax"])) +
  scale_fill_aop(palette = "blue_red", reverse = TRUE) +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA, color = "grey80"),
        panel.grid = element_blank()) +
  labs(fill = "tipo",
       title = "Porto Alegre / RS - expansão urbana entre 1974 e 2014")

ggsave(filename = here::here("figures", "types_of_growth.png"),
       width = 10, height = 8, units = "cm", dpi = 300, scale = 1.8)


