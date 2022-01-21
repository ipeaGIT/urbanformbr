#' Script para preparar plots para o relatório

source('R/fun_support/setup.R')
source("R/fun_support/colours.R")

library(ggspatial)
mapviewOptions(platform = "leaflet")

metrics_df <- readr::read_csv(file = '../../data/urbanformbr/consolidated_data/urbanformbr_metrics_full.csv')

# aqui, possivelmente filtrar apenas cidades grandes

metrics_df <- dplyr::select(metrics_df, starts_with("i_"),
                            x_pop_2010, x_density_pop_02km_2014,
                            x_compacity_2014, x_contiguity_2014, x_land_use_mix,
                            x_intersection_density_km, x_normalized_closeness_centrality_avg,
                            x_circuity_avg) %>%
  arrange(desc(x_pop_2010)) %>%
  slice(1:20)

metrics_long_df <- tidyr::pivot_longer(metrics_df, cols = starts_with("x_"), names_to = "metric")

metrics_filtered_df <- metrics_long_df %>%
  group_by(metric) %>%
  arrange(desc(value)) %>%
  slice(c(1, n()/2, n()))


# City Centroids
ghsl_df <- read_rds("../../data/urbanformbr/ghsl/results/grid_uca_2014_cutoff20.rds") %>%
  st_transform(crs = 4326)

centroids_df <- ghsl_df %>%
  group_by(code_urban_concentration) %>%
  arrange(desc(built)) %>%
  slice(1) %>%
  st_centroid()

centroids_df <- tribble(
  ~code_urban_concentration, ~name_uca_case, ~lat, ~lon,
  3106200, "belo_horizonte_mg", -19.958544536819858, -43.965403455388696,
  3509502, "campinas_sp", -22.90898211616576, -47.063063036305095,
  3548500, "baixada_santista_sp", -23.94764584661452, -46.33678960789777,
  2211001, "teresina_pi", -5.096231093442593, -42.80242793737632,
  1501402, "belem_pa", -1.4552025447811645, -48.48288973633754,
  2927408, "salvador_ba", -12.974162764218896, -38.476097592809126,
  2408102, "natal_rn", -5.791375445088197, -35.209543506286295
)

# OSM Road Network Metrics ------------------------------------------------

metric_name <- "x_circuity_avg"
metric_long_name <- "Sinuosidade"

plot_road_network_metric <- function(metric_name, metric_long_name) {

  # function for plotting city's road network
  # uca_code <- 3303906
  # uca_name <- "Petrópolis/RJ"
  plot_road_network <- function(metrics_data, uca_code, uca_name) {
    # load osm for UCA
    osm <- read_rds(sprintf("../../data/urbanformbr/osmdata/rds_intersected/%s.rds", uca_code))

    metrics_data <- filter(metrics_data, i_code_urban_concentration == uca_code)

    centroid <- filter(centroids_df, code_urban_concentration == uca_code) %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326)
    buffer_dist <- 1000
    units(buffer_dist) <- "m"
    buffer <- st_buffer(centroid, buffer_dist)
    b_box <- st_bbox(buffer)


    labels_sf <- data.frame(metric_name = metric_name,
                            metric_value = metrics_data$value[[1]],
                            label = paste0(metric_long_name, " = ", comma(metrics_data$value[[1]], accuracy = 0.001)),
                            x = b_box["xmin"],
                            y = b_box["ymax"]) %>%
      st_as_sf(coords = c("x", "y"), crs = 4326)


    p <- osm %>%
      mutate(uca = uca_name) %>%
      ggplot() +
      geom_sf() +
      geom_sf_label(data = labels_sf, aes(label = label), hjust = 0, size = 2.8) +
      coord_sf(xlim = c(b_box["xmin"], b_box["xmax"]),
               ylim = c(b_box["ymin"], b_box["ymax"]),
               datum = NA) +
      facet_wrap(~uca) +
      theme_minimal() +
      theme(strip.text = element_text(face = "bold", hjust = 0),
            panel.background = element_rect(fill = NA, color = "grey40"),
            axis.text = element_blank(),
            axis.title = element_blank()) +
      annotation_scale(location = "br", width_hint = 0.5, style = "ticks")

    p
    return(p)
  }

  # filter metric values from full dataframe
  data_filtered <- filter(metrics_filtered_df, metric == metric_name)

  ps <- map2(data_filtered$i_code_urban_concentration,
             data_filtered$i_name_urban_concentration,
             metrics_data = data_filtered,
             plot_road_network)
  p <- patchwork::wrap_plots(ps, ncol = 1) + plot_annotation(title = metric_long_name)

  return(ps)

}



# GHSL landscape metrics --------------------------------------------------


# metric_name <- "x_compacity_2014"
# metric_long_name <- "Compacidade"
# size = 50000

## find bounding box
cities_sf <- st_read("../../data/urbanformbr/fragmentation_compacity/compacity_2014.gpkg") %>%
  filter(code_urban_concentration %in% metrics_filtered_df$i_code_urban_concentration)



plot_landscape_metric <- function(metric_name, metric_long_name, size) {


  # data <- cities_sf
  # metrics_data <- data_filtered
  # uca_code <- 1302603
  # uca_code <- 5300108 # Brasília
  # uca_name <- "Manaus"
  # size <- 50000

  plot_raster <- function(data, metrics_data, uca_code, uca_name, size) {
    data2 <- filter(data, code_urban_concentration == uca_code)
    metrics_data <- filter(metrics_data, i_code_urban_concentration == uca_code)

    centroid <- st_centroid(data2)
    buffer_dist <- size
    units(buffer_dist) <- "m"
    buffer <- st_buffer(centroid, buffer_dist)
    b_box <- st_bbox(buffer)


    labels_sf <- data.frame(metric_name = metric_name,
                            metric_value = metrics_data$value[[1]],
                            label = paste0(metric_long_name, " = ", comma(metrics_data$value[[1]], accuracy = 0.001)),
                            x = b_box["xmin"],
                            y = b_box["ymax"]) %>%
      st_as_sf(coords = c("x", "y"), crs = 4326)


    p <- data2 %>%
      mutate(uca = uca_name) %>%
      ggplot() +
      geom_sf(size = 0.3) +
      geom_sf_text(data = labels_sf, aes(label = label), hjust = 0, size = 2.8) +
      coord_sf(xlim = c(b_box["xmin"], b_box["xmax"]),
               ylim = c(b_box["ymin"], b_box["ymax"]),
               datum = NA) +
      facet_wrap(~uca) +
      theme_minimal() +
      theme(strip.text = element_text(face = "bold", hjust = 0),
            panel.background = element_rect(fill = NA, color = "grey40"),
            axis.text = element_blank(),
            axis.title = element_blank()) +
      annotation_scale(location = "br", width_hint = 0.5, style = "ticks")

    return(p)
  }


  # plot_raster(cities_sf, uca_code = 1500107, uca_name = "Abaetetuba", size, size)

  # filter metric values from full dataframe
  data_filtered <- filter(metrics_filtered_df, metric == metric_name)


  ps <- map2(data_filtered$i_code_urban_concentration,
             data_filtered$i_name_urban_concentration,
             .f = plot_raster,
             data = cities_sf,
             metrics_data = data_filtered,
             size = size)


  return(ps)
}



# Plots -------------------------------------------------------------------


plots_sinuosity <- plot_road_network_metric("x_circuity_avg", "Sinuosidade")
plots_intersection_density <- plot_road_network_metric("x_intersection_density_km", "D. Interseções")
plots_closeness <- plot_road_network_metric("x_normalized_closeness_centrality_avg", "Centralidade")

plots_compacity <- plot_landscape_metric(metric_name = "x_compacity_2014", metric_long_name = "Compacidade", size = 50000)
plots_contiguity <- plot_landscape_metric(metric_name = "x_contiguity_2014", metric_long_name = "Contiguidade", size = 50000)


plots_sinuosity[[1]] <- plots_sinuosity[[1]] + labs(subtitle = "Sinuosidade")
plots_intersection_density[[1]] <- plots_intersection_density[[1]] + labs(subtitle = "Densidade de Interseções")
plots_closeness[[1]] <- plots_closeness[[1]] + labs(subtitle = "Centralidade por Proximidade")
plots_compacity[[1]] <- plots_compacity[[1]] + labs(subtitle = "Compacidade")
plots_contiguity[[1]] <- plots_contiguity[[1]] + labs(subtitle = "Contiguidade")

all_plots <- c(plots_compacity, plots_contiguity,
               plots_sinuosity, plots_intersection_density, plots_closeness)


composite_plot <- wrap_plots(all_plots, ncol = 5, byrow = FALSE)
composite_plot

ggsave("urban_form_plots_4.png", plot = composite_plot, width = 297, height = 210,
       units = "mm", dpi = 300)
