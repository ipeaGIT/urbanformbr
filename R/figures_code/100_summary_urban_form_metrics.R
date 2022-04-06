#' Script para preparar plots para o relatório

source('R/fun_support/setup.R')
source("R/fun_support/colours.R")

library(ggspatial)
mapviewOptions(platform = "leaflet")

ucas_df <- geobr::read_urban_concentrations() %>% st_set_geometry(NULL)
metrics_df <- readr::read_csv(file = '../../data/urbanformbr/consolidated_data/urbanformbr_metrics_full.csv')

# filtrar apenas cidades grandes
metrics_df <- dplyr::select(metrics_df, starts_with("i_"),
                            x_pop_2010, x_density_pop_02km_2014,
                            x_compacity_2014, x_contiguity_2014, x_land_use_mix,
                            x_intersection_density_km, x_normalized_closeness_centrality_avg,
                            x_circuity_avg) %>%
  arrange(desc(x_pop_2010)) %>%
  slice(1:30)

metrics_long_df <- tidyr::pivot_longer(metrics_df, cols = starts_with("x_"), names_to = "metric")


chosen_cities <- tribble(
  ~i_name_uca_case, ~metric,
  "campo_grande"       , "x_compacity_2014",
  "porto_alegre_rs"    , "x_compacity_2014",
  "baixada_santista_sp", "x_compacity_2014",

  "manaus"         , "x_contiguity_2014",
  "goiania_go"     , "x_contiguity_2014",
  "brasilia_df"    , "x_contiguity_2014"
)

metrics_filtered_df <- left_join(chosen_cities, metrics_long_df,
                                  by = c("i_name_uca_case", "metric"))


# City Centroids
ghsl_df <- read_rds("../../data/urbanformbr/ghsl/results/grid_uca_2014_cutoff20.rds") %>%
  st_transform(crs = 4326)


# x_circuity_avg
# x_intersection_density_km
# x_normalized_closeness_centrality_avg

centroids_df <- tribble(
  ~code_urban_concentration, ~name_uca_case, ~metric, ~value, ~lat, ~lon,

  5300108, "brasilia_df", "x_intersection_density_km", "low"   , -15.899897, -47.930008,
  # 5300108, "brasilia_df", "x_intersection_density_km", "medium", -15.800255, -48.149414,
  4106902, "curitiba_pr", "x_intersection_density_km", "medium", -25.493171, -49.247388,
  5208707, "goiania_go" , "x_intersection_density_km", "high"  , -16.607890, -49.317297,
  # 2611606, "recife_pe"  , "x_intersection_density_km", "high"  ,  -8.113726, -34.895814,
  # 5300108, "brasilia_df", "x_intersection_density_km", "high"  , -15.791341, -48.126920,


  # 1501402, "belem_pa"   , "x_intersection_density_km", "medium",  -1.455202, -48.482889,

  # 3509502, "campinas_sp"      , "x_circuity_avg", "low"   , -22.890973, -47.084762,
  4314902, "porto_alegre_rs"  , "x_circuity_avg", "low"   , -30.008977, -51.202044,
  3106200, "belo_horizonte_mg", "x_circuity_avg", "medium", -19.958544, -43.965403,
  3550308, "sao_paulo_sp"     , "x_circuity_avg", "high"  , -23.464192, -46.284005,

  # 5300108, "brasilia_df"      , "x_normalized_closeness_centrality_avg", "low"   , -15.899897, -47.930008,
  3304557, "rio_de_janeiro_rj", "x_normalized_closeness_centrality_avg", "low"   , -22.913595, -43.519349,
  5300108, "brasilia_df"      , "x_normalized_closeness_centrality_avg", "medium", -15.800255, -48.149414,
  2408102, "natal_rn"         , "x_normalized_closeness_centrality_avg", "high"  ,  -5.791375, -35.209543

  # 3548500, "baixada_santista_sp", -23.94764584661452, -46.33678960789777,
  # 2211001, "teresina_pi"        , -5.096231093442593, -42.80242793737632,
  # 2927408, "salvador_ba"        , -12.974162764218896, -38.476097592809126,

)

centroids_df$value <- factor(centroids_df$value, levels = c("high", "medium", "low"))

# OSM Road Network Metrics ------------------------------------------------

metric_name <- "x_circuity_avg"
metric_long_name <- "Sinuosidade"

plot_road_network_metric <- function(metric_name, metric_long_name) {

  # function for plotting city's road network
  # uca_code <- 3106200
  # uca_name <- "Belo Horizonte/MG"
  plot_road_network <- function(n, df) { #metrics_data, uca_code, uca_name) {
    df <- df[n, ]

    # load osm for UCA
    uca_code <- df$code_urban_concentration[1]
    munis <- filter(ucas_df, code_urban_concentration == uca_code)$code_muni

    osm <- map_df(munis, function(m) {
      osm_partial <- read_rds(sprintf("../../data/urbanformbr/osmdata/rds_intersected/%s.rds", m))
      return(osm_partial)
    })


    # filter metrics
    metrics_data <- filter(metrics_df, i_code_urban_concentration == uca_code)
    uca_name <- metrics_data$i_name_urban_concentration[1]

    # get centroid and buffer around area to be mapped
    centroid <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
    buffer_dist <- 1000
    units(buffer_dist) <- "m"
    buffer <- st_buffer(centroid, buffer_dist)
    b_box <- st_bbox(buffer)


    osm <- st_crop(osm, b_box)

    labels_sf <- data.frame(metric_name = metric_name,
                            city_name = metrics_data$i_name_urban_concentration[[1]],
                            x = b_box["xmin"],
                            y = b_box["ymax"]) %>%
      st_as_sf(coords = c("x", "y"), crs = 4326)


    p <- osm %>%
      mutate(uca = uca_name) %>%
      ggplot() +
      geom_sf() +
      # geom_sf_label(data = labels_sf, aes(label = city_name), hjust = 0, size = 2.8) +
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
  data_filtered <- filter(centroids_df, metric == metric_name) %>%
    arrange(value)

  ps <- purrr::map(1:nrow(data_filtered), plot_road_network, df = data_filtered)

  # ps <- map2(data_filtered$i_code_urban_concentration,
  #            data_filtered$i_name_urban_concentration,
  #            metrics_data = data_filtered,
  #            plot_road_network)
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
      # geom_sf_text(data = labels_sf, aes(label = label), hjust = 0, size = 2.8) +
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

plots_compacity <- plot_landscape_metric(metric_name = "x_compacity_2014", metric_long_name = "Compacidade", size = 45000)
plots_contiguity <- plot_landscape_metric(metric_name = "x_contiguity_2014", metric_long_name = "Contiguidade", size = 45000)


plots_sinuosity[[1]] <- plots_sinuosity[[1]] + labs(subtitle = "Sinuosidade")
plots_intersection_density[[1]] <- plots_intersection_density[[1]] + labs(subtitle = "Densidade de Interseções")
plots_closeness[[1]] <- plots_closeness[[1]] + labs(subtitle = "Centralidade por Proximidade")
plots_compacity[[1]] <- plots_compacity[[1]] + labs(subtitle = "Compacidade")
plots_contiguity[[1]] <- plots_contiguity[[1]] + labs(subtitle = "Contiguidade")

all_plots <- c(plots_compacity, plots_contiguity,
               plots_sinuosity, plots_intersection_density, plots_closeness)


composite_plot <- wrap_plots(all_plots, ncol = 5, byrow = FALSE)
composite_plot

ggsave(here::here("figures", "urban_form_summary.png"),
       plot = composite_plot, width = 297, height = 210,
       units = "mm", dpi = 300)
