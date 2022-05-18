
# description -------------------------------------------------------------

# this script plots urban extent (cutoff 20% of built up area) shape and urban shapes
# (political administrative) to show the evolution of the urban extent from 1990 to 2014


# setup -------------------------------------------------------------------
source("R/fun_support/setup.R")
#library("ggmap")
source("R/fun_support/style.R")
source("R/fun_support/colours.R")

# select ucas -------------------------------------------------------------

#built_growth <- data.table::fread("../../data/urbanformbr/consolidated_data/urban_growth_builtarea.csv")

#pop_growth <- data.table::fread("../../data/urbanformbr/consolidated_data/urban_growth_population.csv")

#censo <- data.table::fread("../../data/urbanformbr/consolidated_data/censo_metrics.csv")

#belem: 1501402
#belo horizonte: 3106200
#campo grande: 5002704
#caxias do sul: 4305108
#salvador: 2927408
#teresina: 2211001

nomes <- c(
  #"Belém/PA",
  "Belo Horizonte/MG"
  , "Salvador/BA"
  #, "Teresina/PI"
)

codigos <- c(
  #1501402
  3106200
  #, 5002704
  #, 4305108
  , 2927408
  #, 2211001
)

anos <- c(1990,2000,2014)
  # function extents ----------------------------------------------------------------
f_prepare <- function(ano){

  # code_uca <- 2927408
  # ano <- 1990

  df_full <- readr::read_rds(sprintf("../../data/urbanformbr/ghsl/results/urban_extent_uca_%s_cutoff20.rds",ano))

  f_subset_bind <- function(code_uca){
    df_subset <- subset(df_full, code_urban_concentration == code_uca)

    df_subset <- df_subset %>%
      dplyr::mutate(year = ano) %>%
      dplyr::relocate("year", .before = geometry)

    return(df_subset)
  }

  df_bind <- purrr::map_df(
    .x = codigos,
    ~f_subset_bind(code_uca = .x)
  )

  return(df_bind)

}


# * run f_extents -----------------------------------------------------------

df_extents <- purrr::map_df(
  .x = anos,
  ~f_prepare(ano = .x)
)

df_extents <- df_extents %>%
  dplyr::arrange(code_urban_concentration, year)

# create column with colour values
df_extents <- df_extents %>%
  dplyr::mutate(
    hex = data.table::fcase(
      year == 1990, "#edf8b1" #"#000004FF"
      , year == 2000, "#7fcdbb" #"#BB3754FF"
      , year == 2014, "#2c7fb8" #FCFFA4FF"
      , default = NA
    )
  )

# read and download data --------------------------------------------------


# * urban shapes (political administrative shape dissolved ucas) ------------

df_shapes <- readr::read_rds("../../data/urbanformbr/urban_area_shapes/urban_area_pop_100000_dissolved.rds")
df_shapes <- subset(df_shapes, code_urban_concentration %in% codigos)

# * ucas (non-dissolved) ----------------------------------------------------

#ucas <- geobr::read_urban_concentrations(simplified = F)


# function tiles ----------------------------------------------------------

# f_tiles <- function(code_uca){
#
#   # code_uca <- 2927408
#   # subset uca
#   df_shapes_s <- subset(df_shapes, code_urban_concentration == code_uca)
#
#   # download tile
#   base_tile <- maptiles::get_tiles(
#     x = df_shapes_s
#     , crop = T
#     , provider = "CartoDB.PositronNoLabels"
#     , zoom = 11
#   )
#
#   # display map
#   #maptiles::plot_tiles(base_tile)
#
#   # convert tile to df
#   df_tile <- terra::as.data.frame(
#     x = base_tile, xy = T
#   ) %>%
#     dplyr::mutate(
#       hex = rgb(red, green, blue, maxColorValue = 255),
#       code_urban_concentration = code_uca
#     ) %>%
#     dplyr::select(-c(red, green, blue))
#
#   return(df_tile)
#
# }

# * run f_tiles -----------------------------------------------------------

#df_tiles <- purrr::map(
#  .x = codigos,
#  ~f_tiles(code_uca = .x)
#)

#names(df_tiles) <- paste0("df_",codigos)


# reproject ---------------------------------------------------------------

#df_shapes <- sf::st_transform(df_shapes, crs(ucas))
#df_extents <- sf::st_transform(df_extents, crs(ucas))

df_shapes <- sf::st_transform(x = df_shapes, crs = 4326)
df_extents <- sf::st_transform(x = df_extents, crs = 4326)
#ucas <- sf::st_transform(x = ucas, crs = 4326)


# centroid and buffer -----------------------------------------------------

df_shapes <- df_shapes %>%
  mutate(centroid = sf::st_centroid(df_shapes) %>% st_geometry())

buffer_dist <- 50000
units(buffer_dist) <- "m"


# plot --------------------------------------------------------------------
#padding <- 0.55

df_shapes <- df_shapes %>%
  dplyr::arrange(name_urban_concentration)

f_plot <- function(code_uca){

  # code_uca <- 2927408
  extents_s <- subset(df_extents, code_urban_concentration == code_uca)

  shapes_s <- subset(df_shapes, code_urban_concentration == code_uca)

  #ucas_s <- subset(ucas, code_urban_concentration == code_uca)

  #df_tiles_s <- pluck(df_tiles, grep(as.character(code_uca), names(df_tiles)))

  # get buffer
  buffer_s <- sf::st_buffer(shapes_s$centroid, dist = buffer_dist)
  b_box_s <- sf::st_bbox(buffer_s)


  # plot
  ggplot() +
    # types of map tile: rosm::osm.types
    ggspatial::annotation_map_tile(
      zoom = 12
      #, zoomin = -1
      , type = "cartolight" #cartodark
    ) +
    geom_sf(
      data = shapes_s
      , aes()
      , fill = NA
    ) +
    geom_sf(
      data = extents_s
      , aes(fill = forcats::fct_rev(factor(year)))
      , colour = NA
      , alpha = 1
    ) +
    ggspatial::annotation_scale(location = "tl") +
    ggspatial::annotation_north_arrow(
      location = "br"
      , height = unit(1, "cm")
      , width = unit(1, "cm")
      ) +
    ggspatial::shadow_spatial(b_box_s) +
    scale_fill_manual(
      values = c(
        "1990" = "#edf8b1"  #
        , "2000" = "#41b6c4" #
        , "2014" = "#253494" #
          )
    ) +
    #scale_fill_brewer(palette = "OrRd", direction = -1) +
    #scale_fill_viridis_d(option = "inferno", direction = 1) +
    theme_map() +
    theme(
      axis.line.x = element_blank()
      ,axis.text = element_blank()
      ,legend.position = "bottom"
      ,panel.border = element_rect(fill = NA, color = "grey80")
      ,legend.text = element_text(size = 10)
      ,legend.title = element_blank()
      #,panel.spacing.y = unit(-.5, "cm")
      #,plot.margin = unit(c(0.25,0.25,0.25,0.25), "cm")
      #,text = element_text(family = "Helvetica", colour = "#808080", size = 14),
    ) +
    labs(
      subtitle = shapes_s$name_urban_concentration
      , fill = "Ano"
    ) +
    guides(
      fill = guide_legend(reverse = F, override.aes = list(colour = "black"))
    )

}

gg_out <- purrr::map(
  .x = codigos,
  ~f_plot(code_uca = .x)
)

# patchwork::wrap_plots()
gg_final <- purrr::reduce(gg_out, .f = `+`)

#png(here::here("figures", "teste.png"),
#    width = 16, height = 16, units = "cm", res = 300, type = "cairo"
#)

gg_final +
  patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

#dev.off


# save plot ---------------------------------------------------------------
ggsave(filename = here::here("figures", "urban_extenet_evolution.png"),
       width = 16, height = 11, units = "cm", dpi = 300, device = "png")



