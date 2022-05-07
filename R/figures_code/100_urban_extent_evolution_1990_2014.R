
# description -------------------------------------------------------------

# this script plots urban extent (cutoff 20% of built up area) shape and urban shapes
# (political administrative) to show the evolution of the urban extent from 1990 to 2014


# setup -------------------------------------------------------------------
source("R/fun_support/setup.R")
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
  "Belém/PA"
  , "Belo Horizonte/MG"
  , "Salvador/BA"
  , "Teresina/PI"
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
  # function ----------------------------------------------------------------
f_prepare <- function(ano){

  # ano <- 1990
  # df <- "urban extent"

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


# run function ------------------------------------------------------------
df_extents <- purrr::map_df(
  .x = anos,
  ~f_prepare(ano = .x)
)

df_extents <- df_extents %>%
  dplyr::arrange(code_urban_concentration, year)

# read urban shapes (political administrative shape for all uca)
df_shapes <- readr::read_rds("../../data/urbanformbr/urban_area_shapes/urban_area_pop_100000_dissolved.rds")
df_shapes <- subset(df_shapes, code_urban_concentration %in% codigos)



# reproject ---------------------------------------------------------------
ucas <- geobr::read_urban_concentrations(simplified = F)


df_shapes <- sf::st_transform(df_shapes, crs(ucas))
df_extents <- sf::st_transform(df_extents, crs(ucas))

# centroids ---------------------------------------------------------------

df_shapes <- df_shapes %>%
  mutate(centroid = sf::st_centroid(df_shapes) %>% st_geometry())

padding <- 0.55

# plot --------------------------------------------------------------------
df_shapes <- df_shapes %>%
  dplyr::arrange(name_urban_concentration)

f_plot <- function(code){

  # code <- 3106200
  extents_s <- subset(df_extents, code_urban_concentration == code)

  shapes_s <- subset(df_shapes, code_urban_concentration == code)

  ucas_s <- subset(ucas, code_urban_concentration == code)

  ggplot() +
    geom_sf(
      data = shapes_s
      ,aes()
      , fill = "light grey"
      , colour = "darkgray"
      , size = 0.75
    ) +
    geom_sf(
      data = ucas_s
      ,aes()
      , fill = "light grey"
      , colour = "grey"
    ) +
    geom_sf(
      data = extents_s
      ,aes(fill = forcats::fct_rev(factor(year)))
      , colour = NA
    ) +
    coord_sf(
      xlim = c(
        shapes_s$centroid[[1]][1] - padding,
        shapes_s$centroid[[1]][1] + padding
        )
      ,ylim = c(
        shapes_s$centroid[[1]][2] - padding,
        shapes_s$centroid[[1]][2] + padding
      )
      ,expand = F
    ) +
    #scale_fill_manual(values = )
    scale_fill_viridis_d(option = "inferno",direction = -1) +
    theme_map() +
    #aop_style() +
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
    guides(fill = guide_legend(reverse = T,
                               override.aes = list(colour = "black")
                               ))

}

gg_out <- purrr::map(
  .x = df_shapes$code_urban_concentration,
  ~f_plot(code = .x)
)

a <- purrr::reduce(gg_out, .f = `+`)

#png(here::here("figures", "teste.png"),
#    width = 16, height = 16, units = "cm", res = 300, type = "cairo"
#)

a + patchwork::plot_layout(guides = "collect") & theme(legend.position = "bottom")

#dev.off

# save plot ---------------------------------------------------------------
ggsave(filename = here::here("figures", "urban_extent_evolution.png"),
       width = 16, height = 11, units = "cm", dpi = 300, device = "png")



