# description -------------------------------------------------------------

# create grids considering the following characteristicas:
# total_area_grid_uca 1975: polygon urban extent 2014 & raster value 1975
# total_area_grid_uca 2014: polygon urban extent 2014 & raster value 2014

# setup -------------------------------------------------------------------

source('R/fun_support/setup.R')


# define function ---------------------------------------------------------

# year <- 2014

f_create_total_area_uca_grids <- function(year){

  # * read data -------------------------------------------------------------

  # * * read polygons -------------------------------------------------------
  # consolidated area = urban extent 1975
  polygon_consolidated_area <- readr::read_rds("../../data/urbanformbr/ghsl/results/urban_extent_uca_1975_cutoff20.rds")

  # expansion area = st_sym_difference(urban_extent_1975, urban_extent_2014)
  polygon_expansion_area <- readr::read_rds("../../data/urbanformbr/ghsl/results/expansion_area_cutoff20.rds")

  # # total area = urban extent 2014
  polygon_total_area <- readr::read_rds("../../data/urbanformbr/ghsl/results/urban_extent_uca_2014_cutoff20.rds")

  # * * read raster ---------------------------------------------------------

  #year <- 1975
  #code = 3106200
  #name = "belo_horizonte_mg"

  #name <- "internacional_de_foz_do_iguacu_brasil_ciudad_del_este_paraguai"
  #code <- 4108304

  urban_areas_cells <- map2_df(
    .x = polygon_total_area$code_muni, .y = polygon_total_area$name_uca_case,
    function(code, name){
      if(year == 1975){
        raster_built <- paste0(
          "../../data/urbanformbr/ghsl/BUILT/polygon_urban_extent_2014_cutoff_20_values_1975/GHS_BUILT_polygon_urban_extent_2014_cutoff_20_values_1975_",
          name,
          "_1K.tif"
        )
      } else {
        raster_built <- paste0(
          "../../data/urbanformbr/ghsl/BUILT/urban_extent_cutoff_20/GHS_BUILT_LDS2014_urban_extent_cutoff_20_",
          name,
          "_1K_raster.tif"
        )
      }


      if (year == 1975) {
        raster_pop <-paste0(
          "../../data/urbanformbr/ghsl/POP/polygon_urban_extent_2014_cutoff_20_values_1975/GHS_POP_polygon_urban_extent_2014_cutoff_20_values_1975_",
          name,
          "_1K.tif"
        )

      } else {
        y <- 2015
        raster_pop <- paste0(
          "../../data/urbanformbr/ghsl/POP/urban_extent_cutoff_20/GHS_POP_E2015_urban_extent_cutoff_20_",
          name,
          "_1K_raster.tif"
        )

      }


      # read raster data & classify
      urban_area <- raster::raster(raster_built) %>%
        rasterToPolygons() %>%
        st_as_sf() %>%
        mutate(
          code_muni = code,
          name_uca_case = name,
          cell = row_number(),
          consolidada = st_within(
            ., subset(polygon_consolidated_area,name_uca_case==name)) %>%
            map_int(length)
        ) %>%
        select(code_muni, name_uca_case, cell, built = 1, consolidada, geometry)


      urban_pop <- raster(raster_pop) %>%
        rasterToPolygons() %>%
        st_as_sf() %>%
        st_centroid() %>%
        rename(pop = 1)


      urban_area <- st_join(urban_area, urban_pop) %>%
        dplyr::relocate(consolidada, .after = cell)

      return(urban_area)
    }
  )


  write_rds(urban_areas_cells, sprintf("../../data/urbanformbr/ghsl/results/total_area_grid_uca_consolidada_expansao_%s_cutoff20.rds", year))


}




# run function ------------------------------------------------------------
# run for each year present at GHSL

f_create_total_area_uca_grids(1975)
f_create_total_area_uca_grids(2014)


# detectar erro -----------------------------------------------------------

# detectar erro walk2/map2
erro <- purrr::map2(
  polygon_total_area$code_muni, polygon_total_area$name_uca_case,
  purrr::possibly(~funcao(.x,.y),'erro') # incluir funcao para verificar erro
)

# erro -> linha 76 -> 4108304 - internacional_de_foz_do_iguacu_brasil_ciudad_del_este_paraguai

map(erro, class) %>% unique()
teste <- purrr::keep(erro, inherits, 'character')
