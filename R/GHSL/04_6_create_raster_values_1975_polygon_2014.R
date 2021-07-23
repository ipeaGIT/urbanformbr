# description -------------------------------------------------------------

# creates raster with the following characteristics:
# value from 1975 and polygon from urban extent 2014 (cutoff 20%)

# setup -------------------------------------------------------------------

source('R/setup.R')

# define function ---------------------------------------------------------

f_raster_all <- function(){

  # read 2014 polygon
  polygon_urban_extent_2014 <- readr::read_rds("../../data/urbanformbr/ghsl/results/urban_extent_uca_2014_cutoff20.rds")
  polygon_urban_extent_2014 <- split(polygon_urban_extent_2014, polygon_urban_extent_2014$name_uca_case)

  # raster files
  raster_built_files <- dir("../../data/urbanformbr/ghsl/BUILT/UCA/", pattern = "1975", full.names = T)
  raster_built_files <- raster_built_files[!str_detect(raster_built_files,'santa_cruz_do_sul_rs')]

  raster_pop_files <- dir("../../data/urbanformbr/ghsl/POP/UCA/", pattern = "1975", full.names = T)
  raster_pop_files <- raster_pop_files[!str_detect(raster_pop_files,'santa_cruz_do_sul_rs')]


  raster_built <- purrr::map(raster_built_files, ~raster::raster(.))
  names_built <- purrr::map_chr(
    raster_built_files,
    ~stringr::str_extract(., "(?<=LDS1975_).+(?=_R2018A)")
  )
  names(raster_built) <- names_built

  raster_pop <- purrr::map(raster_pop_files, ~raster::raster(.))
  names_pop <- purrr::map_chr(
    raster_pop_files,
    ~stringr::str_extract(., "(?<=E1975_).+(?=_R2019A)")
  )
  names(raster_pop) <- names_pop

  # reorder list to match order
  raster_pop <- raster_pop[names(raster_built)]

  polygon_urban_extent_2014 <- polygon_urban_extent_2014[names(raster_built)]

  # crop and mask raster from polygon

  raster_built_crop <- purrr::map2(
    .x = raster_built, .y = polygon_urban_extent_2014,
    function(x,y)
      raster::crop(x = x, y = y)
  )

  raster_built_mask <- purrr::map2(
    .x = raster_built_crop, .y = polygon_urban_extent_2014,
    function(x,y)
      raster::mask(x = x, mask = y)
  )

  raster_pop_crop <- purrr::map2(
    .x = raster_pop, .y = polygon_urban_extent_2014,
    function(x,y)
      raster::crop(x = x, y = y)
  )

  raster_pop_mask <- purrr::map2(
    .x = raster_pop_crop, .y = polygon_urban_extent_2014,
    function(x,y)
      raster::mask(x = x, mask = y)
  )

  # save data
  purrr::walk2(
    .x = raster_built_mask, .y = names_built,
    function(x,y)
      raster::writeRaster(
        x = x,
        filename = paste0("../../data/urbanformbr/ghsl/BUILT/polygon_urban_extent_2014_cutoff_20_values_1975/GHS_BUILT_polygon_urban_extent_2014_cutoff_20_values_1975_", y, "_1K.tif"),
        overwrite = T
      )
  )

  purrr::walk2(
    .x = raster_pop_mask, .y = names_pop,
    function(x,y)
      raster::writeRaster(
        x = x,
        filename = paste0("../../data/urbanformbr/ghsl/POP/polygon_urban_extent_2014_cutoff_20_values_1975/GHS_POP_polygon_urban_extent_2014_cutoff_20_values_1975_", y, "_1K.tif"),
        overwrite = T
      )
  )


}



# run function ------------------------------------------------------------
f_raster_all()


# metodo antigo (corrigir walk2) ------------------------------------------


df_pca <- readr::read_rds("../../data/urbanformbr/pca_regression_df/pca_regression_df.rds")

codigos <- unique(df_pca$code_urban_concentration)
nomes <- unique(df_pca$name_uca_case)

#codigo <- 3106200
#nome <- "belo_horizonte_mg"

#codigo <- 4108304
#nome <- "internacional_de_foz_do_iguacu_brasil_ciudad_del_este_paraguai"



f_raster_values_1975_pol_2014 <- function(codigo, nome) {

  message(paste0("\n working on ", nome,"\n"))

  # read 2014 polygon
  polygon_urban_extent_2014 <- readr::read_rds("../../data/urbanformbr/ghsl/results/urban_extent_uca_2014_cutoff20.rds")

  # read full uca raster
  raster_built <- raster::raster(paste0("../../data/urbanformbr/ghsl/BUILT/UCA/GHS_BUILT_LDS1975_",nome,"_R2018A_54009_1K_V2_0_raster.tif"))

  raster_pop <- raster::raster(paste0("../../data/urbanformbr/ghsl/POP/UCA/GHS_POP_E1975_",nome,"_R2019A_54009_1K_V1_0_raster.tif"))

  df_subset <- subset(polygon_urban_extent_2014, name_uca_case == nome)

  # crop and mask raster from polygon

  raster_built_crop <- raster::crop(x = raster_built, y = df_subset)
  raster_built_mask <- raster::mask(x = raster_built_crop, mask = df_subset)

  raster_pop_crop <- raster::crop(x = raster_pop, y = df_subset)
  raster_pop_mask <- raster::mask(x = raster_pop_crop, mask = df_subset)

  # save data
  raster::writeRaster(
    x = raster_built_mask,
    filename = paste0("../../data/urbanformbr/ghsl/BUILT/polygon_urban_extent_2014_cutoff_20_values_1975/GHS_BUILT_polygon_urban_extent_2014_cutoff_20_values_1975_",nome,"_1K.tif"),
    overwrite = T
  )

  raster::writeRaster(
    x = raster_pop_mask,
    filename = paste0("../../data/urbanformbr/ghsl/POP/polygon_urban_extent_2014_cutoff_20_values_1975/GHS_POP_polygon_urban_extent_2014_cutoff_20_values_1975_",nome,"_1K.tif"),
    overwrite = T
  )


}



purrr::walk2(
  codigos, nomes,
  function(x,y)
    f_raster_values_1975_pol_2014(codigo = codigos, nome = nomes)
  )

# detectar erro walk2/map2
erro <- purrr::map2(
  .x = codigos, .y = nomes,
  purrr::possibly(~f_raster_values_1975_pol_2014(.x,.y),'erro')
)

map_chr(erro, class) %>% unique()
teste <- purrr::keep(erro, inherits, 'character')





