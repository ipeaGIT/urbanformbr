# description -------------------------------------------------------------

# creates raster with the following characteristics:
# value from 1975 and polygon from urban extent 2014 (cutoff 20%)

# setup -------------------------------------------------------------------

source('R/setup.R')

# define function ---------------------------------------------------------

df_pca <- readr::read_rds("../../data/urbanformbr/pca_regression_df/pca_regression_df.rds")

codigos <- unique(df_pca$code_urban_concentration)
nomes <- unique(df_pca$name_uca_case)

#codigo <- 3106200
#nome <- "belo_horizonte_mg"

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
    filename = paste0("//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/BUILT/polygon_urban_extent_2014_cutoff_20_values_1975//GHS_BUILT_polygon_urban_extent_2014_cutoff_20_values_1975_",codigo,"_1K.tif"),
    overwrite = T
  )

  raster::writeRaster(
    x = raster_pop_mask,
    filename = paste0("//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/POP/polygon_urban_extent_2014_cutoff_20_values_1975//GHS_POP_polygon_urban_extent_2014_cutoff_20_values_1975_",codigo,"_1K.tif"),
    overwrite = T
  )


}


# run function ------------------------------------------------------------

### CORRIGIR; FAZER SEPARADO POP E BUILT
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


