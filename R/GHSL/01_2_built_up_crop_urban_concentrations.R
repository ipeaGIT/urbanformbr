# description -------------------------------------------------------------

# this script
# i. reads data from GHS-BUILT-BRASIL (1km resolution) saved previously as .rds
# ii. filter data from urban concentration (uc)
# devtools::install_github("ipeaGIT/geobr", subdir = "r-package")
# iii. saves data as .rds for future cleaning an manipulation


# setup -------------------------------------------------------------------

source('R/setup.R')
library(stars)
library(raster)
library(rgdal)
#library(terra)

# directory ---------------------------------------------------------------

ghsl_dir <- "//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/"

# example bhz -------------------------------------------------------------

# read urban concentration data
uc <- geobr::read_urban_concentrations()
# bhz code muni
#geobr::lookup_muni('Belo Horizonte')
# filter bhz
bhz <- uc %>%
  dplyr::filter(code_urban_concentration == 3106200)



# define function ---------------------------------------------------------

files_input <- dir(ghsl_dir, pattern = 'BRASIL.*raster.tif$')

files_output_raster <- gsub('BRASIL','UC_BHZ', files_input)

f_crop_urban_concentration <- function(input, output){

  bua_br <- raster::raster(paste0(ghsl_dir, input))

  bhz <- sf::st_transform(bhz, raster::projection(bua_br))

  bhz_bua <- raster::crop(bua_br, bhz)

  raster::writeRaster(
    x = bhz_bua,
    filename = paste0('//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/', output),
    overwrite = T
  )

}

# multiple years
purrr::walk2(.x = files_input, .y = files_output_raster, function(x,y)
  f_crop_urban_concentration(input = x, output = y)
)



# APAGAR ------------------------------------------------------------------


# read built up area raster data 1975
bua_br <- readr::read_rds('//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/GHS_BUILT_LDS1975_BRASIL_R2018A_54009_1K_V2_0_raster.rds')

# change bhz projection
bhz <- sf::st_transform(bhz, raster::projection(bua_br))

# crop built up raster with bhz uc borders
bhz_bua <- raster::crop(bua_br, bhz)

# save bhz raster
# name files
name_output <- gsub('BRASIL', 'UC_BHZ', dir(ghsl_dir)[[1]])
#
saveRDS(
  bhz_bua,
  paste0(ghsl_dir, name_output),
  compress = 'xz'
)
