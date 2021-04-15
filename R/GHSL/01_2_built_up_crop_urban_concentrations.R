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


# directory and files input -----------------------------------------------

# directory
ghsl_built_dir <- "//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/BUILT/BRASIL/"

# files input
files_input <- dir(ghsl_built_dir, pattern = 'BRASIL.*raster.tif$')


# future setting ----------------------------------------------------------
future::plan(future::multicore)


# define function ---------------------------------------------------------

f_crop_uca <- function(input){

  # raster projection -------------------------------------------------------
  bua_br_projection <- rgdal::GDALinfo(paste0(ghsl_built_dir, input)) %>%
    attr('projection')


  # uca shape ----------------------------------------------------------
  if (!exists(x = 'uca_all')) {

    # read uca sf saved at 01_01
    uca_all <- readr::read_rds('//storage6/usuarios/Proj_acess_oport/data/urbanformbr/urban_area_shapes/urban_area_pop_100000_dissolved.rds')

    # add column with clean uca
    uca_all <- uca_all %>%
      mutate(name_uca_case = janitor::make_clean_names(name_urban_concentration))

    # arrange uca sf (name order with name_uca_case)
    uca_all <- uca_all %>%
      dplyr::arrange(name_uca_case)

    # change uca sf projection (using raster projection)
    uca_all <- sf::st_transform(uca_all, bua_br_projection)

    # split uca sf into list with a df for each uca
    uca_split <- base::split(uca_all, uca_all$name_uca_case)

  }

  # built up area brasil data ------------------------------------------

  # read bua brasil raster
  bua_br <- raster::raster(paste0(ghsl_built_dir, input))

  # crop and mask -----------------------------------------------------------

  # crop raster with each uca sf
  uca_crop <- furrr::future_map(uca_split, ~raster::crop(bua_br, .))

  # mask raster with each uca sf
  uca_mask <- furrr::future_map2(
    .x = uca_crop, .y = uca_split, function(x, y)
      raster::mask(x = x, mask = y)
  )

  #uca_crop <- purrr::map(uca_split, ~raster::crop(bua_br, .))

  #uca_mask <- purrr::map2(
  #  .x = uca_crop, .y = uca_split, function(x, y)
  #    raster::mask(x = x, mask = y)
  #)


  # create directory
  if (!dir.exists("//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/BUILT/UCA")){
    dir.create("//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/BUILT/UCA")
  }

  # create output name with input
  files_output <- purrr::map_chr(
    uca_all$name_uca_case,
    ~gsub('BRASIL', ., input)
  )

  # write each mask as raster file
  purrr::walk2(
    uca_mask, files_output, function(x,y)
      raster::writeRaster(
        x = x,
        filename = paste0('//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/BUILT/UCA/', y),
        overwrite = T
      )
  )

  #furrr::future_map2(
  #  uca_mask, files_output, function(x,y)
  #    raster::writeRaster(
  #      x = x,
  #      filename = paste0('//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/BUILT/UCA/', y, '_raster.tif'),
  #      overwrite = T
  #    )
  #)

}




# run for multiple bua years ----------------------------------------------


purrr::walk(files_input, ~f_crop_uca(.))

#furrr::future_walk(files_input, ~f_crop_uca(.))
