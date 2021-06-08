# description -------------------------------------------------------------

# this script
# i. reads population raster data from GHS-BUILT-BRASIL (1km resolution)
#..saved previously
# ii. crops spatially using shapes from urban concentration areas by IBGE (uca)
## devtools::install_github("ipeaGIT/geobr", subdir = "r-package")
# iii. saves raster for each uca and each year


# setup -------------------------------------------------------------------

source('R/setup.R')

# directory and files input -----------------------------------------------

# directory
ghsl_pop_dir <- "//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/POP/BRASIL/"

# files input
files_input <- dir(ghsl_pop_dir, pattern = 'BRASIL.*raster.tif$')


# future setting ----------------------------------------------------------
future::plan(future::multicore)


# define function ---------------------------------------------------------

f_crop_uca <- function(input){

  # raster projection -------------------------------------------------------
  pop_br_projection <- rgdal::GDALinfo(paste0(ghsl_pop_dir, input)) %>%
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
    uca_all <- sf::st_transform(uca_all, pop_br_projection)

    # split uca sf into list with a df for each uca
    uca_split <- base::split(uca_all, uca_all$name_uca_case)

  }

  # pop up area brasil data ------------------------------------------

  # read pop brasil raster
  pop_br <- raster::raster(paste0(ghsl_pop_dir, input))

  # crop and mask -----------------------------------------------------------

  # crop raster with each uca sf
  uca_crop <- furrr::future_map(uca_split, ~raster::crop(pop_br, .))

  # mask raster with each uca sf
  uca_mask <- furrr::future_map2(
    .x = uca_crop, .y = uca_split, function(x, y)
      raster::mask(x = x, mask = y)
  )

  #uca_crop <- purrr::map(uca_split, ~raster::crop(pop_br, .))

  #uca_mask <- purrr::map2(
  #  .x = uca_crop, .y = uca_split, function(x, y)
  #    raster::mask(x = x, mask = y)
  #)


  # create directory
  if (!dir.exists("//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/POP/UCA")){
    dir.create("//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/POP/UCA")
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
        filename = paste0('//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/POP/UCA/', y),
        overwrite = T
      )
  )

  #furrr::future_map2(
  #  uca_mask, files_output, function(x,y)
  #    raster::writeRaster(
  #      x = x,
  #      filename = paste0('//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/POP/UCA/', y, '_raster.tif'),
  #      overwrite = T
  #    )
  #)

}




# run for multiple pop years ----------------------------------------------


purrr::walk(files_input, ~f_crop_uca(.))

#furrr::future_walk(files_input, ~f_crop_uca(.))
