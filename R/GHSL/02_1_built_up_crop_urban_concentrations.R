# description -------------------------------------------------------------

# this script
# i. reads built-up area raster data from GHS-BUILT-BRASIL (1km resolution)
#..saved previously at R/GHSL/01_2_built_up_filter_save_br_data.R
# ii. crops and masks spatially using urban concentration areas (uca) shapes by IBGE
## devtools::install_github("ipeaGIT/geobr", subdir = "r-package")
# iii. saves raster for each uca and each year

# Obs.: uca shapes were previously saved at R/urban_concentration_area/01_1_uca_shapes.R

# setup -------------------------------------------------------------------

source('R/fun_support/setup.R')

# setup parallel ----------------------------------------------------------

future::plan(future::multicore, workers = future::availableCores() / 2)

# define function ---------------------------------------------------------

#ano <- 1990

f_crop_built_uca <- function(ano){


  # * read files ------------------------------------------------------------

  # read uca sf saved at urban_concentration_area/01_1_uca_shapes
  urban_shapes <- readr::read_rds('../../data/urbanformbr/urban_area_shapes/urban_area_pop_100000_dissolved.rds')

  # read brasil raster built up area ano
  raster_br_bua <- raster::raster(sprintf("../../data/urbanformbr/ghsl/BUILT/BRASIL/GHS_BUILT_LDS%s_BRASIL_R2018A_54009_1K_V2_0_raster.tif", ano))


  # * change shape projection -----------------------------------------------

  projection_br_bua <- rgdal::GDALinfo(sprintf("../../data/urbanformbr/ghsl/BUILT/BRASIL/GHS_BUILT_LDS%s_BRASIL_R2018A_54009_1K_V2_0_raster.tif", ano)) %>%
    attr('projection')

  # change uca sf projection (using brasil built raster projection)
  urban_shapes <- sf::st_transform(urban_shapes, projection_br_bua)

  codigos <- urban_shapes$code_urban_concentration

  furrr::future_walk(
    codigos,
    function(code_uca){
      message(paste0("\n working on ", code_uca,"\n"))

      # * subset urban_shapes ---------------------------------------------------

      # subset urban_shapes
      df_urban_shape <- subset(urban_shapes, code_urban_concentration == code_uca)

      # * crop & mask raster file ------------------------------------------------

      # crop raster with urban shape
      raster_uca_bua <- raster::crop(raster_br_bua, df_urban_shape)

      # mask raster with urban shape
      raster_uca_bua <- raster::mask(raster_uca_bua, df_urban_shape)

      # * save file ------------------------------------------------------------

      # create output name with input
      name_output <- gsub(
        pattern = "BRASIL",
        replacement = df_urban_shape$code_urban_concentration,
        x = sprintf("GHS_BUILT_LDS%s_BRASIL_R2018A_54009_1K_V2_0_raster.tif", ano)
      )

      # save built up area raster file
      raster::writeRaster(
        raster_uca_bua,
        filename = paste0("../../data/urbanformbr/ghsl/BUILT/UCA/", name_output),
        overwrite = T
      )

    }
  )

}


# run for mulitple years --------------------------------------------------

anos <- c("1990","2000","2014")

furrr::future_walk(anos, ~f_crop_built_uca(.))
