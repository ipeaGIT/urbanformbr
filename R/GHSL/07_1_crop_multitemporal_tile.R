
# description -------------------------------------------------------------

# this script uses GHSL multitemporal built data from tile id 10_13 to
# crop the political administrative shape of belo horizonte's metro area

# tile containing belo horizonte: ID 10_13

# setup -------------------------------------------------------------------
source("R/fun_support/setup.R")


# function ----------------------------------------------------------------
funcao <- function(){

  #  read data --------------------------------------------------------------

  # * tile ------------------------------------------------------------------
  tile_10_13 <- raster::raster("../../data-raw/ghsl/BUILT/multitemporal_30m/GHS_BUILT_LDSMT_GLOBE_R2018A_3857_30_V2_0_10_13.tif")

  # * political administrative shape ----------------------------------------

  urban_shapes <- readr::read_rds("../../data/urbanformbr/urban_area_shapes/urban_area_pop_100000_dissolved.rds")
  urban_shapes <- urban_shapes %>%
    dplyr::select(-starts_with("pop"))
  bh <- urban_shapes %>% filter(code_urban_concentration == 3106200)

  # reproject/transform urban shape crs to tile crs
  bh <- sf::st_transform(bh, raster::projection(tile_10_13))

  # crop and mask -----------------------------------------------------------
  # crop raster data using bh polygon
  bh_crop <- raster::crop(tile_10_13, bh)

  # mask raster data
  bh_mask <- raster::mask(bh_crop, bh)

  # save data ---------------------------------------------------------------
  # create directory
  if (!dir.exists("../../data/urbanformbr/ghsl/BUILT/multitemporal")){
    dir.create("../../data/urbanformbr/ghsl/BUILT/multitemporal")
  }

  raster::writeRaster(
    x = bh_crop,
    filename = '../../data/urbanformbr/ghsl/BUILT/multitemporal/GHS_BUILT_LDSMT_GLOBE_R2018A_3857_30_V2_0_belo_horizonte_metro_area_CROP.tif',
    overwrite = T
  )

  raster::writeRaster(
    x = bh_mask,
    filename = '../../data/urbanformbr/ghsl/BUILT/multitemporal/GHS_BUILT_LDSMT_GLOBE_R2018A_3857_30_V2_0_belo_horizonte_metro_area_MASK.tif',
    overwrite = T
  )

}


# run function ------------------------------------------------------------
funcao()

