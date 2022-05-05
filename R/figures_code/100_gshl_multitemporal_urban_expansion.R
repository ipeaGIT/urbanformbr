
# description -------------------------------------------------------------

# this script uses GHSL multitemporal built data from belo horizonte metro area
# to create a map of urban expansion from 1975 to 2015

# tile containing belo horizonte: ID 10_13

# setup -------------------------------------------------------------------
source("R/fun_support/setup.R")


# function ----------------------------------------------------------------
funcao <- function(){

  #  read data --------------------------------------------------------------

  # * tile ------------------------------------------------------------------
  bh_metro <- raster::raster("../../data/urbanformbr/ghsl/BUILT/multitemporal/GHS_BUILT_LDSMT_GLOBE_R2018A_3857_30_V2_0_belo_horizonte_metro_area_CROP.tif")

  # * urban extent ----------------------------------------
  urban_extent <- readr::read_rds("../../data/urbanformbr/ghsl/results/urban_extent_uca_2014_cutoff20.rds")
  bh_extent <- urban_extent %>%
    filter(code_urban_concentration == 3106200)

  # reproject/transform urban extent crs to tile crs
  bh_extent <- sf::st_transform(bh_extent, raster::projection(bh_metro))


  # bh metro area shapes ----------------------------------------------------
  rmbh_shapes <- geobr::read_urban_concentrations(simplified = F)
  rmbh_shapes <- rmbh_shapes %>%
    dplyr::filter(code_urban_concentration == 3106200)

  # reproject/transform urban shape crs to tile crs
  rmbh_shapes <- sf::st_transform(rmbh_shapes, raster::projection(bh_metro))


  # crop and mask -----------------------------------------------------------
  # crop raster data using bh polygon
  bh_crop <- raster::crop(tile_10_13, bh)

  # mask raster data
  bh_mask <- raster::mask(bh_crop, bh)



  # plot ---------------------------------------------------------------


  # * using ggplot ----------------------------------------------------------

  bh_spdf <- as(bh_metro, "SpatialPixelsDataFrame")
  bh_df <- as.data.frame(bh_spdf)
  colnames(bh_df) <- c("value", "x", "y")

  # check values
  data.table::setDT(bh_df)
  bh_df[,logical(1),by=value]$value %>% sort()

  666666
  # land no built up any epoch = 2 -> replace with NA, drop, then fill = NA
  bh_df[value==2, value := NA]

  ggplot() +
    geom_tile(
      data = bh_df
      , aes(x = x, y = y, fill = as.factor(value))
      , alpha = 0.8
    ) +
    geom_sf(
      data = rmbh_shapes
      ,aes()
      ,fill = NA
      , alpha = 0.8
    ) +
    scale_fill_viridis_d(option = "viridis",na.value = "black")


  # * using tmap ------------------------------------------------------------



  # save plot ---------------------------------------------------------------




}


# run function ------------------------------------------------------------
funcao()

