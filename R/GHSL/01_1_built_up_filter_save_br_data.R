# description -------------------------------------------------------------

# this script
# i. reads data from GHS-BUILT (1km resolution)
# ii. filter data from Brazil polygon
# iii. saves brasil raster .tif for for future cleaning and manipulation

# setup -------------------------------------------------------------------

source('R/setup.R')

# directory ---------------------------------------------------------------

ghsl_dir <- "../../data-raw/ghsl"

# 1 read polygon data -----------------------------------------------------

br <- geobr::read_country()

# 2 function and files ----------------------------------------------------

# * 2.1 files -------------------------------------------------------------

files_input <- dir(paste0(ghsl_dir,'/BUILT'), pattern = "1K_V2_0.tif$")

files_output <- gsub('GLOBE','BRASIL', files_input)

files_output_raster <- gsub('.tif','_raster.tif', files_output)


# * 2.3 function raster ---------------------------------------------------

f_save_brasil_raster <- function(input, output){

  # read ghsl data
  bua <- raster::raster(paste0(ghsl_dir,'/BUILT/', input))

  # read br outside function
  #br <- geobr::read_country()
  # transform br crs to bua crs
  br <- sf::st_transform(br, raster::projection(bua))

  # crop raster data using br polygon
  bua_crop <- raster::crop(bua, br)

  # mask raster data
  bua_mask <- raster::mask(bua_crop, br)

  # code below not run, but can be used to check
  # according to ghsl data documentation, nodata value = -200
  # check if there is any negative value (which would compromise area calc)
  ## any(bua_mask[bua_mask<0])

  # create directory
  if (!dir.exists("../../data/urbanformbr/ghsl/BUILT/BRASIL")){
    dir.create("../../data/urbanformbr/ghsl/BUILT/BRASIL")
  }

  # don't uses save raster files as .rds.
  # See https://stackoverflow.com/a/48512398 for details
  #saveRDS(
  #  bua_crop,
  #  paste0('../../data/urbanformbr/ghsl/',output),
  #  compress = 'xz'
  #  )

  raster::writeRaster(
    x = bua_mask,
    filename = paste0('../../data/urbanformbr/ghsl/BUILT/BRASIL/', output),
    overwrite = T
    )

}


# * * 2.2.1 run multiple years --------------------------------------------

#future::plan(future::multisession)
#options(future.globals.maxSize = Inf)

purrr::walk2(.x = files_input, .y = files_output_raster, function(x,y)
  f_save_brasil_raster(input = x, output = y)
)
