# description -------------------------------------------------------------

# this script
# i. reads data from GHS-POP (1km resolution)
# ii. filter data from Brazil polygon
# iii. saves brasil raster .tif for for future cleaning and manipulation

# TO DO LIST:
## FIX OR ERASE STARS FUNCTION
## CHECK THE NEED TO STACK RASTER/STARS FILES ON TOP OF EACH OTHER
## EXPAND THE FUNCTION TO OTHER RESOLUTIONS?

# setup -------------------------------------------------------------------

source('R/setup.R')

# directory ---------------------------------------------------------------

ghsl_dir <- "//storage6/usuarios/Proj_acess_oport/data-raw/ghsl"

# 1 read polygon data -----------------------------------------------------

br <- geobr::read_country()

# 2 function and files ----------------------------------------------------

# * 2.1 files -------------------------------------------------------------

files_input <- dir(paste0(ghsl_dir,'/POP'), pattern = "1K_V1_0.tif$")

files_output <- gsub('GLOBE','BRASIL', files_input)

files_output_raster <- gsub('.tif','_raster.tif', files_output)



# * 2.2 function raster ---------------------------------------------------

f_save_brasil_raster <- function(input, output){

  # read ghsl data
  pop <- raster::raster(paste0(ghsl_dir,'/POP/', input))

  # read br outside function
  #br <- geobr::read_country()
  # transform br crs to pop crs
  br <- sf::st_transform(br, raster::projection(pop))

  # crop raster data using br polygon
  pop_crop <- raster::crop(pop, br)

  # mask raster data
  pop_mask <- raster::mask(pop_crop, br)
  ## erro (que aparentemente nao atrapalha o processo)
  ##  Error in (function (x)  : tentativa de aplicar uma não-função

  # code below not run, but can be used to check
  # according to ghsl data documentation, nodata value = -200
  # check if there is any negative value (which would compromise area calc)
  ## any(pop_mask[pop_mask<0])

  # create directory
  if (!dir.exists("//STORAGE6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/POP/")){
    dir.create("//STORAGE6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/POP/")
  }

  if (!dir.exists("//STORAGE6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/POP/BRASIL")){
    dir.create("//STORAGE6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/POP/BRASIL")
  }

  # don't uses save raster files as .rds.
  # See https://stackoverflow.com/a/48512398 for details
  #saveRDS(
  #  pop_crop,
  #  paste0('//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/',output),
  #  compress = 'xz'
  #  )

  raster::writeRaster(
    x = pop_mask,
    filename = paste0('//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/POP/BRASIL/', output),
    overwrite = T
  )

}


# * * 2.2.1 run multiple years --------------------------------------------

#future::plan(future::multisession)
#options(future.globals.maxSize = Inf)

purrr::walk2(.x = files_input, .y = files_output_raster, function(x,y)
  f_save_brasil_raster(input = x, output = y)
)
