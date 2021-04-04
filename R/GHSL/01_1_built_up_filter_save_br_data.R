# description -------------------------------------------------------------

# this script
# i. reads data from GHS-BUILT (1km resolution)
# ii. filter data from Brazil polygon
# iii. saves data as .rds for future cleaning an manipulation

# setup -------------------------------------------------------------------

source('R/setup.R')
library(stars)
library(raster)

# directory ---------------------------------------------------------------

ghsl_dir <- "//storage6/usuarios/Proj_acess_oport/data-raw/ghsl"


# 1 read data -------------------------------------------------------------
# DO ALL THE YEARS

# * 1.1 read br polygon ---------------------------------------------------

br <- geobr::read_country()


# 2 function and files ------------------------------------------------------


# * 2.1 files -------------------------------------------------------------

files_input <- dir(paste0(ghsl_dir,'/BUILT'), pattern = "1K_V2_0.tif$")

files_output <- gsub('GLOBE','BRASIL', files_input)
files_output <- gsub('.tif','.rds', files_output)

# * 2.2 define function ---------------------------------------------------

f_save_brasil_raster <- function(input, output){

  # read ghsl data
  bua <- stars::read_stars(paste0(ghsl_dir,'/BUILT/', input))

  # read br outside function
  #br <- geobr::read_country()
  # transform br crs to bua crs
  br <- sf::st_transform(br, sf::st_crs(bua))

  # crop raster data using br polygon
  bua_crop <- sf::st_crop(bua, br)

  # create directory
  if (!dir.exists("//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl")){
    dir.create("//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl")
  }

  # save .rds data
  readr::write_rds(
    bua_crop,
    paste0("//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/", output),
    compress = 'gz'
    )

}

# run for multiple years ---------------------------------------------------

#future::plan(future::multisession)
#options(future.globals.maxSize = Inf)

purrr::walk2(.x = files_input, .y = files_output, function(x,y)
  f_save_brasil_raster(input = x, output = y)
  )
