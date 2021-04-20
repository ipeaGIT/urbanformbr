# description -------------------------------------------------------------

# this script
# 1. reads data from built up area cropped for each urban concentration area...
#...for 1975 and 2014
# 2. converts area covered with built up area (value>0) as polygon
# 3. saves the resulting sf as rds for use at urbanformbr

# setup -------------------------------------------------------------------

source('R/setup.R')
library(stars)
library(raster)
library(rgdal)
#library(terra)

# directory ---------------------------------------------------------------

ghsl_dir <- "//storage6/usuarios/Proj_acess_oport/data-raw/ghsl"
