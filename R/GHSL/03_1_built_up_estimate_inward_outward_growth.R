# description -------------------------------------------------------------

# this script
# i. reads data from BRASIL-GHS-BUILT and POP from urban concentration areas (1km)
# ii. estimate inward and outward population growth

# TO DO LIST:
## SAVE POPULATION DATA TO BE USED IN THIS SCRIPT

# setup -------------------------------------------------------------------

source('R/setup.R')
library(stars)
library(raster)
library(rgdal)
#library(terra)

# directory ---------------------------------------------------------------

ghsl_dir <- "//storage6/usuarios/Proj_acess_oport/data-raw/ghsl"
