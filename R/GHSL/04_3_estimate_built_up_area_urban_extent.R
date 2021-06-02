# description -------------------------------------------------------------

# this script estimates urban extent area and other measures considering vectors and
# raster files generated at 04_1 and 04_1
# measures estimated:
# MEASURES
# these measures are then added to a df that will be used to estimate PCA & econometric
# analysis

# setup -------------------------------------------------------------------

source('R/setup.R')

# directory ---------------------------------------------------------------

ghsl_dir <- "//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/"

# files vector
years <-c('1975','2014')
files_built <- purrr::map(
  years,
  ~dir(
    path = paste0(ghsl_dir,'BUILT/urban_extent_cutoff_20'),
    pattern = .,
    full.names = T
    )
  )

# define function ---------------------------------------------------------
funcao <- function(input){


  # * read urban extent polygons --------------------------------------------



}

# run function ------------------------------------------------------------


