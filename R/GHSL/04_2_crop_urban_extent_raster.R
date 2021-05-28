# description -------------------------------------------------------------

# this script crops and saves raster for each urban extent (saved at 04_1)
#..containing
# i) built-up area data
# ii) population
#..for every year

# setup -------------------------------------------------------------------

source('R/setup.R')

# directory ---------------------------------------------------------------

ghsl_built_dir <- "//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/BUILT/UCA/"

# files vector
years <-c('1975','1990','2000','2014')
files <- purrr::map(years, ~dir(ghsl_built_dir, pattern = .))

# APAGAR DEPOIS
#input <- files[[1]]

# define function ---------------------------------------------------------------

666666666 definir como operacionalizar. alem disso, usar este script ou continuar no 04_1?

f_create_polygon_cutoff <- function(input){

  # read all raster files from one year in a list
  bua_uca <- purrr::map(input, ~ raster::raster(paste0(ghsl_built_dir, .)))

  # extract the year
  anos <- purrr::map_chr(input, ~ stringr::str_extract(., "(?<=LDS)[0-9]{4}"))

  # extract uca name
  uca_name <- purrr::map_chr(input, ~ stringr::str_extract(., "(?<=LDS[\\d]{4}_).+(?=_R2)"))

  # add years to name
  #uca_name <- paste0(uca_name,'_',anos)

  # rename each raster in the list
  names(bua_uca) <- uca_name


}

# run function ------------------------------------------------------------

# set up parallel
future::plan(future::multicore)


# run for multiple years
furrr::future_map(files, ~f_create_polygon_cutoff(.))
