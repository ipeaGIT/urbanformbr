# description -------------------------------------------------------------

# this script
# 1. reads GHS-BUILT (1km res) raster from each urban concentration
#..areas (uca)
# 2. estimates the built up area within each one of them (in each year)
# 3. adds this information to uca shape file

# TO DO LIST
## USE FULL DATA (WORLD) TO COMPARE ESTIMATION WITH SUMMARY STATISTICS AT
# CORBANE ET AL (2019) OR FLORCZYK ET AL (2019)

# setup -------------------------------------------------------------------

source('R/setup.R')

# directory ---------------------------------------------------------------

ghsl_built_dir <- "//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/BUILT/UCA/"

# define function ---------------------------------------------------------

# function to extract values from raster file
f_extrair_mean <- function(raster, shape){

  extrair <- exactextractr::exact_extract(raster, shape) %>%
    dplyr::bind_rows(., .id = 'id') %>%
    dplyr::mutate(id = as.numeric(id)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(bua_mean = mean(value, na.rm = T)) %>%
    dplyr::mutate(id = shape$name_uca_case) %>%
    dplyr::rename(name_uca_case = id) %>%
    data.table::setDT()

}

# main function
funcao <- function(input){

  # read all raster files from one year in a list
  bua_uca <- purrr::map(input, ~raster::raster(paste0(ghsl_built_dir, .)))

  # extract the year
  anos <- purrr::map_chr(input, ~stringr::str_extract(., "(?<=LDS)[0-9]{4}"))

  # extract uca name
  uca_name <- purrr::map_chr(input, ~stringr::str_extract(., "(?<=LDS[\\d]{4}_).+(?=_R2)"))

  # rename each raster in the list
  names(bua_uca) <- uca_name

  # read uca shapefiles in one dataset
  uca_all <- readr::read_rds('//storage6/usuarios/Proj_acess_oport/data/urbanformbr/urban_area_shapes/urban_area_pop_100000_dissolved.rds')

  # change shape crs
  uca_all <- sf::st_transform(uca_all, raster::projection(purrr::pluck(bua_uca, 1)))

  # split shape dataset into list with each uca as an individual element
  uca_split <- base::split(uca_all, uca_all$name_uca_case)

  # reorder shape list according to raster list (must do to ensure match)
  uca_split <- uca_split[order(names(bua_uca))]

  # extract raster information (average built up area within shape)
  extrair <- purrr::map2(.x = bua_uca, .y = uca_split, function(x,y) f_extrair_mean(x,y))

  # change built up area column name
  extrair <- purrr::map2(extrair, anos, function(x,y)
    data.table::setnames(x, old = 'bua_mean', new = paste0('bua_mean', y))
  )

  # bind all datasets together
  extrair <- data.table::rbindlist(extrair)

  # left join built up area extracted to uca shape
  uca_all <- dplyr::left_join(uca_all, extrair, by = c('name_uca_case' = 'name_uca_case'))

  return(uca_all)

}


# run function ------------------------------------------------------------

# set up parallel
future::plan(future::multicore)

# files vector
years <-c('1975','1990','2000','2014')
files <- purrr::map(years, ~dir(ghsl_built_dir, pattern = .))

#files_input <- dir(ghsl_built_dir)
#files_input_1975 <- dir(ghsl_built_dir, pattern = '1975')
#files_input_1990 <- dir(ghsl_built_dir, pattern = '1990')
#files_input_2000 <- dir(ghsl_built_dir, pattern = '2000')
#files_input_2014 <- dir(ghsl_built_dir, pattern = '2014')

# run for multiple years
extrair <- furrr::future_map(files, ~funcao(.))
#extrair <- furrr::future_map(
#  list(files_input_1975,files_input_1990,files_input_2000,files_input_2014),
#  ~funcao(.)
#  )


# join resulting datasets -------------------------------------------------

uca_all_final <- dplyr::left_join(
  purrr::pluck(extrair, 1),
  #extrair$files1975,
  data.table::setDT(purrr::pluck(extrair, 2))[, .(name_uca_case, bua_mean1990)],
  by = c('name_uca_case' = 'name_uca_case')
)

uca_all_final <- dplyr::left_join(
  uca_all_final,
  data.table::setDT(purrr::pluck(extrair, 3))[, .(name_uca_case, bua_mean2000)],
  by = c('name_uca_case' = 'name_uca_case')
)

uca_all_final <- dplyr::left_join(
  uca_all_final,
  data.table::setDT(purrr::pluck(extrair, 4))[, .(name_uca_case, bua_mean2014)],
  by = c('name_uca_case' = 'name_uca_case')
)

# save dataset ------------------------------------------------------------

# create directory
if (!dir.exists("//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/results")){
  dir.create("//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/results")
}

saveRDS(
  object = uca_all_final,
  file = '//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/results/uca_pop_100000_built_up_area_results.rds',
  compress = 'xz'
)


