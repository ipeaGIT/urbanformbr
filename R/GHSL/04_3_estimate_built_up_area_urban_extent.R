# description -------------------------------------------------------------

# this script estimates urban extent area and other measures considering vectors and
# raster files generated at 04_1 and 04_2.
# measures estimated:
# MEASURES
# these measures are then added to a df used to estimate PCA & econometric analysis
#.. data/urbanformbr/pca_regression_df.rds

# setup -------------------------------------------------------------------

source('R/setup.R')

# directory ---------------------------------------------------------------

ghsl_dir <- "//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/"

# files vector
years <-c('1975','2014')
files_built_polygon <- purrr::map(
  years,
  ~dir(
    path = paste0(ghsl_dir,'results/'),
    pattern = paste0('urban_extent_uca_',.),
    full.names = T
    )
  )

# define function ---------------------------------------------------------
funcao <- function(input){


  # * read urban extent polygons --------------------------------------------

  bua_pol <- purrr::map(input, ~readr::read_rds(.))
  names(bua_pol) <- paste0('urban_extent_', years)

  bua_pol <- purrr::map(bua_pol, ~split(., .$name_uca_case))

  # * consolidated area: urban extent 1975 ----------------------------------
  bua_pol$urban_extent_1975 <- purrr::map(
    bua_pol$urban_extent_1975,
    ~dplyr::mutate(., area_urban_extent_1975 = units::set_units(sf::st_area(.), value = km^2))
  )

  bua_pol$urban_extent_2014 <- purrr::map(
    bua_pol$urban_extent_2014,
    ~dplyr::mutate(., area_urban_extent_2014 = units::set_units(sf::st_area(.), value = km^2))
  )

  # * expansion area: sym_diff(ue_1975,ue_2014) -----------------------------
  bua_select <- purrr::modify_in(
    .x = bua_pol, .where = 1,
    ~purrr::map(
      .x = ., ~dplyr::select(., code_muni,name_uca_case,geometry)
    )
  )
  bua_select <- purrr::modify_in(
    .x = bua_pol, .where = 2,
    ~purrr::map(
      .x = ., ~dplyr::select(., geometry)
    )
  )

  expansion_area <- purrr::map2(
    .x = bua_select$urban_extent_1975,
    .y = bua_select$urban_extent_2014,
    function(x,y)
      sf::st_sym_difference(x = x, y =  y)
  )

  expansion_area <- purrr::map(
    expansion_area,
    ~dplyr::mutate(., area_expansion_area_1975_2014 = units::set_units(sf::st_area(.), value = km^2))
  )


  # estimate consolidated area: urban extent 1975)

  # estimate



}

# run function ------------------------------------------------------------


