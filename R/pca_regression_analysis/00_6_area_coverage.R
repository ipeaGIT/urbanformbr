# description -------------------------------------------------------------

# this script estimates urban extent area considering vectors files generated
# at R/GHSL/04_1 and R/GHSL/04_2.

# measures estimated:
# area urban extent 1975
# area urban extent 2014
# area urban expansion (difference between urban extent 2014 and 1975)

# these measures are then added to pca_regression_df:
# data/urbanformbr/pca_regression_df.rds

# setup -------------------------------------------------------------------

source('R/setup.R')

# directory ---------------------------------------------------------------

ghsl_dir <- "//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/"

years <-c('1975','2014')

# files polygon
files_built_polygon <- purrr::map(
  years,
  ~dir(
    path = paste0(ghsl_dir,'results/'),
    pattern = paste0('urban_extent_uca_',.),
    full.names = T
    )
  )

# files raster
files_urban_extent_raster <- purrr::map(years, ~dir(paste0(ghsl_dir, "BUILT/urban_extent_cutoff_20/"), pattern = ., full.names = T))


### APAGAR
input_polygon <- files_built_polygon
input_raster <- files_urban_extent_raster[[1]]

# define function ---------------------------------------------------------
funcao <- function(input_polygon, input_raster){


  # * read urban extent polygons --------------------------------------------

  bua_pol <- purrr::map(input_polygon, ~readr::read_rds(.))
  names(bua_pol) <- paste0('urban_extent_', years)

  bua_pol <- purrr::map(bua_pol, ~split(., .$name_uca_case))

  # * estimate area value (km2) ----------------------------------
  bua_pol$urban_extent_1975 <- purrr::map(
    bua_pol$urban_extent_1975,
    ~dplyr::mutate(., area_urban_extent_1975 = units::set_units(sf::st_area(.), value = km^2))
  )

  bua_pol$urban_extent_2014 <- purrr::map(
    bua_pol$urban_extent_2014,
    ~dplyr::mutate(., area_urban_extent_2014 = units::set_units(sf::st_area(.), value = km^2))
  )


  # * expansion area --------------------------------------------------------

  # * * polygon ----------------------------------------------

    bua_select <- purrr::modify_in(
    .x = bua_pol, .where = 1,
    ~purrr::map(
      .x = ., ~dplyr::select(., code_muni,name_uca_case,geometry)
      )
    )
  bua_select <- purrr::modify_in(
    .x = bua_select, .where = 2,
    ~purrr::map(
      .x = ., ~dplyr::select(., geometry)
      )
    )

  expansion_area_polygon <- purrr::map2(
    .x = bua_select$urban_extent_1975,
    .y = bua_select$urban_extent_2014,
    function(x,y)
      sf::st_sym_difference(x = x, y =  y)
  )


  # * * value ------------------------------------------------
  # expansion area via subtraction (area_urban_extent_2014 - area_urban_extent_1975)

  expansion_area_df <- purrr::map(
    bua_pol,
    ~purrr::map(., ~sf::st_drop_geometry(.))
  )

  expansion_area_df <- purrr::map(expansion_area_df, ~purrr::reduce(., dplyr::bind_rows))
  expansion_area_df <- dplyr::left_join(
    expansion_area_df$urban_extent_1975,
    expansion_area_df$urban_extent_2014 %>% dplyr::select(-name_uca_case),
    by = 'code_muni'
  )

  data.table::setDT(expansion_area_df)[
    ,
    `:=`(
      # expansion area size in km2
      area_expansion = area_urban_extent_2014 - area_urban_extent_1975,
      # ratio between urban extent 2014 and urban extent 1975
      urban_extent_ratio = as.double(area_urban_extent_2014 / area_urban_extent_1975),
      # horizontal expansion rate of growth 1975-2014
      horizontal_expansion = as.double( (area_urban_extent_2014 - area_urban_extent_1975) / area_urban_extent_1975 )
    )
  ]

  # check any null expansion area (uca that did not expand 1975-2014)
  expansion_area_df[area_expansion == 0]
  # 2306405 (itapipoca) did not expand horizontally 1975-2014
  ##### DECIDE WHAT TO DO



  # * save area rds ---------------------------------------------------------
  saveRDS(
    object = expansion_area_df,
    file = '//storage6/usuarios/Proj_acess_oport/data/urbanformbr/pca_regression_df/area.rds',
    compress = 'xz'
  )


  # * read raster -----------------------------------------------------------
  # read all raster files from one year in a list
  #bua_raster <- purrr::map(input_raster, ~ raster::raster(paste0(.)))
  bua_raster <- purrr::map(
    input_raster,
    ~purrr::map(., ~raster::raster(.))
    )

  names(bua_raster) <- paste0('urban_extent_', c(1975,2014))

  # extract year
  anos <- purrr::map(
    input_raster,
    ~purrr::map_chr(., ~stringr::str_extract(., "(?<=LDS)[0-9]{4}"))
  )

  # extract uca name
  uca_name <- purrr::map(
    input_raster,
    ~purrr::map_chr(., ~ stringr::str_extract(., "(?<=cutoff_20_).+(?=_1K_raster)"))
  )

  # add years to name
  #uca_name <- paste0(uca_name,'_',anos)

  # rename each raster in the list
  names(bua_raster$urban_extent_1975) <- uca_name[[1]]
  names(bua_raster$urban_extent_2014) <- uca_name[[2]]


  # * saturation difference -------------------------------------------------
  # saturation = built up area / urban extent
    # difference between average saturation 1975-2014 in the desired areas:
  # consolidated area; expansion area; total area

  # function to extract values from raster file defining polygon (sf) shape
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

  # * * consolidated area ---------------------------------------------------
  # consolidated area = urban extent 1975
  # two options:

  # 1 extract mean value using raster from 1975-2014 and polygon from 1975..
  #..and subtract these values
  # 2 crop raster from 2014 using 1975 polygon;
  # subtract rasters (values 2014 and polygon 1975 - values 1975 and polygon 1975)
  # extract mean value from subtraction
  # obs.: option 2 works because polygons are the same (raster subtraction performs
  #..operation on intersection)

  # ARE THESE TWO OPERATIONS THE SAME?

  # * * expansion area ------------------------------------------------------
  # expansion area: sym_difference urban extent 1975-2014 (intersection inverse)


  # * * total area ----------------------------------------------------------
  # saturation total area for each year (considering total urban extent in each year)

  total_area <- purrr::map(
    bua_raster,
    ~purrr::map(., ~raster::cellStats(., mean))
  )

  total_area <- data.table::data.table(
    name_uca_case = names(total_area$urban_extent_1975),
    saturation_total_area_1975 = as.double(total_area$urban_extent_1975),
    saturation_total_area_2014 = as.double(total_area$urban_extent_2014)
  )








}

# run function ------------------------------------------------------------


