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
## raster urban extent (cutoff >= 20%)
files_urban_extent_raster <- purrr::map(years, ~dir(paste0(ghsl_dir, "BUILT/urban_extent_cutoff_20/"), pattern = ., full.names = T))

## raster uca complete (uca )
files_uca_complete_raster <- purrr::map(years, ~dir(paste0(ghsl_dir, "BUILT/UCA/"), pattern = ., full.names = T))
## remove santa_cruz_do_sul_rs (excluded at R/GHSL/04_1)
files_uca_complete_raster[[1]] <- files_uca_complete_raster[[1]][!str_detect(files_uca_complete_raster[[1]],'santa_cruz_do_sul_rs')]
files_uca_complete_raster[[2]] <- files_uca_complete_raster[[2]][!str_detect(files_uca_complete_raster[[2]],'santa_cruz_do_sul_rs')]


### APAGAR
input_polygon <- files_built_polygon
input_raster <- files_urban_extent_raster

# define function ---------------------------------------------------------

####### MUDAR INPUTS DAS FUNCOES. ADICIONAR INPUT RASTER COMPLETE?

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

  expansion_area_polygon <- purrr::modify_in(
    .x = bua_pol, .where = 1,
    ~purrr::map(
      .x = ., ~dplyr::select(., code_muni,name_uca_case,geometry)
      )
    )
  expansion_area_polygon <- purrr::modify_in(
    .x = expansion_area_polygon, .where = 2,
    ~purrr::map(
      .x = ., ~dplyr::select(., geometry)
      )
    )

  expansion_area_polygon <- purrr::map2(
    .x = expansion_area_polygon$urban_extent_1975,
    .y = expansion_area_polygon$urban_extent_2014,
    function(x,y)
      sf::st_sym_difference(x = x, y =  y)
  )


  # * * value ------------------------------------------------
  # expansion area via subtraction (area_urban_extent_2014 - area_urban_extent_1975)

  horizontal_expansion_area_df <- purrr::map(
    bua_pol,
    ~purrr::map(., ~sf::st_drop_geometry(.))
  )

  horizontal_expansion_area_df <- purrr::map(horizontal_expansion_area_df, ~purrr::reduce(., dplyr::bind_rows))
  horizontal_expansion_area_df <- dplyr::left_join(
    horizontal_expansion_area_df$urban_extent_1975,
    horizontal_expansion_area_df$urban_extent_2014 %>% dplyr::select(-name_uca_case),
    by = 'code_muni'
  )

  data.table::setDT(horizontal_expansion_area_df)[
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
  # itapipoca expansion area equals to zero
  horizontal_expansion_area_df[area_expansion == 0]
  # 2306405 (itapipoca) did not expand horizontally 1975-2014
  ##### DECIDE WHAT TO DO






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

  # 1 crop raster from 2014 using 1975 polygon (1975 raster will already be croped);
  # extract mean value from 1975 and 2014;
  # subtract these values
  # 2 crop raster from 2014 using 1975 polygon (1975 raster will already be croped);
  # subtract rasters (values 2014 and polygon 1975 - values 1975 and polygon 1975)
  # extract mean value from subtraction
  # obs.: opt 2 only works because polygons are the same (raster subtraction performs
  #..operation on intersection)

  # ARE THESE TWO OPERATIONS THE SAME?

  consolidated <- bua_raster
  consolidated$urban_extent_2014 <- purrr::map2(
    .x = consolidated$urban_extent_2014, .y = bua_pol$urban_extent_1975,
    function(x,y)
      raster::crop(x = x, y = y)
  )

  consolidated$urban_extent_2014 <- purrr::map2(
    .x = consolidated$urban_extent_2014, .y = bua_pol$urban_extent_1975,
    function(x,y)
      raster::mask(x = x, mask = y)
  )

  consolidated_area_df <- purrr::map(
    consolidated,
    ~purrr::map(., ~raster::cellStats(., mean))
  )

  consolidated_area_df <- data.table::data.table(
    name_uca_case = names(consolidated_area_df$urban_extent_1975),
    saturation_consolidated_area_1975 = as.double(consolidated_area_df$urban_extent_1975),
    saturation_consolidated_area_2014 = as.double(consolidated_area_df$urban_extent_2014)
  )
  consolidated_area_df[
    ,
    `:=`(
      saturation_consolidated_diff = saturation_consolidated_area_2014 - saturation_consolidated_area_1975
    )
  ]


  # * * expansion area ------------------------------------------------------
  # expansion area: sym_difference urban extent 1975-2014 (intersection inverse)

  # to estimate saturation in expansion area in 1975 & 2014, must use complete..
  #..uca raster 1975, since masked raster1975 will not contain all pixels from..
  #..the 2014 polygon.
  complete_raster <- purrr::map(
    files_uca_complete_raster,
    ~purrr::map(., ~raster::raster(.))
  )

  names(complete_raster) <- paste0('uca_complete_raster_', c(1975,2014))

  # extract uca name
  uca_name <- purrr::map(
    files_uca_complete_raster,
    ~purrr::map_chr(., ~ stringr::str_extract(., "(?<=LDS[\\d]{4}_).+(?=_R2018A)"))
  )

  # rename each raster in the list
  names(complete_raster$uca_complete_raster_1975) <- uca_name[[1]]
  names(complete_raster$uca_complete_raster_2014) <- uca_name[[2]]

  # REMOVE itapipoca which expansion area equals to zero
  expansion_raster <- complete_raster
  expansion_raster$uca_complete_raster_1975$itapipoca <- NULL
  expansion_raster$uca_complete_raster_2014$itapipoca <- NULL
  expansion_area_polygon$itapipoca <- NULL

  # reorder list to match
  expansion_raster$uca_complete_raster_1975 <-
    expansion_raster$uca_complete_raster_1975[
      names(expansion_area_polygon)
      ]
  expansion_raster$uca_complete_raster_2014 <-
    expansion_raster$uca_complete_raster_2014[
      names(expansion_area_polygon)
    ]

  expansion_raster$uca_complete_raster_1975 <- purrr::map2(
    .x = expansion_raster$uca_complete_raster_1975, .y = expansion_area_polygon,
    ~raster::crop(.x, .y)
  )
  expansion_raster$uca_complete_raster_2014 <- purrr::map2(
    .x = expansion_raster$uca_complete_raster_2014, .y = expansion_area_polygon,
    ~raster::crop(.x, .y)
  )

  #erro <- purrr::map2(
  #  .x = expansion_raster$uca_complete_raster_1975, .y = expansion_area_polygon,
  #  purrr::possibly(~raster::crop(.x, .y), 'erro')
  #)
  #filter errors
  #map_chr(erro, class) %>% unique()
  #teste <- purrr::keep(erro, inherits, 'character')

  expansion_raster$uca_complete_raster_1975 <- purrr::map2(
    .x = expansion_raster$uca_complete_raster_1975, .y = expansion_area_polygon,
    ~raster::mask(x = .x, mask =  .y)
  )
  expansion_raster$uca_complete_raster_2014 <- purrr::map2(
    .x = expansion_raster$uca_complete_raster_2014, .y = expansion_area_polygon,
    ~raster::mask(x = .x, mask =  .y)
  )

  # extract mean value
  expansion_area_df <- purrr::map(
    expansion_raster,
    ~purrr::map(., ~raster::cellStats(., mean))
  )

  expansion_area_df <- data.table::data.table(
    name_uca_case = names(expansion_area_df$uca_complete_raster_1975),
    saturation_expansion_area_1975 = as.double(expansion_area_df$uca_complete_raster_1975),
    saturation_expansion_area_2014 = as.double(expansion_area_df$uca_complete_raster_2014)
  )
  expansion_area_df[
    ,
    `:=`(
      saturation_expansion_diff = saturation_expansion_area_2014 - saturation_expansion_area_1975
    )
  ]


  # * * total area ----------------------------------------------------------

  # UTILIZAR
  # AREA TOTAL CONSIDERANDO POLIGONO EM CADA ANO (i.e. poligonos totais variam)
  # area poligono 1975 e valores raster 1975 e area poligono 2014 e raster 2014
  # OU
  # AREA TOTAL CONSIDERANDO O MESMO POLIGONO (ex.: poligono total 1975)

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
  total_area[
    ,
    `:=`(
      saturation_total_area_diff = saturation_total_area_2014 - saturation_total_area_1975
    )
  ]



  # * merge dfs -------------------------------------------------------------
  merged_df <- dplyr::left_join(
    horizontal_expansion_area_df,
    total_area,
    by = 'name_uca_case'
  ) %>%
    dplyr::left_join(
      consolidated_area_df,
      by = 'name_uca_case'
    ) %>%
    dplyr::left_join(
      expansion_area_df,
      by = 'name_uca_case'
    )


  # * save area rds ---------------------------------------------------------
  saveRDS(
    object = merged_df,
    file = '//storage6/usuarios/Proj_acess_oport/data/urbanformbr/pca_regression_df/area.rds',
    compress = 'xz'
  )


}

# run function ------------------------------------------------------------


