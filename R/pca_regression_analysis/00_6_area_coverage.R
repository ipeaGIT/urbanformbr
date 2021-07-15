# description -------------------------------------------------------------

# this script estimates urban extent area and derived variables, considering
#...raster and vectors files generated at R/GHSL.

## definitions:

# saturation = built up area density = ghsl raster pixel's value

# urban extent = comparable spatially unit of analysis of each urban concentration..
#..area. the urban extent in each year is defined by raster pixels whose value..
#..(i.e, whose saturation or built up land coverage) equals or is greater than 20%.

# consolidated area = urban extent from 1975.

# expansion area = the spatial difference (intersection's inverse) between the..
#..urban extent from 2014 and the urban extent from 1975.

# total area = urban extent from 2014 = consolidated area + expansion area


## measures estimated:

# urban extent area size 1975
# urban extent area size 2014
# expansion area size
# urban extent size ratio = (urban extent 2014 / urban extent 1975)
# urban_extent_horizontal_growth ( (urban extent 2014 - urban extent 1975) / urban extent 1975 )
# saturation *total/consolidated/expansion* *year* = mean value from desired polygon in each year
# saturation diff = saturation *total/consolidated/expansion* 2014 - saturation *total/consolidated/expansion* 1975


# these measures are then added to pca_regression_df:
# data/urbanformbr/pca_regression_df.rds

# setup -------------------------------------------------------------------

source('R/setup.R')

# directory ---------------------------------------------------------------

ghsl_dir <- "../../data/urbanformbr/ghsl/"

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
input_urban_extent_raster <- files_urban_extent_raster
input_uca_complete_raster <- files_uca_complete_raster

# define function ---------------------------------------------------------

f_area_variables <- function(input_polygon, input_urban_extent_raster, input_uca_complete_raster){

  # read & clean data -------------------------------------------------------------

  # * read polygon --------------------------------------------------------

  # urban extent polygon
  polygon_built_area <- purrr::map(input_polygon, ~readr::read_rds(.))
  names(polygon_built_area) <- paste0('urban_extent_', years)

  polygon_built_area <- purrr::map(polygon_built_area, ~split(., .$name_uca_case))
  # * * clip expansion polygon ------------------------------------------------

  polygon_expansion_area <- purrr::modify_in(
    .x = polygon_built_area, .where = 1,
    ~purrr::map(
      .x = ., ~dplyr::select(., code_muni,name_uca_case,geometry)
    )
  )
  polygon_expansion_area <- purrr::modify_in(
    .x = polygon_expansion_area, .where = 2,
    ~purrr::map(
      .x = ., ~dplyr::select(., geometry)
    )
  )

  polygon_expansion_area <- purrr::map2(
    .x = polygon_expansion_area$urban_extent_1975,
    .y = polygon_expansion_area$urban_extent_2014,
    function(x,y)
      sf::st_sym_difference(x = x, y =  y)
  )

  # * read raster ---------------------------------------------------------

  # * * urban extent raster -----------------------------------------------

  raster_urban_extent <- purrr::map(
    input_urban_extent_raster,
    ~purrr::map(., ~raster::raster(.))
  )

  names(raster_urban_extent) <- paste0('urban_extent_', c(1975,2014))

  # extract year
  anos <- purrr::map(
    input_urban_extent_raster,
    ~purrr::map_chr(., ~stringr::str_extract(., "(?<=LDS)[0-9]{4}"))
  )

  # extract uca name
  uca_name <- purrr::map(
    input_urban_extent_raster,
    ~purrr::map_chr(., ~ stringr::str_extract(., "(?<=cutoff_20_).+(?=_1K_raster)"))
  )

  # add years to name
  #uca_name <- paste0(uca_name,'_',anos)

  # rename each raster in the list
  names(raster_urban_extent$urban_extent_1975) <- uca_name[[1]]
  names(raster_urban_extent$urban_extent_2014) <- uca_name[[2]]


  # * * complete uca raster -----------------------------------------------
  # to estimate saturation in expansion area in 1975 & 2014, must use complete..
  #..uca raster 1975, since masked raster1975 will not contain all pixels from..
  #..the 2014 polygon.
  raster_complete <- purrr::map(
    files_uca_complete_raster,
    ~purrr::map(., ~raster::raster(.))
  )

  names(raster_complete) <- paste0('uca_complete_raster_', c(1975,2014))

  # extract uca name
  uca_name <- purrr::map(
    files_uca_complete_raster,
    ~purrr::map_chr(., ~ stringr::str_extract(., "(?<=LDS[\\d]{4}_).+(?=_R2018A)"))
  )

  # rename each raster in the list
  names(raster_complete$uca_complete_raster_1975) <- uca_name[[1]]
  names(raster_complete$uca_complete_raster_2014) <- uca_name[[2]]

  # * * expansion raster ----------------------------------------------------

    # REMOVE itapipoca which expansion area equals to zero
  raster_expansion <- raster_complete
  raster_expansion$uca_complete_raster_1975$itapipoca <- NULL
  raster_expansion$uca_complete_raster_2014$itapipoca <- NULL
  polygon_expansion_area$itapipoca <- NULL

  # reorder list to match order
  raster_expansion$uca_complete_raster_1975 <-
    raster_expansion$uca_complete_raster_1975[names(polygon_expansion_area)]
  raster_expansion$uca_complete_raster_2014 <-
    raster_expansion$uca_complete_raster_2014[names(polygon_expansion_area)]

  raster_expansion$uca_complete_raster_1975 <- purrr::map2(
    .x = raster_expansion$uca_complete_raster_1975, .y = polygon_expansion_area,
    ~raster::crop(.x, .y)
  )
  raster_expansion$uca_complete_raster_2014 <- purrr::map2(
    .x = raster_expansion$uca_complete_raster_2014, .y = polygon_expansion_area,
    ~raster::crop(.x, .y)
  )

  #erro <- purrr::map2(
  #  .x = raster_expansion$uca_complete_raster_1975, .y = polygon_expansion_area,
  #  purrr::possibly(~raster::crop(.x, .y), 'erro')
  #)
  #filter errors
  #map_chr(erro, class) %>% unique()
  #teste <- purrr::keep(erro, inherits, 'character')

  raster_expansion$uca_complete_raster_1975 <- purrr::map2(
    .x = raster_expansion$uca_complete_raster_1975, .y = polygon_expansion_area,
    ~raster::mask(x = .x, mask =  .y)
  )
  raster_expansion$uca_complete_raster_2014 <- purrr::map2(
    .x = raster_expansion$uca_complete_raster_2014, .y = polygon_expansion_area,
    ~raster::mask(x = .x, mask =  .y)
  )


  # estimate metrics -------------------------------------------------------

  # * land size area ------------------------------------------------------

  polygon_built_area$urban_extent_1975 <- purrr::map(
    polygon_built_area$urban_extent_1975,
    ~dplyr::mutate(., urban_extent_size_1975 = units::set_units(sf::st_area(.), value = km^2))
  )

  polygon_built_area$urban_extent_2014 <- purrr::map(
    polygon_built_area$urban_extent_2014,
    ~dplyr::mutate(., urban_extent_size_2014 = units::set_units(sf::st_area(.), value = km^2))
  )

  # expansion area via subtraction (urban_extent_size_2014 - urban_extent_size_1975)

  df_horizontal_expansion_area <- purrr::map(
    polygon_built_area,
    ~purrr::map(., ~sf::st_drop_geometry(.))
  )

  df_horizontal_expansion_area <- purrr::map(
    df_horizontal_expansion_area,
    ~purrr::reduce(., dplyr::bind_rows)
    )

  df_horizontal_expansion_area <- dplyr::left_join(
    df_horizontal_expansion_area$urban_extent_1975,
    df_horizontal_expansion_area$urban_extent_2014 %>% dplyr::select(-name_uca_case),
    by = 'code_muni'
  )

  data.table::setDT(df_horizontal_expansion_area)[
    ,
    `:=`(
      # expansion area size in km2
      expansion_area_size = urban_extent_size_2014 - urban_extent_size_1975,
      # ratio between urban extent 2014 and urban extent 1975
      urban_extent_size_ratio = as.double(urban_extent_size_2014 / urban_extent_size_1975),
      # horizontal expansion rate of growth 1975-2014
      urban_extent_horizontal_growth = as.double( (urban_extent_size_2014 - urban_extent_size_1975) / urban_extent_size_1975 )
    )
  ]

  # check any null expansion area (uca that did not expand 1975-2014)
  # itapipoca expansion area equals to zero
  df_horizontal_expansion_area[expansion_area_size == 0]
  # 2306405 (itapipoca) did not expand horizontally 1975-2014
  ##### DECIDE WHAT TO DO

  # * land coverage: saturation --------------------------------------------
  #  land coverage = saturation = (built up area / urban extent)
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

  # * * consolidated area -------------------------------------------------

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

  consolidated <- raster_urban_extent

  consolidated$urban_extent_2014 <- purrr::map2(
    .x = consolidated$urban_extent_2014, .y = polygon_built_area$urban_extent_1975,
    function(x,y)
      raster::crop(x = x, y = y)
  )

  consolidated$urban_extent_2014 <- purrr::map2(
    .x = consolidated$urban_extent_2014, .y = polygon_built_area$urban_extent_1975,
    function(x,y)
      raster::mask(x = x, mask = y)
  )

  df_consolidated_area <- purrr::map(
    consolidated,
    ~purrr::map(., ~raster::cellStats(., mean))
  )

  df_consolidated_area <- data.table::data.table(
    name_uca_case = names(df_consolidated_area$urban_extent_1975),
    saturation_consolidated_area_1975 = as.double(df_consolidated_area$urban_extent_1975),
    saturation_consolidated_area_2014 = as.double(df_consolidated_area$urban_extent_2014)
  )

  df_consolidated_area[
    ,
    `:=`(
      saturation_consolidated_diff = saturation_consolidated_area_2014 - saturation_consolidated_area_1975
    )
  ]

  # * * expansion area ----------------------------------------------------
  # extract mean value
  df_expansion_area <- purrr::map(
    raster_expansion,
    ~purrr::map(., ~raster::cellStats(., mean))
  )

  df_expansion_area <- data.table::data.table(
    name_uca_case = names(df_expansion_area$uca_complete_raster_1975),
    saturation_expansion_area_1975 = as.double(df_expansion_area$uca_complete_raster_1975),
    saturation_expansion_area_2014 = as.double(df_expansion_area$uca_complete_raster_2014)
  )
  df_expansion_area[
    ,
    `:=`(
      saturation_expansion_diff = saturation_expansion_area_2014 - saturation_expansion_area_1975
    )
  ]

  # * * total area --------------------------------------------------------
  # saturation from total area depends on
  # i) the raster data containing the saturation/coverage data
  # ii) the polygon that defines the limits in which the average saturation data..
  #..will be estimated.

  # thus, total area saturation can be estimated in two ways, considering different
  #..definitions of "total area" (i.e., which polygon constitutes the "total"
  #..polygon of an urban concentration area):

  ## 1: using the raster data from each year and the urban extent polygon from each
  # year as the corresponding polygon for that year
  # ex1.:
  # uca raster from 1975 and polygon (total area = urban extent) from 1975 and
  # uca raster from 2014 and polygon (total area = urban extent) from 2014
  # OR
  ## 2: using the raster data from each year, but the SAME urban extent polygon
  #..for both years as the defining total area limits (in this case, the latter year
  #..constitutes the only sensible urban extent polygon, i.e, 2014)
  # ex.2:
  # uca raster from 1975 and polygon (total area = urban extent) from 2014 and
  # uca raster from 2014 and polygon (total area = urban extent) from 2014

  # aftwer deliberation (17/06/21), the second option will be taken


  # * * * varying total area ------------------------------------------------
  # uca raster from 1975 and polygon (total area = urban extent) from 1975 and
  # uca raster from 2014 and polygon (total area = urban extent) from 2014
  # OPTION NOT TAKEN, but estimation preserved


  #df_total_area <- purrr::map(
  #  raster_urban_extent,
  #  ~purrr::map(., ~raster::cellStats(., mean))
  #)

  #df_total_area <- data.table::data.table(
  #  name_uca_case = names(df_total_area$urban_extent_1975),
  #  saturation_total_area_1975 = as.double(df_total_area$urban_extent_1975),
  #  saturation_total_area_2014 = as.double(df_total_area$urban_extent_2014)
  #)

  #df_total_area[
  #  ,
  #  `:=`(
  #    saturation_total_area_diff = saturation_total_area_2014 - saturation_total_area_1975
  #  )
  #]


  # * * * fixed total area --------------------------------------------------
  # fixed total area = urban extent 2014
  # uca raster from 1975 and polygon (total area = urban extent) from 2014 and
  # uca raster from 2014 and polygon (total area = urban extent) from 2014
  # OPTION TAKEN


  total_fixed <- raster_complete

  # reorder list to match order
  total_fixed$uca_complete_raster_1975 <-
    total_fixed$uca_complete_raster_1975[
      names(polygon_built_area$urban_extent_2014)
    ]
  total_fixed$uca_complete_raster_2014 <-
    total_fixed$uca_complete_raster_2014[
      names(polygon_built_area$urban_extent_2014)
    ]

  total_fixed$uca_complete_raster_1975 <- purrr::map2(
    .x = total_fixed$uca_complete_raster_1975, .y = polygon_built_area$urban_extent_2014,
    function(x,y)
      raster::crop(x = x, y = y)
  )

  total_fixed$uca_complete_raster_1975 <- purrr::map2(
    .x = total_fixed$uca_complete_raster_1975, .y = polygon_built_area$urban_extent_2014,
    function(x,y)
      raster::mask(x = x, mask = y)
  )

  total_fixed$uca_complete_raster_2014 <- purrr::map2(
    .x = total_fixed$uca_complete_raster_2014, .y = polygon_built_area$urban_extent_2014,
    function(x,y)
      raster::crop(x = x, y = y)
  )

  total_fixed$uca_complete_raster_2014 <- purrr::map2(
    .x = total_fixed$uca_complete_raster_2014, .y = polygon_built_area$urban_extent_2014,
    function(x,y)
      raster::mask(x = x, mask = y)
  )

  df_total_area_fixed <- purrr::map(
    total_fixed,
    ~purrr::map(., ~raster::cellStats(., mean))
  )

  df_total_area_fixed <- data.table::data.table(
    name_uca_case = names(df_total_area_fixed$uca_complete_raster_1975),
    saturation_total_area_fixed_1975 = as.double(df_total_area_fixed$uca_complete_raster_1975),
    saturation_total_area_fixed_2014 = as.double(df_total_area_fixed$uca_complete_raster_2014)
  )

  df_total_area_fixed[
    ,
    `:=`(
      saturation_total_area_fixed_diff = saturation_total_area_fixed_2014 - saturation_total_area_fixed_1975
    )
  ]

  # merge dfs -------------------------------------------------------------
  df_merged <- dplyr::left_join(
    df_horizontal_expansion_area,
    df_total_area_fixed,
    by = 'name_uca_case'
  ) %>%
    dplyr::left_join(
      df_consolidated_area,
      by = 'name_uca_case'
    ) %>%
    dplyr::left_join(
      df_expansion_area,
      by = 'name_uca_case'
    )


  # save area rds ---------------------------------------------------------
  saveRDS(
    object = df_merged,
    file = '../../data/urbanformbr/pca_regression_df/area.rds',
    compress = 'xz'
  )


}

# run function ------------------------------------------------------------
f_area_variables(
  input_polygon = files_built_polygon,
  input_urban_extent_raster = files_urban_extent_raster,
  input_uca_complete_raster = files_uca_complete_raster
)

