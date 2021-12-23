# description -------------------------------------------------------------

# this script
# 1. downloads urban concentrations areas (uca) delimited by IBGE
# 2. dissolve their internal limits
# 3. filters uca
  # 3.1 with pop >= 100000
  # 3.2 exclude uca's that share borders with international urban areas (listed below)
    # 4322400 Uruguaiana RS
    # 4108304 Foz do Iguaçu PR
    # 5003207 Corumbá MS
# 4. saves shapefiles as .rds for future use @urbanformbr

# setup -------------------------------------------------------------------

# load libraries
source('R/fun_support/setup.R')

# load functions to dissolve polygons
source('R/fun_support/dissolve_polygons.R')

# define function ---------------------------------------------------------

f_uca_shapes <- function(){

  # 1 read uca --------------------------------------------------------------
  uca <- geobr::read_urban_concentrations(simplified = F)

  # 2 create pop sum --------------------------------------------------------

  # setDT
  data.table::setDT(uca)

  # create total population by urban concentration
  uca[
    ,
    `:=` (
      pop_ibge_total_2010 = sum(pop_total_2010)#,
      #pop_ibge_urban_2010 = sum(pop_urban_2010),
      #pop_ibge_rural_2010 = sum(pop_rural_2010)
    ),
    by = .(code_urban_concentration)
    ]

  # 3 dissolve internal limits ----------------------------------------------

  # change uca to sf
  uca <- sf::st_as_sf(uca)

  # to_multipolygon
  dissolved <- to_multipolygon(uca)
  # simplify_temp_sf
  dissolved <- simplify_temp_sf(dissolved)
  # dissolve_polygons
  dissolved <- dissolve_polygons(dissolved, 'code_urban_concentration')

  uca_join <- data.table::setDT(uca)[
    ,
    lapply(.SD, sum),
    by = .(code_urban_concentration, name_urban_concentration),
    .SDcols = c('pop_total_2010')
  ]

  data.table::setnames(
    uca_join,
    c('pop_total_2010'),
    c('pop_ibge_total_2010')
    )

  dissolved <- dplyr::left_join(dissolved, uca_join)

  # reproject crs to 4326
  dissolved <- sf::st_transform(dissolved, 4326)

  # add column with clean uca name
  dissolved <- dissolved %>%
    dplyr::mutate(name_uca_case = janitor::make_clean_names(name_urban_concentration)) %>%
  # reorder df by code_urban_concentration
    dplyr::arrange(code_urban_concentration) %>%
  # change column order
    dplyr::relocate(name_uca_case, .after = name_urban_concentration)


  # 4 filter uca with pop >= 100000 -----------------------------------------
  dissolved <- subset(dissolved, pop_ibge_total_2010 >= 100000)

  # 5 remove ucas that share international borders ---------------------------------
  # 4322400 Uruguaiana RS
  # 4108304 Foz do Iguaçu PR
  # 5003207 Corumbá MS
  to_be_removed <- c(4322400, 4108304, 5003207)
  dissolved <- subset(dissolved, code_urban_concentration %nin% to_be_removed)


  # 5 save resulting shape --------------------------------------------------
  saveRDS(
    object = dissolved,
    file = '//storage6/usuarios/Proj_acess_oport/data/urbanformbr/urban_area_shapes/urban_area_pop_100000_dissolved.rds',
    compress = 'xz'
  )


}


# run function ------------------------------------------------------------
f_uca_shapes()


