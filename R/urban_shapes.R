# description -------------------------------------------------------------

# this script
# 1 downloads urban concentrations areas (uca) delimited by IBGE
# 2 filter uca with population >= 100.000
# 3 dissolve their internal limits..
# 4 saves the resulting shapefile as .rds for future use @urbanformbr


# setup -------------------------------------------------------------------

# load libraries
source('R/setup.R')

# load functions to dissolve polygons
source('R/dissolve_polygons.R')

# define function ---------------------------------------------------------

f_uca_shapes <- function(){

  # 1 read uca --------------------------------------------------------------
  uca <- geobr::read_urban_concentrations(simplified = F)

  # 2 filter pop ------------------------------------------------------------

  #uca <- uca %>%
  #  dplyr::mutate(
  #    name_urb_case = snakecase::to_any_case(name_urban_concentration)
  #  )

  # setDT
  data.table::setDT(uca)

  # create total population by urban concentration
  uca[
    ,
    `:=` (
      sum_pop_total_2010_uca = sum(pop_total_2010)#,
      #sum_pop_urban_2010_uca = sum(pop_urban_2010),
      #sum_pop_rural_2010_uca = sum(pop_rural_2010)
    ),
    by = .(code_urban_concentration)
    ]

  # filter uca with population >= 100000
  uca <- uca[sum_pop_total_2010_uca >= 100000]

  #uca <- uca %>%
  #  dplyr::filter(pop_total_2010 >= 100000)

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
    .SDcols = c('pop_total_2010','pop_urban_2010','pop_rural_2010')
  ]

  data.table::setnames(
    uca_join,
    c('pop_total_2010','pop_urban_2010','pop_rural_2010'),
    c('sum_pop_total_2010_uca','sum_pop_urban_2010_uca','sum_pop_rural_2010_uca')
    )

  # dplyr way
  #uca_join <- uca %>%
  #  dplyr::group_by(code_urban_concentration, name_urban_concentration) %>%
  #  summarise(
  #    sum_pop_total_2010_uca = sum(pop_total_2010)),
  #sum_pop_urban_2010_uca = sum(pop_urban_2010),
  #sum_pop_rural_2010_uca = sum(pop_rural_2010)
  #)

  dissolved <- dplyr::left_join(dissolved, uca_join)

  # reproject crs to 4326
  dissolved <- sf::st_transform(dissolved, 4326)


  #### CORRIGIR BASE: PERMITIR USAR MAPVIEW -> POR QUE NAO ACEITA NO MOMENTO?

  # 4 save resulting shape --------------------------------------------------

  saveRDS(
    object = dissolved,
    file = '//storage6/usuarios/Proj_acess_oport/data/urbanformbr/urban_area_shapes/urban_area_pop_100000_dissolved.rds',
    compress = 'xz'
  )

}


# run function ------------------------------------------------------------
f_uca_shapes()


