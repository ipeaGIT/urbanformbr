# description -------------------------------------------------------------

# this script
# 1 downloads urban concentrations areas (uca) delimited by IBGE
# 2 dissolve their internal limits
# 3 saves two shapefiles as .rds for future use @urbanformbr:
## 3.1 all the uca present in the IBGE urban concentration dataset
## 3.2 urban concentration areas with population >= 100000


# setup -------------------------------------------------------------------

# load libraries
source('R/setup.R')

# load functions to dissolve polygons
source('R/dissolve_polygons.R')

# define function ---------------------------------------------------------

f_uca_shapes <- function(){

  # 1 read uca --------------------------------------------------------------
  uca <- geobr::read_urban_concentrations(simplified = F)

  # 2 create pop sum --------------------------------------------------------

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

  # add column with clean uca name
  dissolved <- dissolved %>%
    dplyr::mutate(name_uca_case = janitor::make_clean_names(name_urban_concentration)) %>%
  # reorder df by clean uca name
    dplyr::arrange(name_uca_case) %>%
  # change column order
    dplyr::relocate(name_uca_case, .after = name_urban_concentration)

  # 4 save resulting shape --------------------------------------------------

    # * 4.1 full uca dataset --------------------------------------------------
  saveRDS(
    object = dissolved,
    file = '//storage6/usuarios/Proj_acess_oport/data/urbanformbr/urban_area_shapes/urban_area_dissolved.rds',
    compress = 'xz'
  )

    # * 4.2 filter pop >= 100000 ----------------------------------------------
  saveRDS(
    object = dissolved %>% dplyr::filter(sum_pop_total_2010_uca >= 100000),
    file = '//storage6/usuarios/Proj_acess_oport/data/urbanformbr/urban_area_shapes/urban_area_pop_100000_dissolved.rds',
    compress = 'xz'
  )


}


# run function ------------------------------------------------------------
f_uca_shapes()


