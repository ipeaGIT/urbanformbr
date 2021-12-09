# description -------------------------------------------------------------

# this script classifies each uca according to its population in 2010, using
# criteria defined at IBGE (2016) - Arranjos Populacionais e Concentracoes Urbanas:

# uca (with more than 100000 pop) can be classified as:
# medium: ucas or isolated (only one muni) muni with 100000 <= pop <= 750000
# large : ucas or isolated (only one muni) muni with pop > 750000

# setup -------------------------------------------------------------------

source('R/setup.R')


# read data ---------------------------------------------------------------

# df_uca created by geobr::read_urban_concentrations
# df_uca contains total pop by uca
df_uca <- readr::read_rds("../../data/urbanformbr/urban_area_shapes/urban_area_pop_100000_dissolved.rds") %>%
  sf::st_drop_geometry() %>%
  dplyr::select(code_urban_concentration,pop_ibge_total_2010) %>%
  dplyr::rename(pop_uca_2010 = pop_ibge_total_2010) %>%
  data.table::setDT()

# classify uca
df_uca[
  ,
  `:=`(
    large_uca_pop = data.table::fcase(
      pop_uca_2010 > 750000, 1L,
      default = 0L
    )
  ),
  by = .(code_urban_concentration)
]


# save data ---------------------------------------------------------------

saveRDS(
  object = df_uca,
  file = '../../data/urbanformbr/pca_regression_df/classify_uca_large_pop.rds',
  compress = 'xz'
)



