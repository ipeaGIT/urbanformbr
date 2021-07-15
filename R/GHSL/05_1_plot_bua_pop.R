# description -------------------------------------------------------------

# this script
# 1. plots built up area and population data saved at 03_1 and 03_2


# setup -------------------------------------------------------------------

source('R/setup.R')

# directory ---------------------------------------------------------------

ghsl_results_dir <- "../../data/urbanformbr/ghsl/results/"


# read data ---------------------------------------------------------------
# uca results based on ghsl
uca_all_final <- readr::read_rds('../../data/urbanformbr/ghsl/results/uca_pop_100000_built_up_area_population_results.rds')
# region dataset
regiao <- geobr::read_region()

# clean and manipulate data -----------------------------------------------

setDT(uca_all_final)
# add regiao
uca_all_final[
  ,
  name_region := data.table::fcase(
    grepl("^1", uca_all_final$code_urban_concentration), 'Norte',
    grepl("^2", uca_all_final$code_urban_concentration), 'Nordeste',
    grepl("^3", uca_all_final$code_urban_concentration), 'Sudeste',
    grepl("^4", uca_all_final$code_urban_concentration), 'Sul',
    grepl("^5", uca_all_final$code_urban_concentration), 'Centro Oeste'
  )
]
# convert sf
uca_all_final <- sf::st_as_sf(uca_all_final)
# estimate polygon area
uca_all_final <- uca_all_final %>%
  dplyr::mutate(area_polygon = units::set_units(sf::st_area(uca_all_final), value = km^2))
# estimate built up area
uca_all_final <- uca_all_final %>%
  dplyr::mutate(
    built_up_area1975 = bua_mean1975 * 0.01 * area_polygon,
    built_up_area1990 = bua_mean1990 * 0.01 * area_polygon,
    built_up_area2000 = bua_mean2000 * 0.01 * area_polygon,
    built_up_area2014 = bua_mean2014 * 0.01 * area_polygon,
  )


# * double variables (bua_mean and pop) -----------------------------------

uca_pivot_double <- uca_all_final %>%
  setDT() %>%
  select(name_uca_case,bua_mean1975:pop2015) %>%
  pivot_longer(
    cols = bua_mean1975:pop2015,
    names_to = 'variavel'
  ) %>%
  separate(variavel,c('variavel','ano'), -4) %>%
  setDT()

# change year 2014 and 2015 into 2015
uca_pivot_double[ano %in% c(2014,2015), ano := 2015]
# tidy dataset
uca_pivot_double <- uca_pivot_double %>%
  pivot_wider(id_cols = c(name_uca_case,ano),names_from = variavel,values_from = value)
# setdt
setDT(uca_pivot_double)


# * units variable (built_up_area -----------------------------------------

uca_pivot_units <- uca_all_final %>%
  setDT() %>%
  select(name_uca_case,built_up_area1975:built_up_area2014) %>%
  pivot_longer(
    cols = built_up_area1975:built_up_area2014,
    names_to = 'variavel'
  ) %>%
  separate(variavel,c('variavel','ano'), -4) %>%
  setDT()
# change year 2014 into 2015
uca_pivot_units[ano %in% c(2014), ano := 2015]
# tidy dataset
uca_pivot_units <- uca_pivot_units %>%
  pivot_wider(id_cols = c(name_uca_case,ano),names_from = variavel,values_from = value)
setDT(uca_pivot_units)

uca_pivot <- uca_pivot_double
uca_pivot[
  uca_pivot_units,
  `:=` (
    built_up_area = i.built_up_area
  ),
  on = c('name_uca_case', 'ano')
]

rm(uca_pivot_double,uca_pivot_units)

# add region
uca_pivot <- uca_pivot %>%
  dplyr::left_join(
    uca_all_final %>% dplyr::select(name_uca_case,name_region),
    by = c('name_uca_case' = 'name_uca_case')
    ) %>%
  dplyr::relocate(name_region, .after = name_uca_case)

# plot data ----------------------------------------------------------------


uca_pivot %>%
  #st_transform(4326) %>%
  ggplot() +
  geom_point(aes(x = ano, y = bua_mean, size = pop))

uca_pivot %>%
  #st_transform(4326) %>%
  ggplot() +
  geom_point(aes(x = bua_mean, y = pop)) +
  geom_smooth(aes(x = bua_mean, y = pop)
              , method = 'lm'
              ) +
  facet_wrap(~ano,nrow = 1) +
  #scale_x_continuous(trans = 'log10') +
  scale_y_continuous(trans = 'log10')


uca_pivot %>%
  #dplyr::filter(!name_uca_case %in% c('rio_de_janeiro_rj', 'sao_paulo_sp')) %>%
  dplyr::mutate(built_up_area = as.double(built_up_area)) %>%
  ggplot() +
  geom_point(aes(x = built_up_area, y = pop, colour = name_region)) +
  geom_smooth(aes(x = built_up_area, y = pop)
              , method = 'lm'
  ) +
  facet_wrap(~ano,nrow = 1) +
  ggforce::scale_x_unit(unit = 'km^2', trans = 'log10') +
  #scale_x_continuous(trans = 'log10') +
  scale_y_continuous(trans = 'log10')

