# description -------------------------------------------------------------

# this script
# 1. plots built up area and population data saved at 03_1 and 03_2


# setup -------------------------------------------------------------------

source('R/setup.R')

# directory ---------------------------------------------------------------

ghsl_results_dir <- "//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/results/"


# read data ---------------------------------------------------------------
uca_all_final <- readr::read_rds('//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/results/uca_pop_100000_built_up_area_population_results.rds')

uca_pivot <- uca_all_final %>%
  setDT() %>%
  select(name_uca_case,bua_mean1975:pop2015) %>%
  pivot_longer(
    cols = bua_mean1975:pop2015,
    names_to = 'variavel'
  ) %>%
  separate(variavel,c('variavel','ano'), -4) %>%
  setDT()

uca_pivot[ano %in% c(2014,2015), ano := 2015]

uca_pivot <- uca_pivot %>%
  pivot_wider(id_cols = c(name_uca_case,ano),names_from = variavel,values_from = value)



uca_pivot %>%
  #st_transform(4326) %>%
  ggplot() +
  geom_point(aes(x = ano, y = bua_mean, size = pop))

uca_pivot %>%
  #st_transform(4326) %>%
  filter(ano == 2015) %>%
  ggplot() +
  geom_point(aes(x = bua_mean, y = pop))
