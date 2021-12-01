# description -------------------------------------------------------------

# this script cleans pop data from ghsl (1975 and 2015) and estimates geometric
#..pop growth between these years to be used at the regression analysis

# setup -------------------------------------------------------------------
source('R/setup.R')

# read and clean data -----------------------------------------------------
df_prep <- readr::read_rds("../../data/urbanformbr/pca_regression_df/pca_regression_df.rds") %>%
  dplyr::select(-c(code_muni_uca))

df_pop_ghsl <- readr::read_rds('../../data/urbanformbr/ghsl/results/uca_pop_100000_built_up_area_population_results.rds')

df_pop_ghsl <- df_pop_ghsl %>%
  sf::st_drop_geometry() %>%
  dplyr::select(code_urban_concentration, pop1975, pop2015) %>%
  rename(
    pop_ghsl_1975 = pop1975,
    pop_ghsl_2015 = pop2015
    ) %>%
  dplyr::filter(code_urban_concentration %in% df_prep$code_urban_concentration)

data.table::setDT(df_pop_ghsl)[
  ,
  pop_growth_1975_2015 := ( (pop_ghsl_2015 / pop_ghsl_1975) ^ (1/40) ) - 1,
  by = .(code_urban_concentration)
]

df_pop_ghsl[
  ,
  `:=`(
    pop_ghsl_1975 = NULL
    ,pop_ghsl_2015 = NULL
  )
  ]


# save data ---------------------------------------------------------------
readr::write_rds(
  x = df_pop_ghsl,
  file = '../../data/urbanformbr/pca_regression_df/pop_growth_ghsl.rds',
  compress = 'gz'
)

