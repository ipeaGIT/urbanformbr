# description -------------------------------------------------------------

# this script estimates multiple factor analysis for all variables of urbanform
# FAZER SO COM URBANFORM?

# setup -------------------------------------------------------------------

source('R/setup.R')

# read data ---------------------------------------------------------------
df_raw <- readr::read_rds('../../data/urbanformbr/pca_regression_df/pca_regression_df_ready_to_use.rds')

# select variables --------------------------------------------------------
df_select <- df_raw %>%
  dplyr::select(
    dplyr::matches("^(i)"),
    d_tma,
    x_urban_extent_size_2014,
    x_prop_built_consolidated_area_2014, # building coverage?
    x_density_pop_05km_total_2014,
    x_land_use_mix,
    #x_n_large_patches,
    x_avg_cell_distance,
    x_k_avg,
    x_intersection_density_km,
    x_circuity_avg,
    x_sd_elevation,
    x_mean_slope
  )

df_area <- readr::read_rds("../../data/urbanformbr/pca_regression_df/area.rds") %>%
  dplyr::select(code_muni,saturation_total_area_fixed_2014 ) %>%
  dplyr::rename(
    i_code_urban_concentration = code_muni,
    x_coverage = saturation_total_area_fixed_2014
    )

df_select <- data.table::merge.data.table(
  df_select,df_area, by = "i_code_urban_concentration")

# change variable class
df_select[
  ,
  x_urban_extent_size_2014 := as.numeric(x_urban_extent_size_2014)
  ]

# factor analysis ---------------------------------------------------------


# * prep data for factanal ------------------------------------------------


# organize the variables into groups -> ALINHAR GRUPOS

## g0: id -> 3 VARS
# i_code_urban_concentration
# i_namee_urban_concentration
# i_name_uca_case

## g1: urban form metrics -> 6 VARS
# x_urban_extent_size_2014
# x_prop_built_consolidated_area_2014
# x_density_pop_05km_total_2014
# x_land_use_mix
# x_avg_cell_distance
# x_coverage

## g2: urban network infrastructure -> 4 VARS
# x_k_avg
# x_intersection_density_km
# x_circuity_avg

## g3: physical characteristics -> 2 VARS
# x_sd_elevation
# x_mean_slope

## g4: categorial variables -> 1 VAR
# d_tma

# after properly classifying each group, reorder columns so that each group is ordered
#..in the dataset
# reorder
df_select <- df_select %>%
  relocate(x_coverage, .after = x_avg_cell_distance) %>%
  relocate(d_tma, .after = x_mean_slope)

#### change datatable to tibble for converting one id column to row.names
df_select <- df_select %>%
  select(-c(i_code_urban_concentration, i_name_uca_case))

666666666 CONTINUAR

a <- data.frame(df_select)

a <- data.frame(
  a[,-1],
  row.names = a[,1]
  )



# define R objects for factor analysis package


# groups
grupo <- c(3,6,3,2,1)
nome_grupo <- c("id","form","network","physical","cat")
tipo <- c("n","s","s","s","n")


# run factor analysis -----------------------------------------------------

res.mfa <- FactoMineR::MFA(
  base = df_select,
  group = grupo,
  name.group = nome_grupo,
  type = tipo,
  excl = c(1,2,3),
  graph = F
)



# save plots --------------------------------------------------------------


