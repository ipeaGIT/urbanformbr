# this scripts prepare the data for the DAG and regression models

library(data.table)
library(readr)
library(tidyverse)




# library(pbapply)
# library(stargazer)
# library(caret)
# library(glmnet)
# library(mctest)
# library(doParallel)
# library(foreach)
# library(jtools)
# library(interactions)
# library(fixest)

options(scipen = 99)
`%nin%` <- Negate(`%in%`)




############### 1.1 read data
# df_raw <- readr::read_rds("../../data/urbanformbr/pca_regression_df/pca_regression_df_ready_to_use.rds")
df_raw <- data.table::fread("C:/Users/user/Downloads/consolidated_data1/urbanformbr_metrics_full.csv", encoding = 'UTF-8')
glimpse(df_raw)



#### remove areas in the international border ------------
#' Internacional de Uruguaiana/Brasil (RS) 4322400
#' Internacional de Foz do Iguaçu/Brasil - Ciudad del Este/Paraguai (PR) 4108304
#' Internacional de Corumba/Brasil (MS) 5003207

# and Santa Cruz do Sul/RS because of min. urban area in 1975
#' Santa Cruz do Sul/RS 4316808

to_be_removed <- c(4322400, 4108304, 5003207, 4316808)
df_raw <- subset(df_raw, i_code_urban_concentration %nin% to_be_removed)


############### 1.2 convert values to log

# id columns
id_cols <- c( names(df_raw)[ names(df_raw) %like% 'i_' ], 'x_state')
glimpse(df_raw)


# cols no to log because of non-positive values
summary(df_raw$x_total_pop_growth_1990_2014)
cols_not_to_log <- c( 'f_compact_contig_inter_dens'
                      , 'x_total_pop_growth_1990_2014'
                      , 'd_tma'
                      , 'x_leapfrog_pop_growth_1990_2014'
                      , 'x_infill_pop_growth_1990_2014'
                      , 'x_extension_pop_growth_1990_2014'
                      , 'x_prop_pop_sub'
                      , 'x_upward_pop_growth_1990_2014'
                      )

cols_to_log <- colnames(df_raw)[ colnames(df_raw) %nin% c(id_cols, cols_not_to_log) ]

# log transformation
df_log <- copy(df_raw)
df_log[, (cols_to_log) := lapply(.SD, function(x){ log(x) } ), .SDcols=cols_to_log]

#' because of non-positive values, we use
#' inverse hyperbolic sine transformation,
#' which has the same effect and interpretation
df_log[, (cols_not_to_log) := lapply(.SD, function(x){ log(x + sqrt(x^2 + 1) ) } ), .SDcols=cols_not_to_log]

glimpse(df_log)
summary(df_log)




############### 1.3 select variables to keep

cols_to_keep <- c( ## id columns
              #     'i_code_urban_concentration'
                   'x_state'
              #    , 'i_name_urban_concentration'
              #    , 'i_name_uca_case'
              #    , 'i_name_state'
              #    , 'i_name_region'

                   ## dependent vars
                   , 'y_energy_per_capita'
                   # , 'y_wghtd_mean_commute_time'

                   ## local context covariates
                   #, 'x_sd_elevation'
                   , 'x_mean_slope'
                   , 'x_total_pop_growth_1990_2014'
                   , 'x_mean_fleet_age' # ----- ? x_mean_age_auto ?
                   , 'x_prop_dom_urban'
                   # , 'x_prop_motos_dom'
                   , 'x_prop_autos_dom'
                   , 'x_wghtd_mean_household_income_per_capita'
                   , 'x_prop_high_educ'
                   , 'x_prop_razao_dep'
                   , 'x_pop_2010'
                   , 'x_street_pop'
                   # , 'x_prop_pop_sub'
                   # , 'd_tma'
                   # , 'x_prop_employed'
                   # , 'x_prop_formal'
                   # ? 'x_prop_new_auto'

                   ## urban form
                   , 'f_compact_contig_inter_dens'
                   , 'x_density_pop_02km_2014'
                   # , 'x_density_pop_03km_2014'
                   # , 'x_density_pop_05km_2014'
                   # , 'x_density_pop_10km_2014'
                   , 'x_land_use_mix'
                   # , 'x_contiguity_2014'
                   # , 'x_compacity_2014'
                   , 'x_circuity_avg'
                   , 'x_intersection_density_km'
                   , 'x_normalized_closeness_centrality_avg'
                   )



### Keep vars
df_fuel <- dplyr::select(df_log, all_of(cols_to_keep))
head(df_fuel)



