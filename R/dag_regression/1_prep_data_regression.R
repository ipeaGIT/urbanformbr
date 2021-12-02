#' https://tanthiamhuat.files.wordpress.com/2019/06/penalized-regression-essentials.pdf


library(pbapply)
library(data.table)
library(stargazer)
library(readr)
library(tidyverse)
library(caret)
library(glmnet)
library(mctest)
library(doParallel)
library(foreach)
library(jtools)
library(interactions)
library(fixest)

options(scipen = 99)
`%nin%` <- Negate(`%in%`)



############### 1. prep data --------------------------

############### 1.1 read data
df_raw <- readr::read_rds("../../data/urbanformbr/pca_regression_df/pca_regression_df_ready_to_use.rds")
glimpse(df_raw)


## add columns with state and region
df_raw$x_state <- substring(df_raw$i_code_urban_concentration, 1,2) %>% as.numeric()


region_labels <- geobr::read_state() %>% setDT()
df_raw[region_labels, on=c('x_state'='code_state'), c('name_state' , 'name_region') := list(i.name_state, i.name_region)]



#### remove areas in the international border ------------
#' Internacional de Uruguaiana/Brasil (RS) 4322400
#' Internacional de Foz do Iguaçu/Brasil - Ciudad del Este/Paraguai (PR) 4108304
#' Internacional de Corumba/Brasil (MS) 5003207

# and Santa Cruz do Sul/RS because of min. urban area in 1975
#' Santa Cruz do Sul/RS 4316808

to_be_removed <- c(4322400, 4108304, 5003207, 4316808)
df_raw <- subset(df_raw, i_code_urban_concentration %nin% to_be_removed)

### 666666666666666666666666666
df_raw[ i_name_urban_concentration %like% 'Internacional']
a[ name_urban_concentration %like% 'Santa Cruz do Sul']

a <- geobr::read_urban_concentrations()
a <- subset(a, pop_total_2010 >=100000)
unique(a$code_urban_concentration) %>% length()

subset(a, code_urban_concentration %in% to_be_removed)


############### 1.2 convert values to log
#' because of non-positive values, we use
#' inverse hyperbolic sine transformation,
#'  which has the same effect and interpretation

# cols not to log

# id columns
id_cols <- c('i_code_urban_concentration', 'i_name_urban_concentration', 'i_name_uca_case')

# cols no to log because of non-positive values
cols_not_to_log <- c(  'x_region','name_region',
                       'x_state', 'name_state',
                       'd_large_uca_pop'
                      , 'd_tma'
                     # ,  'x_land_use_mix'
                      , 'x_pop_growth_15_00'
                      , 'x_prop_work_from_home_res_not_nucleo'
                      , 'x_prop_work_other_muni_res_nucleo'
                      , 'x_prop_work_other_muni_res_not_nucleo'
                      , 'f_compact_contig_inter_dens'
                      , 'x_prop_pop_sub'
                     , 'x_pop_growth_1975_2015'
                     , 'x_prop_slope_above_10'
                      )
cols_to_log <- colnames(df_raw)[ colnames(df_raw) %nin% c(id_cols, cols_not_to_log) ]

df_log <- copy(df_raw)
df_log[, (cols_to_log) := lapply(.SD, function(x){ log(x) } ), .SDcols=cols_to_log]
# df_log[, (cols_to_log) := lapply(.SD, function(x){ log(x + sqrt(x^2 + 1) ) } ), .SDcols=cols_to_log]




############### 1.3 select variables to drop
# dropping built area vars because we measure 'compactness / sprawl' with the x_avg_cell_distance var already
drop1 <- c(  'x_built_total_2014'
            # 'x_urban_extent_size_2014'
           , 'x_prop_built_consolidated_area_2014'
           , 'd_tma'
           ,'name_region'
           , 'name_state'
           #, 'x_urban_extent_size_2014'
           )


drop2 <- c('x_sd_elevation' #, 'x_mean_slope' # we already use x_circuity_avg
           , 'd_large_uca_pop'              # we control for pop continuous
           # , 'x_rooms_per_household'        # we control for experienced density
           # , 'x_residents_per_household'    # we control for experienced density
          # , 'x_prop_black'                 # no strong theoretical justification
           , 'x_street_length' #  we control for x_street_pop
           )


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# drop on radius in experimented measures
drop3 <- c(
           # 'x_density_pop_10km_2015',
           'x_built_area_coverage_05km_2014', 'x_built_area_coverage_10km_2014',
           'x_land_use_mix_5km', 'x_land_use_mix_10km', 'x_land_use_mix_15km'
           )


# other multicolinear variables
drop4 <- c( 'x_prop_services'  # colinear with x_prop_industry
        #  , 'x_prop_employed'  # colinear with x_wghtd_mean_household_income_per_capita
         # , 'x_prop_high_educ' # colinear with x_wghtd_mean_household_income_per_capita
         # , 'x_prop_formal'    # colinear with x_wghtd_mean_household_income_per_capita
         # , 'x_k_avg'           # colinear with x_wghtd_mean_household_income_per_capita
)

# work from home
drop5 <- c(   'x_prop_work_from_home_res_nucleo'
            , 'x_prop_work_from_home_res_not_nucleo'
            , 'x_prop_work_other_muni_res_nucleo'
            , 'x_prop_work_other_muni_res_not_nucleo')



### drop vars
df_fuel <- dplyr::select(df_log, - c('y_wghtd_mean_commute_time', all_of(c(id_cols, drop1, drop2, drop3, drop4, drop5)) ))
# df_time <- dplyr::select(df_log, - c('y_fuel_consumption_per_capita_2010', all_of(c(id_cols, drop1, drop2, drop3, drop4, drop5)) ))
head(df_fuel)
# df_fuel <- dplyr::select(df_log, - all_of(c('y_wghtd_mean_commute_time',id_cols)))




