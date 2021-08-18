# description -------------------------------------------------------------

# this script checks the correlation between interaction & explanatory variables

# d_large_uca_pop & prop_industry/services

# setup -------------------------------------------------------------------
rm(list=ls())
source('R/setup.R')

# read  ---------------------------------------------------------------
df <- readr::read_rds('../../data/urbanformbr/pca_regression_df/pca_regression_df_ready_to_use.rds')


# analysis ----------------------------------------------------------------

r_wo_int <- lm(
  data = df,
  formula = y_fuel_consumption_per_capita_2010 ~ x_prop_industry + d_large_uca_pop
)
summary(r_wo_int)

r_int <- lm(
  data = df,
  formula = y_fuel_consumption_per_capita_2010 ~ x_prop_industry + d_large_uca_pop + x_prop_industry:d_large_uca_pop
)

summary(r_int)


anova(r_wo_int,r_int)

# save data ---------------------------------------------------------------

saveRDS(
  object = ,
  file = '../../data/urbanformbr/pca_regression_df/',
  compress = 'xz'
)

