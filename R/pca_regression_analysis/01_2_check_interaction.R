# description -------------------------------------------------------------

# this script checks the correlation between interaction & explanatory variables

# d_large_uca_pop & prop_industry/services

# setup -------------------------------------------------------------------
rm(list=ls())
source('R/setup.R')

# read  ---------------------------------------------------------------
df <- readr::read_rds('../../data/urbanformbr/pca_regression_df/pca_regression_df_ready_to_use.rds')


# sector ----------------------------------------------------------------


# * industry --------------------------------------------------------------
r_wo_int_ind <- lm(
  data = df,
  formula = y_fuel_consumption_per_capita_2010 ~ x_prop_industry + d_large_uca_pop
)
summary(r_wo_int_ind)

r_int_ind <- lm(
  data = df,
  formula = y_fuel_consumption_per_capita_2010 ~ x_prop_industry + d_large_uca_pop + x_prop_industry:d_large_uca_pop
)

summary(r_int_ind)


anova(r_wo_int_ind,r_int_ind)


# * services --------------------------------------------------------------
r_wo_int_ser <- lm(
  data = df,
  formula = y_fuel_consumption_per_capita_2010 ~ x_prop_services + d_large_uca_pop
)
summary(r_wo_int_ser)

r_int_ser <- lm(
  data = df,
  formula = y_fuel_consumption_per_capita_2010 ~ x_prop_services + d_large_uca_pop + x_prop_services:d_large_uca_pop
)

summary(r_int_ser)


anova(r_wo_int_ser,r_int_ser)

# * industry & services ---------------------------------------------------
r_wo_int_both <- lm(
  data = df,
  formula = y_fuel_consumption_per_capita_2010 ~
    x_prop_industry + x_prop_services + d_large_uca_pop
)
summary(r_wo_int_both)

r_int_both <- lm(
  data = df,
  formula = y_fuel_consumption_per_capita_2010 ~
    x_prop_industry + x_prop_services + #d_large_uca_pop +
    x_prop_industry:d_large_uca_pop + x_prop_services:d_large_uca_pop
)

summary(r_int_both)


anova(r_wo_int_both,r_int_both)


# work place --------------------------------------------------------------


# * other muni ------------------------------------------------------------
r_wo_int_other <- lm(
  data = df,
  formula = y_fuel_consumption_per_capita_2010 ~
    x_prop_work_other_muni_nucleo + d_isolated_muni
)
summary(r_wo_int_other)

r_int_other <- lm(
  data = df,
  formula = y_fuel_consumption_per_capita_2010 ~
    x_prop_work_other_muni_nucleo + d_isolated_muni +
    x_prop_work_other_muni_nucleo:d_isolated_muni
)

summary(r_int_other)


anova(r_wo_int_other,r_int_other)


# save data ---------------------------------------------------------------

saveRDS(
  object = ,
  file = '../../data/urbanformbr/pca_regression_df/',
  compress = 'xz'
)

