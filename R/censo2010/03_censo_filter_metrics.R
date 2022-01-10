# description -------------------------------------------------------------

# this script filters censo metrics to be used at cluster/factor and regression analysis


# setup -------------------------------------------------------------------

source('R/fun_support/setup.R')


# read data and select vars ---------------------------------------------------------------
df_censo <- data.table::fread('../../data/urbanformbr/censo/censo_non_filtered_df.csv')

df_censo <- df_censo %>%
  dplyr::select(
    code_urban_concentration
    , pop_2010
    , prop_dom_urban
    , prop_high_educ
    , prop_razao_dep
    , wghtd_mean_household_income_per_capita
    )

# save data ---------------------------------------------------------------
data.table::fwrite(
  x = df_censo
  , file = '../../data/urbanformbr/consolidated_data/censo_metrics.csv'
  , sep = ";"
  , append = F
)


