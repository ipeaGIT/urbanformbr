# description -------------------------------------------------------------

# this script filters censo metrics to be used at cluster/factor and regression analysis


# setup -------------------------------------------------------------------

source('R/fun_support/setup.R')


# read data and select vars ---------------------------------------------------------------
df_censo <- data.table::fread('../../data/urbanformbr/censo/censo_non_filtered_df.csv')

df_censo <- df_censo %>%
  dplyr::select(666666666666 COMPLETAR)

# save data ---------------------------------------------------------------
data.table::fwrite(
  x = df_censo
  , file = '../../data/urbanformbr/consolidated_data/censo_metrics.csv'
  , sep = ";"
  , append = F
)


