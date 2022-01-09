# description -------------------------------------------------------------
# this scritp selects the metrics (columns) generated at R/topography/01 and saves
# the dataset used at factor/cluster and regression analysis


# setup -------------------------------------------------------------------
source("R/fun_support/setup.R")


# read data ---------------------------------------------------------------
df_topo <- data.table::fread('../../data/urbanformbr/topography/topography_non_filtered_df.csv')

df_topo <- df_topo %>%
  dplyr::select(code_urban_concentration, mean_slope)

# save results ---------------------------------------------------------------

data.table::fwrite(
  x = df_topo
  , file = '../../data/urbanformbr/consolidated_data/topography_metrics.csv'
  , sep = ";"
  , append = F
  )

