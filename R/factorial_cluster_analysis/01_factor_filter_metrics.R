# description -------------------------------------------------------------

# this script merges all metrics necessary for factor and cluster analysis and saves the
# dataframe for it to be used

# setup -------------------------------------------------------------------

source('R/fun_support/setup.R')

# read data ---------------------------------------------------------------

# * ghsl density ----------------------------------------------------------
df_density <- data.table::fread("../../data/urbanformbr/consolidated_data/ghsl_experienced_density_metrics.csv")

df_density <- df_density %>%
  dplyr::select(code_urban_concentration, )

# * land use mix ----------------------------------------------------------


# * fragmentation compacity -----------------------------------------------


# * street metrics --------------------------------------------------------


# merge data --------------------------------------------------------------



# save data ---------------------------------------------------------------


