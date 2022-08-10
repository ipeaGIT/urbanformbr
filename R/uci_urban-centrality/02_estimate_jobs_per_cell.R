
# description -------------------------------------------------------------


# setup -------------------------------------------------------------------
source("R/fun_support/setup.R")


# setup parallel ----------------------------------------------------------

#future::plan(future::multicore, workers = future::availableCores() / 2)


# function ----------------------------------------------------------------

f_uci <- function(){


# read data ---------------------------------------------------------------


# * uca -------------------------------------------------------------------
  df_uca <- readr::read_rds("../../data/urbanformbr/urban_area_shapes/urban_area_pop_100000_dissolved.rds")

  df_uca_muni <- geobr::read_urban_concentrations(simplified = F)


# * RAIS ------------------------------------------------------------------
  df_rais <- readRDS('../../data/urbanformbr/rais/2010/rais_2010_geocoded_sum_of_workers')

# * grid ------------------------------------------------------------------
  66666666 NECESSARY CREATE UCA GRID POLYGON FOR WHOLE POLITICAL-ADMINISTRATIVE AREA (without 20% cutoff)

  df_grid <- readRDS("../../data/urbanformbr/ghsl/results/")

# save data ---------------------------------------------------------------

saveRDS(
  object = df_merge,
  file = '../../data/urbanformbr/rais/2010/rais_2010_geocoded_sum_of_workers',
  compress = 'xz'
)

}



