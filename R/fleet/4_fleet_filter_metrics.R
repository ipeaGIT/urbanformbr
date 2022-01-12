# description -------------------------------------------------------------

# this script filters fleet metrics to be used at regression analysis


# setup -------------------------------------------------------------------

source('R/fun_support/setup.R')

# read data ---------------------------------------------------------------

# * urban shapes ----------------------------------------------------------
df_shapes <- readr::read_rds("../../data/urbanformbr/urban_area_shapes/urban_area_pop_100000_dissolved.rds")

df_shapes <- df_shapes %>%
  sf::st_drop_geometry() %>%
  dplyr::select(-pop_ibge_total_2010)

# * fleet mean ------------------------------------------------------------------
df_fleet_age_mean <- readr::read_rds("../../data/urbanformbr/denatran/denatran_mean-fleet-age.rds")

# * fleet class -----------------------------------------------------------------
df_fleet_age_class <- readr::read_rds("../../data/urbanformbr/denatran/denatran_proportion-vehicle-classe.rds")


# clean data --------------------------------------------------------------

# * fleet mean ------------------------------------------------------------

df_fleet_age_mean <- df_fleet_age_mean[classe == "carro" & code_urban_concentration %in% df_shapes$code_urban_concentration]

df_fleet_age_mean[, classe := NULL]

data.table::setnames(df_fleet_age_mean, "age", "fleet_age_mean")


# * fleet class -----------------------------------------------------------

df_fleet_age_class <- df_fleet_age_class[classe == "carro" & status == "0-10" & code_urban_concentration %in% df_shapes$code_urban_concentration]

df_fleet_age_class[, classe := NULL]
df_fleet_age_class[, status := NULL]

data.table::setnames(df_fleet_age_class, "prop_veh_status", "prop_new_auto")


# merge data --------------------------------------------------------------

df_fleet <- dplyr::left_join(df_fleet_age_mean, df_fleet_age_class)

# save data ---------------------------------------------------------------
data.table::fwrite(
  x = df_fleet
  , file = '../../data/urbanformbr/consolidated_data/denatran_fleet_metrics.csv'
  , sep = ";"
  , append = F
)


