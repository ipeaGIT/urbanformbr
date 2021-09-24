# description -------------------------------------------------------------

# this script
# i. reads data from FUA and UCDB datasets
## Functional Urban Areas (FUA) and Urban Centre Database (UCDB)
# ii. filter data from brazilian cities
# iii. saves data as .rds for future cleaning an manipulation

# setup -------------------------------------------------------------------

source('R/setup.R')

# directory ---------------------------------------------------------------

#ghsl_dir <- "//storage6/usuarios/Proj_acess_oport/data-raw/ghsl"



# define function ---------------------------------------------------------

filter_savebr <- function(){


# 1 read raw data ---------------------------------------------------------

# * 1.1 fua ---------------------------------------------------------------

fua <- sf::read_sf('../../data-raw/ghsl/fua/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0.gpkg')

#dplyr::glimpse(fua)

# * 1.2 ucdb --------------------------------------------------------------

ucdb <- sf::read_sf("../../data-raw/ghsl/ucdb/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg")

#dplyr::glimpse(ucdb)


# 2 filter brazilian cities -----------------------------------------------

# * 2.1 fua ---------------------------------------------------------------

fua <- fua %>% dplyr::filter(Cntry_ISO=="BRA")

#fua %>% dplyr::filter(eFUA_name =="Belo Horizonte") %>% mapview()


# * 2.2 ucdb --------------------------------------------------------------

ucdb <- ucdb %>% dplyr::filter(CTR_MN_ISO == "BRA")

#ucdb %>% dplyr::filter(UC_NM_MN == "Belo Horizonte") %>% mapview::mapview()


# 3 save rds data ---------------------------------------------------------

readr::write_rds(
  x = fua,
  file = "../../data-raw/ghsl/fua/fua_br_2015_1k_v1.rds",
  compress = 'gz'
)

readr::write_rds(
  x = ucdb,
  file = "../../data-raw/ghsl/ucdb/ucdb_br_2015_v1_2.rds",
  compress = 'gz'
)


}


# run function ------------------------------------------------------------
filter_savebr()

