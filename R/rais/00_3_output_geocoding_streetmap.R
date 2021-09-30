# description -------------------------------------------------------------

# from: https://github.com/ipeaGIT/geocode/blob/main/1.1-rais_output.R

# this script reads geocoded data from rais (using streetmap), reads rais raw..
#..and merges them, adding geocoding information to rais data

# setup -------------------------------------------------------------------

source('R/setup.R')

options(scipen = 99999)

# define function ---------------------------------------------------------

# DEPOIS, FAZER 1986-2009 E VERIFICAR DIFERENCA ENTRE ANOS

# ano <- 2010
f_rais_output_streetmap <- function(ano) {


  # checar colunas
  col_names <- fread(
    sprintf("../../data/geocode/rais/%s/rais_%s_output_geocode_streetmap.csv",ano,ano),
    nrows = 100,
    colClasses = "character") %>%
    colnames()

  # * not 2010 --------------------------------------------------------------

  if (ano != 2010) {


  # * 2010 ------------------------------------------------------------------
  } else{

  columns <- c("id_estab", "ano", "Status", "Match_addr","Score", "Addr_type",
               "lon_output", "lat_output",
               "logradouro", "name_muni", "code_muni", "uf")

  rais_geocoded <- data.table::fread(
    sprintf("../../data/geocode/rais/%s/rais_%s_output_geocode_streetmap.csv",ano,ano),
    encoding = "UTF-8",
    colClasses = "character",
    select = columns
  )

  }

  # limpar nome colunas
  rais_geocoded <- janitor::clean_names(rais_geocoded)

  # renomear colunas
  data.table::setnames(
    rais_geocoded,
    old = c("match_addr","lon_output","lat_output"),
    new = c("matched_address","lon","lat")
  )

  # id_estab para 14 caracteres
  rais_geocoded[, id_estab := stringr::str_pad(id_estab, width = 14, pad = 0)]

  fwrite(
    rais_geocoded,
    sprintf("../../data/geocode/rais/%s/rais_%s_raw_geocoded.csv", ano, ano)
    )

}





# run function ------------------------------------------------------------
anos <- 2010:2016

future::plan(future::multisession)

furrr::future_walk(
  anos,
  ~f_rais_output_streetmap(.)
)

