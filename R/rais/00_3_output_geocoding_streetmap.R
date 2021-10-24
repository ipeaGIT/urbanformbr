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
  (col_names <- fread(
    sprintf("../../data/geocode/rais/%s/rais_%s_output_geocode_streetmap.csv",ano,ano),
    nrows = 100,
    colClasses = "character") %>%
    colnames()
  )

  # * not 2010 --------------------------------------------------------------

  if (ano != 2010) {

    columns <- c("id_estab", "ano", "Status", "Match_addr","Score", "Addr_type",
                 "lon_output", "lat_output",
                 "logradouro", "bairro", "name_muni", "code_muni", "uf","cep")


  # * 2010 ------------------------------------------------------------------
  } else{

  columns <- c("id_estab", "ano", "Status", "Match_addr","Score", "Addr_type",
               "lon_output", "lat_output",
               "logradouro", "name_muni", "code_muni", "uf","cep")

  }

  columns_classes <- list(
    "double" = "id_estab"
    , "character" = columns[columns %nlike% "id_estab"]
  )

  # ler dados
  rais_geocoded <- data.table::fread(
    sprintf("../../data/geocode/rais/%s/rais_%s_output_geocode_streetmap.csv",ano,ano)
    , encoding = "UTF-8"
    , colClasses = columns_classes
    , select = columns
    #, nrows = 100
  )

  # limpar nome colunas
  rais_geocoded <- janitor::clean_names(rais_geocoded)

  # renomear colunas
  data.table::setnames(
    x = rais_geocoded
    , old = c("match_addr","lon_output","lat_output", "code_muni")
    , new = c("matched_address","lon","lat", "codemun")
  )


  # id_estab para 14 caracteres & cep para 8 caracteres
  rais_geocoded[
    ,
    `:=`(
      id_estab = stringr::str_pad(as.character(id_estab), width = 14, pad = 0)
      , cep = stringr::str_pad(cep, width = 8, pad = 0)
    )
  ]


  # * merge and save --------------------------------------------------------

  # salvar dados
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


