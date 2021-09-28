# description -------------------------------------------------------------

# from: https://github.com/ipeaGIT/geocode/blob/main/1.1-rais_input.R

# this script prepares employment data from rais to be used as an input at streetmap
# to be geocoded.
# data from 2010 to 2016

# variables necessary for geocoding with streepmap:
# id_estab: firm id
# qtd_vinc_ativos: number of active employees
# logradouro: address
# bairro: neighborhood
# code_muni: municipality code
# name_muni: municipality name
# uf: state abbreviation
# cep: postal code


# all firms from will be geocoded

# setup -------------------------------------------------------------------

source('R/setup.R')

# define function ---------------------------------------------------------

# FAZER PRIMEIRO 2010-2016
# DEPOIS, FAZER 1986-2009 E VERIFICAR DIFERENCA ENTRE ANOS

# ano <- 2010
f_rais_input_streetmap <- function(ano) {

  # checar colunas
  col_names <- fread(
    sprintf('//storage6/bases/DADOS/RESTRITO/RAIS/csv/estab%s.csv', ano),
    nrows = 100,
    #select = c("id_estab", "qt_vinc_ativos", 'nat_jur2018',  "logradouro", "bairro", "codemun", "uf", "cep"),
    colClasses = "character") %>%
    colnames()

  # * not 2010 ------------------------------------------------------------------
  666666666666666 COMPLETAR PARA 2011 EM DIANTE; RODAR TUDO EM PARALELO DEPOIS
  if (ano != 2010) {######## COMPLETAR ELSE 2011 PRA FRENTE
    columns <- c("id_estab", "qt_vinc_ativos", col_names[col_names %like% "nat_jur"],
                 "logradouro", "bairro", "codemun", "uf", "cep")


  # * 2010 ------------------------------------------------------------------

  } else {

    # 1) dados dos estabelecimentos

    # selecionar colunas
    columns <- c("id_estab", "qt_vinc_ativos", col_names[col_names %like% "nat_jur"],
                 "logradouro", "codemun", "uf", "cep")

    # abrir dados
    rais_estabs <- data.table::fread(sprintf('//storage6/bases/DADOS/RESTRITO/RAIS/csv/estab%s.csv', ano)
                         #, nrows = 5
                         , colClasses='character'
                         , select = columns)

    # renomear colunas
    colnames(rais_estabs) <- c("id_estab", "qt_vinc_ativos", "nat_jur", "logradouro",
                               "codemun", "uf", "cep")

    # trazer todos estab id para 14 caracteres
    rais_estabs[, id_estab := stringr::str_pad(id_estab, width = 14, pad = 0)]

    # pegar nome do municipio
    muni_lookup <- geobr::lookup_muni(code_muni = "all")

    muni_lookup <- muni_lookup %>%
      dplyr::select(codemun = code_muni, name_muni, abrev_state) %>%
      dplyr::mutate(codemun = substr(codemun, 1, 6))

    rais_estabs[
      muni_lookup,
      `:=`(
        name_muni = i.name_muni,
        abrev_state = i.abrev_state
      ),
      on = c("codemun" = "codemun")
    ]

    # todo os CEPS para 8 caracteres
    rais_estabs[, cep := stringr::str_pad(cep, width = 8, pad = 0)]

    # correcao de logradouro, retirar '999999' e semelhantes
    rais_estabs[ , logradouro := stringr::str_replace(string = logradouro,
                                             pattern = ', 999999$',
                                             replacement = '')]
    rais_estabs[ , logradouro := stringr::str_replace(string = logradouro,
                                             pattern = ' 999999$',
                                             replacement = '')]

    rais_estabs[ , logradouro := stringr::str_replace(string = logradouro,
                                             pattern = ', 99999$',
                                             replacement = '')]
    rais_estabs[ , logradouro := stringr::str_replace(string = logradouro,
                                             pattern = ' 99999$',
                                             replacement = '')]

    rais_estabs[ , logradouro := stringr::str_replace(string = logradouro,
                                             pattern = ', 9999$',
                                             replacement = '')]
    rais_estabs[ , logradouro := stringr::str_replace(string = logradouro,
                                             pattern = ' 9999$',
                                             replacement = '')]

    # criar coluna ano
    rais_estabs[, c("ano") := ano]

    # renomear colunas
    data.table::setnames(
      x = rais_estabs,
      old = c("codemun","abrev_state"),
      new = c("code_muni","uf")
    )

    # informar numero de estabelecimentos que serao geocodificados
    message("Total of to be geocoded: ", nrow(rais_estabs))


    if (!dir.exists(sprintf("../../data/geocode/rais/%s",ano))){
      dir.create(sprintf("../../data/geocode/rais/%s",ano))
    }

    # salvar base
    data.table::fwrite(
      rais_estabs,
      sprintf("../../data/geocode/rais/%s/rais_%s_input_geocode.csv", ano, ano)
    )

  }



}

# run function ------------------------------------------------------------


