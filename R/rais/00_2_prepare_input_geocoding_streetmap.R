# description -------------------------------------------------------------

# from: https://github.com/ipeaGIT/geocode/blob/main/1.1-rais_input.R

# this script prepares employment data from rais to be used as an input at streetmap
# to be geocoded.
# data from the years 2010-2016 are to be used

# variables necessary for geocoding with streepmap:
# id_estab: firm id
# qtd_vinc_ativos: number of active employees
# logradouro: address
# bairro: neighborhood
# code_muni: municipality code
# name_muni: municipality name
# uf: state abbreviation
# cep: postal code


# all firms from 2010 are geocoded
# from 2010 to 2016, geocoded firms are:
## new firms (that are not present in the previous year)
## firms that have changed location (i.e., firms that have changed postal code)

# setup -------------------------------------------------------------------

source('R/setup.R')

# define function ---------------------------------------------------------

# FAZER PRIMEIRO 2010-2016
# DEPOIS, FAZER 1986-2009 E VERIFICAR DIFERENCA ENTRE ANOS

# ano <- 2010
f_rais_input_streetmap <- function(ano) {


  # 1) Dados dos estabelecimentos

  # 1.1) Abrir dados
  # select columns
  col_names <- fread(
    sprintf('//storage6/bases/DADOS/RESTRITO/RAIS/csv/estab%s.csv', ano),
    nrows = 100,
    #select = c("id_estab", "qt_vinc_ativos", 'nat_jur2018',  "logradouro", "bairro", "codemun", "uf", "cep"),
    colClasses = "character") %>%
    colnames()

  if (ano != 2010) {######## COMPLETAR ELSE 2011 PRA FRENTE
    columns <- c("id_estab", "qt_vinc_ativos", col_names[col_names %like% "nat_jur"],
                 "logradouro", "bairro", "codemun", "uf", "cep")
  } else {
    columns <- c("id_estab", "qt_vinc_ativos", col_names[col_names %like% "nat_jur"],
                 "logradouro", "codemun", "uf", "cep")

    rais_estabs <- fread(sprintf('//storage6/bases/DADOS/RESTRITO/RAIS/csv/estab%s.csv', ano)
                         #, nrows = 5
                         , colClasses='character'
                         , select = columns)

    # 1.3) Renomear columns
    colnames(rais_estabs) <- c("id_estab", "qt_vinc_ativos", "nat_jur", "logradouro",
                               "codemun", "uf", "cep")

    # todo os estabs para 14 characetrs
    rais_estabs[, id_estab := stringr::str_pad(id_estab, width = 14, pad = 0)]

  }

6666666 CONTINUAR






  # 4) Trazer o nome do municipio
  muni_lookup <- geobr::lookup_muni(code_muni = "all")
  muni_lookup <- muni_lookup %>%
    select(codemun = code_muni, name_muni, abrev_state) %>%
    mutate(codemun = substr(codemun, 1, 6))

  rais_estabs <- rais_estabs %>%
    left_join(muni_lookup, by = "codemun")


  # todo os CEPS para 8 digitos characetrs
  rais_estabs[, cep := stringr::str_pad(cep, width = 8, pad = 0)]

  # correcao de logradouro, retira '999999'
  rais_estabs[ , logradouro := str_replace(string = logradouro,
                                           pattern = ', 999999$',
                                           replacement = '')]
  rais_estabs[ , logradouro := str_replace(string = logradouro,
                                           pattern = ' 999999$',
                                           replacement = '')]

  rais_estabs[ , logradouro := str_replace(string = logradouro,
                                           pattern = ', 99999$',
                                           replacement = '')]
  rais_estabs[ , logradouro := str_replace(string = logradouro,
                                           pattern = ' 99999$',
                                           replacement = '')]

  rais_estabs[ , logradouro := str_replace(string = logradouro,
                                           pattern = ', 9999$',
                                           replacement = '')]
  rais_estabs[ , logradouro := str_replace(string = logradouro,
                                           pattern = ' 9999$',
                                           replacement = '')]


  # 2) Se o ano nao for 2017, selecionar somente os estabelecimentos que sao
  # novos ou que mudaram de endereço e um ano para o outro
  if (ano != 2017) {

    # 2.1) Abrir do ano anterior
    rais_estabs_anterior <- fread(sprintf('//storage6/bases/DADOS/RESTRITO/RAIS/csv/estab%s.csv', ano-1)
                                  #, nrows = 5
                                  , colClasses='character'
                                  , select = c("id_estab", "cep"))
    # Pad to 14 chars
    rais_estabs_anterior[, id_estab := str_pad(id_estab, width = 14, pad = 0)]

    # 2.2) Selecionar somente os estabs que nao foram georef antes
    rais_estabs_geocode_new <- rais_estabs[id_estab %nin% rais_estabs_anterior$id_estab]

    # identify if estab is new or whether it comes from previous year
    rais_estabs_geocode_new[, type_input_galileo := paste0("new_estab_", ano)]

    message("Total of new estabs compared to previous year: ", nrow(rais_estabs_geocode_new))

    # 2.3) Selecionar somente os estabs que mudaram o cep em relacao ao ano anterior
    # Pad to 8 chars
    rais_estabs_anterior[, cep := str_pad(cep, width = 8, pad = 0)]
    rais_estabs_geocode_new[, cep := str_pad(cep, width = 8, pad = 0)]
    rais_estabs[, cep := str_pad(cep, width = 8, pad = 0)]

    # identifica empresas que foram geocode no ano anterior, mas que mudaram de endereco no ano atual
    rais_geocode_cep <- left_join(rais_estabs, select(rais_estabs_anterior, id_estab, cep),
                                  by = "id_estab",
                                  suffix = c("_anterior", "_atual")) %>%
      mutate(cep_anterior = substr(cep_anterior, 1, 6)) %>%
      mutate(cep_atual = substr(cep_atual, 1, 6)) %>%
      mutate(cep_igual = ifelse(cep_anterior == cep_atual, "sim", "nao")) %>%
      filter(cep_igual == "nao") %>%
      rename(cep = cep_atual) %>%
      # rename(cep = cep_atual, logradouro = logradouro_atual) %>%
      select(-cep_anterior, -cep_igual) %>%
      # identify type
      mutate(type_input_galileo = paste0("cep_changed_", ano))


    message("Total of estabs that changed CEP: ", nrow(rais_geocode_cep))

    # 2.4) Juntar os novos estabs a serem georef
    rais_geocode <- rbind(rais_estabs_geocode_new, rais_geocode_cep)
    rais_geocode <- unique(rais_geocode) %>% setDT()
    message("Total of to be geocoded: ", nrow(rais_geocode))

    # 2.5) Selecionar colunas e salvar input para galileo
    rais_geocode %>%
      # fix uf and codemun
      select(id_estab, qt_vinc_ativos, logradouro, bairro, code_muni = codemun, name_muni, uf = abrev_state, cep, type_input_galileo) %>%
      fwrite(sprintf("../../data/geocode/rais/rais_%s_input_geocode.csv", ano))


  } else if (ano == 2017) {

    rais_geocode <- rais_estabs %>%
      mutate(type_input_galileo = paste0("rais_", "2017"))

    message("Total of to be geocoded: ", nrow(rais_geocode))

    # Encoding(rais_geocode$name_muni)

    # 2.5) Selecionar colunas e salvar input para galileo
    rais_geocode %>%
      # fix uf and codemun
      select(id_estab, qt_vinc_ativos, logradouro, bairro, code_muni = codemun, name_muni, uf = abrev_state, cep, type_input_galileo) %>%
      fwrite(sprintf("../../data/geocode/rais/rais_%s_input_geocode.csv", ano))

  }




}

# run function ------------------------------------------------------------


