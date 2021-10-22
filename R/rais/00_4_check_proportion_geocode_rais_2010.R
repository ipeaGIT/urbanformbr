# description -------------------------------------------------------------

# from # https://github.com/ipeaGIT/acesso_oport/blob/master/R/fun/empregos/empregos_geo.R

# this script reads geocoded employment data from RAIS 2010; filters for the 184
# ucas in the study; estimates the percentage of good quality geocoding, according
# to the criteria used in the AOP; saves geocoded employment data to estimate
# policentrality metrics


# setup -------------------------------------------------------------------

source('R/setup.R')

# * supplementary data ----------------------------------------------------

# * * df prep ----------------------------------------------------------------

df_prep <- readr::read_rds("../../data/urbanformbr/pca_regression_df/pca_regression_df.rds") %>%
  dplyr::select(-c(code_muni_uca))

# * * df codes ------------------------------------------------------------
df_codes <- readr::read_rds("//storage6/usuarios/Proj_acess_oport/data/urbanformbr/pca_regression_df/pca_regression_df.rds")

df_codes <- df_codes %>%
  dplyr::mutate(
    isolated_muni = data.table::fcase(
      purrr::map_int(code_muni_uca, length) == 1L, 1L,
      default = 0L
    )
  )

df_codes <- df_codes %>%
  tidyr::unnest_longer(code_muni_uca) %>%
  data.table::setDT()

df_codes[
  ,
  `:=`(
    codemun = substr(code_muni_uca, 1, 6),
    nucleo = data.table::fcase(
      code_muni_uca == code_urban_concentration, 1L,
      code_muni_uca != code_urban_concentration, 0L,
      default = 100L
      )
    )
  ]

# * * df pop censo --------------------------------------------------------
df_pop_censo <- readr::read_rds("../../data/urbanformbr/pca_regression_df/1970-2015_pop.rds")

# filter only 184 from our df
df_pop_censo <- subset(df_pop_censo, code_urban_concentration %in% df_prep$code_urban_concentration)

df_pop_censo <- df_pop_censo %>%
  tidyr::pivot_wider(
    names_from = c("ano"),
    values_from = c("pop"),
    names_prefix = "pop_"
  )

df_pop_censo <- df_pop_censo %>%
  dplyr::select(-pop_1970)


# * * df classify large uca -----------------------------------------------

df_classify_uca_large <- readr::read_rds('../../data/urbanformbr/pca_regression_df/classify_uca_large_pop.rds') %>%
  dplyr::select(-pop_uca_2010)

# filter ucas
df_classify_uca_large <- subset(
  df_classify_uca_large,
  code_urban_concentration %in% df_prep$code_urban_concentration
)


# read and clean rais estabs raw ------------------------------------------

ano <- 2010
f_rais_clean_estabs_geocoded <- function(ano) {

  # 1) dados dos estabelecimentos

  # * rais geocode -----------------------------------------------------------
  rais_geocode <- fread(
    file = sprintf("../../data/geocode/rais/%s/rais_%s_raw_geocoded.csv", ano, ano)
    #, select = columns
    , colClasses = "character"
    , encoding = "UTF-8"
    #, nrows = 100
  )

  data.table::setnames(
    x = rais_geocode
    , old = "codemun"
    , new = "code_muni"
    )

  # * rais raw ----------------------------------------------------------------
  col_names_raw <- fread(
    sprintf('//storage6/bases/DADOS/RESTRITO/RAIS/csv/estab%s.csv', ano)
    , nrows = 100
    , colClasses = "character"
    ) %>%
    colnames()

  columns_raw <- c("id_estab", "qt_vinc_ativos",
                   col_names_raw[col_names_raw %like% "nat_jur"])

  rais_raw <- fread(
    sprintf('//storage6/bases/DADOS/RESTRITO/RAIS/csv/estab%s.csv', ano)
    , select = columns_raw
    , colClasses = "character"
    , encoding = "UTF-8"
    #, nrows = 100
  )

  # * merge -----------------------------------------------------------------
  rais_geocode[
    rais_raw,
    `:=`(
      qt_vinc_ativos = i.qt_vinc_ativos,
      nat_jur = i.nat_jur2009
    ),
    on = c("id_estab")
  ]

  6666666666666 MUDAR CODEMUN OU CODEMUNI -> MUDAR NO 00_3 TAMBEM
  # 2) filtrar somente empresas com vinculos ativos
  rais_filtro <- rais_geocode[as.numeric(qt_vinc_ativos) > 0]

  # filtrar 184 ucas
  ## adicionar coluna codigo concentracao urbana
  rais_filtro[
    df_codes,
    `:=`(
      code_urban_concentration = i.code_urban_concentration,
      name_uca_case = i.name_uca_case,
      nucleo = i.nucleo,
      isolated_muni = i.isolated_muni
    ),
    on = c("codemun" = "codemun")
  ]
  ## filtrar 184 ucas
  data.table::setkey(rais_filtro, code_urban_concentration)
  rais_filtro <- rais_filtro[.(unique(df_prep$code_urban_concentration))]

  #rais_geocode <- rais_geocode[codemun %in% substr(df_codes$code_muni_uca, 1, 6)]

  # 3) Filtro 2: deletar todas as intituicoes com Natureza Juridica 'publica'
  # (ver ../data-raw/rais/ManualRAIS2018.pdf) pagina 19
  # todos que comecam com o numeral 1 sao de administracao publica

  # 3.1) Identificar natureza de adm publica
  rais_filtro[, adm_pub := data.table::fifelse(substr(nat_jur, 1, 1)==1, 1, 0)]

  # 3.2) Filtrar apenas adm publica
  rais_filtro <- rais_filtro[ adm_pub != 1 ]

  # quantos vinculos de natureza juridica publica a gente perde?
  # nrow(rais_filtro) / nrow(rais_filtro)

  # 3.3) Deletar todas as empresas publicas (a entidade 2011 eh empresa publica)
  rais_filtro <- rais_filtro[nat_jur != "2011"]

  #message("Total number of private active estabs: ", unique(rais_filtro$id_estab) %>% length())

  # todo os estabs para 14 characetrs
  rais_filtro[, id_estab := str_pad(id_estab, width = 14, pad = 0)]

  # garantir que os cnpjs sao unicos
  rais_filtro <- rais_filtro %>% distinct(id_estab, .keep_all = TRUE)

  # 5) Selecionar colunas e salvar
  rais_filtro %>%
    # fix uf and codemun
    select(id_estab, qt_vinc_ativos, logradouro, bairro, codemun, name_muni, uf, cep,
           lon, lat,
           Addr_type, Score, Status, matched_address, type_year_input) %>%
    # save it
    #fwrite(sprintf("../../data/urbanformbr/rais/%s/rais_%s_filter_geocoded.csv", ano, ano))
    saveRDS(
      file = sprintf("../../data/urbanformbr/rais/%s/rais_%s_filter_geocoded.rds", ano, ano),
      compress = 'xz'
    )

}

f_rais_clean_estabs_raw(ano = ano)

# filter good geocode -----------------------------------------------------


f_filter_good_geocode <- function(ano){

  rais_filtro <- readr::read_rds(sprintf("../../data/urbanformbr/rais/%s/rais_%s_filter_geocoded.rds", ano, ano))

  rais_filtro_good_geo <- rais_filtro

  data.table::setkey(rais_filtro_good_geo, Addr_type)
  rais_filtro_good_geo <- rais_filtro_good_geo[
    .(c('PointAddress', "StreetAddress", "StreetAddressExt", "StreetName",
        'street_number', 'route', 'airport', 'amusement_park', 'intersection',
        'premise','town_square'))
  ]

  data.table::setkey(rais_filtro_good_geo, code_urban_concentration)
  rais_filtro_good_geo <- rais_filtro_good_geo[.(unique(df_prep$code_urban_concentration))]

  saveRDS(
    object = rais_filtro_good_geo,
    file = sprintf("../../data/urbanformbr/rais/%s/rais_%s_geocoded_streepmap_gquality.rds", ano, ano),
    compress = 'xz'
  )


}

f_filter_good_geocode(ano = ano)


# check and plot proportion -----------------------------------------------

rais_filtro <- readr::read_rds(sprintf("../../data/urbanformbr/rais/%s/rais_%s_filter_geocoded.rds", ano, ano))
rais_filtro_good_geo <- readr::read_rds(sprintf("../../data/urbanformbr/rais/%s/rais_%s_geocoded_streepmap_gquality.rds", ano, ano))


rais_filtro[,sum(as.integer(qt_vinc_ativos))]
rais_filtro_good_geo[,sum(as.integer(qt_vinc_ativos))]
#prop
rais_filtro_good_geo[,sum(as.integer(qt_vinc_ativos))]/rais_filtro[,sum(as.integer(qt_vinc_ativos))]


vinc <- data.table::merge.data.table(
  rais_filtro[
    ,
    .(vinc_tot = sum(as.integer(qt_vinc_ativos))),
    keyby = .(name_uca_case,code_urban_concentration)
  ],
  rais_filtro_good_geo[
    ,
    .(vinc_good = sum(as.integer(qt_vinc_ativos))),
    keyby = .(name_uca_case,code_urban_concentration)
  ]
)
vinc[,good_percent := vinc_good/vinc_tot]

vinc <- data.table::merge.data.table(
  vinc,
  df_codes[,.(code_urban_concentration,isolated_muni,nucleo)],
  by = "code_urban_concentration"
  )
vinc <- data.table::merge.data.table(
  vinc,
  df_pop_censo,
  by = "code_urban_concentration"
)

vinc <- data.table::merge.data.table(
  vinc,
  df_classify_uca_large,
  by = "code_urban_concentration"
)









# * rais raw --------------------------------------------------------------
columns_raw <- c("id_estab", "qt_vinc_ativos", 'nat_jur')
columns_raw <-

  rais_raw <- fread(
    sprintf('//storage6/bases/DADOS/RESTRITO/RAIS/csv/estab%s.csv', ano)
    , nrows = 100
    , select = c("id_estab", "qt_vinc_ativos", 'nat_jur2018')
    ,
    colClasses = "character")
