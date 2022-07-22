
# description -------------------------------------------------------------


# setup -------------------------------------------------------------------
source("R/fun_support/setup.R")


# setup parallel ----------------------------------------------------------

#future::plan(future::multicore, workers = future::availableCores() / 2)

# read data ---------------------------------------------------------------

# * uca and municipalities -------------------------------------------
df_uca <- readr::read_rds("../../data/urbanformbr/urban_area_shapes/urban_area_pop_100000_dissolved.rds")

df_uca_muni <- geobr::read_urban_concentrations(simplified = F)

# * RAIS data -------------------------------------------------------------

# * * geocoded RAIS -------------------------------------------------------

# abrir rais para ver quais colunas selecionar
# rais geocoded
ex_rais_geocoded <- data.table::fread(
  "../../data/geocode/rais/2010/rais_2010_raw_geocoded.csv"
  , nrows = 100
  , colClasses = "character"
  , encoding = "UTF-8"
)

cols_rais_geocoded <- c(
  "id_estab","name_muni","codemun","cep","logradouro","lon","lat"
)

# rais with selected columns
df_rais_geocoded <- data.table::fread(
  "../../data/geocode/rais/2010/rais_2010_raw_geocoded.csv"
  #, nrows = 100
  , colClasses = "character"
  , encoding = "UTF-8"
  , select = cols_rais_geocoded
)

# make sure id estab has 14 characters
df_rais_geocoded[, id_estab := str_pad(id_estab, width = 14, pad = 0)]

# remove duplicates
df_rais_geocoded <- df_rais_geocoded %>%
  dplyr::distinct(id_estab, logradouro, .keep_all = T)

# * * worker data -----------------------------------------------------
ex_rais_worker <- data.table::fread(
    "//storage6/bases/DADOS/RESTRITO/RAIS/csv/brasil2010.csv"
    , nrows = 100
    , colClasses = "character"
    , encoding = "UTF-8"
  )

cols_rais_worker <- c(
  "id_estab"
  #,"qt_vinc_ativos"
  ,"nat_jur2009", "emp_31dez","clas_cnae20", "codemun"
  )

df_rais_worker <- data.table::fread(
  "//storage6/bases/DADOS/RESTRITO/RAIS/csv/brasil2010.csv"
  #, nrows = 100
  , colClasses = "character"
  , encoding = "UTF-8"
  , select = cols_rais_worker
)

# make sure id estab has 14 characters
df_rais_worker[, id_estab := str_pad(id_estab, width = 14, pad = 0)]

# * * firm data -----------------------------------------------------
ex_rais_firm <- data.table::fread(
  "//storage6/bases/DADOS/RESTRITO/RAIS/csv/estab2010.csv"
  , nrows = 100
  , colClasses = "character"
  , encoding = "UTF-8"
)

cols_rais_firm <- c(
  "id_estab", "clas_cnae20","codemun","cep","logradouro"
)

df_rais_firm <- data.table::fread(
  "//storage6/bases/DADOS/RESTRITO/RAIS/csv/estab2010.csv"
  #, nrows = 100
  , colClasses = "character"
  , encoding = "UTF-8"
  , select = cols_rais_firm
)

# make sure id estab has 14 characters
df_rais_firm[, id_estab := str_pad(id_estab, width = 14, pad = 0)]

# remove duplicates
df_rais_firm <- df_rais_firm %>%
  dplyr::distinct(id_estab, logradouro, .keep_all = T)

# subset munis ------------------------------------------------------------

# * ucas ------------------------------------------------------------------

# uca data
df_uca_muni <- subset(df_uca_muni, code_urban_concentration %in% df_uca$code_urban_concentration)

# * RAIS ------------------------------------------------------------------

# code_muni with 6 characters
code_munis <- substring(df_uca_muni$code_muni, first = 1, last = 6)

# * * geocoded ------------------------------------------------------------

# filter munis from ucas analysed (182 ucas & 635 munis)
# data.table::setkey(df_rais_geocoded, codemun)
# a <- df_rais_geocoded[.(code_munis)]
df_rais_geocoded <- subset(df_rais_geocoded, codemun %in% code_munis)


# * * firm ----------------------------------------------------------------

df_rais_firm <- subset(df_rais_firm, codemun %in% code_munis)


# * * worker --------------------------------------------------------------

# rename nat_jur
data.table::setnames(df_rais_worker, old = "nat_jur2009", new = "nat_jur")

# filter munis from ucas analysed (182 ucas & 635 munis)
df_rais_worker <- subset(df_rais_worker, codemun %in% code_munis)

# * filter RAIS data ------------------------------------------------------

# from acesso_oport/R/fun/empregos/empregos.R
# 1. vinculos ativos
# 2. remover natureza juridica "publica"
# 3. remover adm publica


# * * vinculos ativos -----------------------------------------------------
# selecionar somente vinculos ativos
df_rais_worker <- df_rais_worker[emp_31dez == 1]

# * * remove nat jur "publica" --------------------------------------------
# nat jur começando com 1 -> publica

# identify pub adm: nat jur starting with 1
df_rais_worker[
  ,
  adm_pub := data.table::fifelse( test = substr(nat_jur, 1, 1) == 1, yes = 1, no = 0 )
]

# filter only those NOT adm pub
df_rais_worker <- df_rais_worker[adm_pub != 1]

# * * remove adm publica --------------------------------------------------
# empresas publicas: nat_jur == 2011
df_rais_worker <- df_rais_worker[nat_jur != "2011"]

df_rais_worker[, adm_pub := NULL]

# * * workers per firm ----------------------------------------------------

#  intersect(colnames(ex_rais_firm), colnames(ex_rais_worker)) %>% sort()
#  intersect(colnames(ex_rais_firm), colnames(ex_rais_geocoded)) %>% sort()

df_rais_wide <- df_rais_worker[
  ,
  .(tot_vinc = .N),
  by = .(id_estab, clas_cnae20, codemun)
]

df_rais_wide <- df_rais_wide %>%
  dplyr::arrange(id_estab, clas_cnae20, codemun, tot_vinc)

# find duplicates
df_rais_wide %>%
  dplyr::group_by(id_estab, clas_cnae20, codemun) %>%
  dplyr::mutate(
    num_obs = n(),
    dup_id = dplyr::row_number()
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(is_duplicated = dup_id > 1) %>%
  dplyr::filter(is_duplicated == T)

data.table::setDT(df_rais_wide)

# check number of duplicates
# df_rais_wide[is_duplicated==T] %>% nrow()

# no duplicates. remove columns created
# df_rais_wide[, num_obs := NULL]
# df_rais_wide[, dup_id := NULL]
# df_rais_wide[, is_duplicated := NULL]

# * * clean outliers ------------------------------------------------------

# see https://github.com/ipeaGIT/acesso_oport/blob/master/R/fun/empregos/empregos.R
# line 165

# create cnae sector and subsector
df_rais_wide[
  ,
  `:=`(
    cnae_setor = substr(clas_cnae20, 1, 2),
    cnae_subsetor = substr(clas_cnae20, 1, 3)
  )
]

# problematic sectors:
cnaes_problema <- c("35", "36", "38", "41", "42", "43", "49", "51", "64", "78", "80", "81", "82", "84")


df_rais_wide[
  ,
  cnae_problema := data.table::fifelse(
    test = cnae_setor %in% cnaes_problema, yes = cnae_setor,
    no = ifelse(cnae_subsetor == '562', yes = '562', no = '')
    )
  ]

# Outlier: estabelecimentos de setores problemáticos cujo total de
# vínculos fique acima do percentil 95 da distribuição de vínculos daquele setor
df_rais_wide[
  ,
  p95_setor := round(quantile(tot_vinc, probs = 0.95), 0), by = .(cnae_problema)
  ]

# Cria coluna total_corrigido com total de vínculos corrigidos
# Caso o estabelecimento de um setor problemático tenha mais vínculos que o p95
# daquele setor, o total corrigido será igual ao p95 do setor. Caso não seja de
# um setor problemático, e caso não tenha mais vínculos que o p95, o total
# permanece igual
df_rais_wide[, total_corrigido := tot_vinc]

df_rais_wide[
  cnae_problema != "",
  total_corrigido := data.table::fifelse(
    test = total_corrigido >= p95_setor, yes = p95_setor, no = total_corrigido)
]

df_rais_wide[, p95_setor := NULL]


# merge rais dfs ----------------------------------------------------------

# subset id_estabs at rais_firm and rais_geocode using df_rais_wide
df_rais_firm <- subset(df_rais_firm, id_estab %in% df_rais_wide$id_estab)
df_rais_geocoded <- subset(df_rais_geocoded, id_estab %in% df_rais_wide$id_estab)

# add number of workers (from rais wide) to firm dataset
df_rais_firm[
  df_rais_wide,
  on = c("id_estab", "clas_cnae20", "codemun"),
  `:=`(
    total_corrigido = i.total_corrigido
  )
]

# make sure remove any is.na
df_rais_firm <- df_rais_firm[complete.cases(df_rais_firm)]


# join number of workers data (firm_join) to geocoded dataset
df_merge <- df_rais_geocoded

df_merge[
  df_rais_firm,
  on = c("id_estab", "logradouro"),
  `:=`(
    total_corrigido = i.total_corrigido
  )
]

# remove any is.na
df_merge <- df_merge[complete.cases(df_merge)]

# check if any duplicates in df_merge
df_merge %>%
  dplyr::group_by(id_estab, logradouro) %>%
  dplyr::mutate(
    num_obs = n(),
    dup_id = dplyr::row_number()
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(is_duplicated = dup_id > 1) %>%
  dplyr::filter(is_duplicated == T)

# distinct based on two columns
df_merge <- df_merge %>%
  dplyr::distinct(id_estab, logradouro, .keep_all = T)

# save data ---------------------------------------------------------------
saveRDS(
  object = df_merge,
  file = '../../data/urbanformbr/rais/2010/rais_2010_geocoded_sum_of_workers',
  compress = 'xz'
)

