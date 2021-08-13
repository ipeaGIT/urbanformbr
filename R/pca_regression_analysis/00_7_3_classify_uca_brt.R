# description -------------------------------------------------------------

# this script classifies each uca (dummy) based on the existence of transport
# systems in any of its municipalities up until 2010

# types of transport systems included

# setup -------------------------------------------------------------------

source('R/setup.R')


# read data ---------------------------------------------------------------


# * uca munis code --------------------------------------------------------

# read df with code and names of urban concentration, and codes of each muni
#..belonging to them
df_codes <- readr::read_rds("//storage6/usuarios/Proj_acess_oport/data/urbanformbr/pca_regression_df/pca_regression_df.rds")

# add column with muni name without

df <- df_codes %>%
  tidyr::unnest_longer(code_muni_uca) %>%
  data.table::setDT()

f_nome_muni <- function(x){
  df <- geobr::lookup_muni(code_muni = x)
  data.table::setDT(df)[, name_muni]
}

# ABAIXO REQUER MULTIPLAS TENTATIVAS
n1 <- map_chr(df$code_muni_uca[1:150], f_nome_muni)
n2 <- map_chr(df$code_muni_uca[151:300], f_nome_muni)
n3 <- map_chr(df$code_muni_uca[301:450], f_nome_muni)
n4 <- map_chr(df$code_muni_uca[451:600], f_nome_muni)
n5 <- map_chr(df$code_muni_uca[601:638], f_nome_muni)

df[, name_muni_uca := c(n1,n2,n3,n4,n5) ]


# * rede tma --------------------------------------------------------------
rede_tma <- readr::read_rds("../../data/urbanformbr/rede_tma.rds")

#df_estacoes <- sf::st_drop_geometry(rede_tma$estacoes)
df_corredores <-  sf::st_drop_geometry(rede_tma$corredores)

# clean data --------------------------------------------------------------

# filter data
df_corredores <- df_corredores %>%
  dplyr::filter(
    !(situacao == "Em construção" | situacao == "Planejado" | situacao == "Cancelado")
    ) %>%
  dplyr::filter(!(modo == "Barca" | modo == "VLT")) %>%
  dplyr::filter(!(ano == 1 | ano == 999 | ano > 2010))

cidades <- df_corredores[order(cidade), .N, by=cidade][,N:=1]
data.table::setnames(
  x = cidades,
  old = c("cidade","N"),
  new = c("name_muni_uca","tma")
  )

#cidades_tma <- df_corredores[,logical(1),keyby=cidade]$cidade

# merge data --------------------------------------------------------------
df_tma <- dplyr::left_join(
  df,
  cidades,
  by = "name_muni_uca"
)

df_tma[, tma := replace_na(tma, 0)]

df_tma <- df_tma %>%
  dplyr::group_by(code_urban_concentration) %>%
  dplyr::summarise(tma = sum(stma))

# save data ---------------------------------------------------------------

saveRDS(
  object = df_tma,
  file = '../../data/urbanformbr/pca_regression_df/classify_uca_tma.rds',
  compress = 'xz'
)



