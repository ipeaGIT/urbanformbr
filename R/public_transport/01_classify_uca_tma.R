# description -------------------------------------------------------------

# this script classifies each uca (dummy) based on the existence of transport
# systems in any of its municipalities up until 2010

# types of transport systems included

# setup -------------------------------------------------------------------

source('R/fun_support/setup.R')


# read data ---------------------------------------------------------------
df_shapes <- readr::read_rds("../../data/urbanformbr/urban_area_shapes/urban_area_pop_100000_dissolved.rds") %>%
  sf::st_drop_geometry() %>%
  select(-pop_ibge_total_2010)


df_uca <- geobr::read_urban_concentrations(simplified = T)

df_uca <- df_uca %>%
  sf::st_drop_geometry() %>%
  dplyr::select(code_urban_concentration:name_muni)

df_uca <- subset(df_uca, code_urban_concentration %in% df_shapes$code_urban_concentration)

data.table::setDT(df_uca)


# add column with muni name without any special character or UF
#(ex: "Belo Horizonte/MG").
# this is done to merge df later

# df <- df_codes %>%
#   tidyr::unnest_longer(code_muni_uca) %>%
#   data.table::setDT()
#
# f_nome_muni <- function(x){
#   df <- geobr::lookup_muni(code_muni = x)
#   data.table::setDT(df)[, name_muni]
# }
#
# # ABAIXO REQUER MULTIPLAS TENTATIVAS
# n1 <- map_chr(df_uca$code_muni[1:150], f_nome_muni)
# n2 <- map_chr(df_uca$code_muni[151:300], f_nome_muni)
# n3 <- map_chr(df_uca$code_muni[301:450], f_nome_muni)
# n4 <- map_chr(df_uca$code_muni[451:600], f_nome_muni)
# n5 <- map_chr(df_uca$code_muni[601:635], f_nome_muni)
#
# df_uca[, name_muni_merge := c(n1,n2,n3,n4,n5) ]


# * rede tma --------------------------------------------------------------
rede_tma <- readr::read_rds("../../data/urbanformbr/rede_transp_tma/rede_tma.rds")

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

cidades <- df_corredores[order(cidade), .N, by=cidade][, N:=1]

data.table::setnames(
  x = cidades,
  old = c("cidade","N"),
  new = c("name_muni_uca","tma")
  )

#cidades_tma <- df_corredores[,logical(1),keyby=cidade]$cidade

# merge data --------------------------------------------------------------
df_tma <- dplyr::left_join(df_uca, cidades, by = c("name_muni" = "name_muni_uca"))

df_tma[, tma := replace_na(tma, 0)]

df_tma <- df_tma %>%
  dplyr::group_by(code_urban_concentration) %>%
  dplyr::summarise(tma = as.integer(sum(tma)))

# save data ---------------------------------------------------------------

data.table::fwrite(
  x = df_tma
  , file = "../../data/urbanformbr/consolidated_data/classify_tma_public_transport.csv"
  , append = F
)

