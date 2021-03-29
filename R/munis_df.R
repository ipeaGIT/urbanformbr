library(data.table)

# municipalities codes ----------------------------------------------------

munis_df <- data.table::setDT(tibble::tribble(
  ~code_muni,  ~abrev_muni, ~name_muni,        ~abrev_estado, ~modo_2017, ~modo_2018, ~modo_2019, ~modo_2020,
  2304400L,    "for",       "Fortaleza",       "CE",          "todos",    "todos",    "todos",    "todos",
  3550308L,    "spo",       "Sao Paulo",       "SP",          "todos",    "todos",    "todos",    "todos",
  3304557L,    "rio",       "Rio de Janeiro",  "RJ",          "ativo",    "todos",    "todos",    "todos",
  4106902L,    "cur",       "Curitiba",        "PR",          "todos",    "todos",    "todos",    "todos",
  4314902L,    "poa",       "Porto Alegre",    "RS",          "todos",    "todos",    "todos",    "todos",
  3106200L,    "bho",       "Belo Horizonte",  "MG",          "todos",    "todos",    "todos",    "todos",
  5300108L,    "bsb",       "Brasilia",        "DF",          "ativo",    "ativo",    "ativo",    "ativo",
  2927408L,    "sal",       "Salvador",        "BA",          "ativo",    "ativo",    "ativo",    "ativo",
  1302603L,    "man",       "Manaus",          "AM",          "ativo",    "ativo",    "ativo",    "ativo",
  2611606L,    "rec",       "Recife",          "PE",          "ativo",    "ativo",    "todos",    "todos",
  5208707L,    "goi",       "Goiania",         "GO",          "ativo",    "ativo",    "todos",    "ativo",
  1501402L,    "bel",       "Belem",           "PA",          "ativo",    "ativo",    "ativo",    "ativo",
  3518800L,    "gua",       "Guarulhos",       "SP",          "ativo",    "ativo",    "ativo",    "ativo",
  3509502L,    "cam",       "Campinas",        "SP",          "todos",    "todos",    "todos",    "ativo",
  2111300L,    "slz",       "Sao Luis",        "MA",          "ativo",    "ativo",    "ativo",    "ativo",
  3304904L,    "sgo",       "Sao Goncalo",     "RJ",          "ativo",    "ativo",    "ativo",    "ativo",
  2704302L,    "mac",       "Maceio",          "AL",          "ativo",    "ativo",    "ativo",    "ativo",
  3301702L,    "duq",       "Duque de Caxias", "RJ",          "ativo",    "ativo",    "ativo",    "ativo",
  5002704L,    "cgr",       "Campo Grande",    "MS",          "ativo",    "ativo",    "ativo",    "ativo",
  2408102L,    "nat",       "Natal",           "RN",          "ativo",    "ativo",    "ativo",    "ativo"
))
