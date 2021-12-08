
# Import packages ----

rm(list=ls())
library(ggplot2)
library(magrittr)
library(data.table)
library(Hmisc)


# Read data -----


dt <- data.table::fread("../../data-raw/frota_denatran/I_Frota_por_UF_Municipio_Marca_e_Modelo_Ano_Dezembro_2020.txt"
                        ,encoding = "Latin-1")

names(dt) <- c("UF","muni","marca_modelo","ano_fabr","qtd")

tmp_dt <- data.table::copy(dt) %>%
  .[,ano_fabr :=  gsub("[^0-9.-]", "",ano_fabr)] %>%
  .[ano_fabr != "",] %>%
  .[,ano_fabr := as.numeric(ano_fabr)]


class_veh <- readr::read_rds("../../data/urbanformbr/denatran/list_cars_motos.rds")
class_veh <- list(
  data.table::data.table("class" = "moto", "model" = class_veh[[1]])
  ,data.table::data.table("class" = "carro", "model" = class_veh[[2]])) %>%
  data.table::rbindlist()


geobr_muni <- geobr::read_municipality()

geobr_states <- geobr::read_state()

geobr_urban_area <- geobr::read_urban_concentrations()


# Prepare data -----


# * sum fleet, by class ----

tmp_dt1 <- data.table::copy(tmp_dt)

tmp_dt1[class_veh, on = c("marca_modelo" = "model"), classe := i.class]

tmp_dt2 <-  data.table::copy(tmp_dt1)  %>%
  .[!is.na(classe),] %>%
  .[,sum(qtd,na.rm = TRUE),by = .(UF,muni,ano_fabr,classe)] %>%
  .[order(ano_fabr,decreasing = FALSE),] %>%
  .[UF != "Sem InformaÃ§Ã£o",]

# * add state into fleet ----

tmp_state <- geobr_states %>%
  data.table::copy() %>%
  data.table::setDT() %>%
  .[,name_state  := toupper(name_state)] %>%
  .[,name_state := stringi::stri_trans_general(str = name_state,
                                               id = "Latin-ASCII")]

tmp_dt2[tmp_state,on = c("UF" = "name_state"),abbrev_state := i.abbrev_state]

# * rename 'name_muni' -----


tmp_muni <- geobr_muni %>%
  data.table::copy() %>%
  data.table::setDT() %>%
  .[,name_muni := toupper(name_muni)] %>%
  .[,name_muni := stringi::stri_trans_general(str = name_muni,
                                              id = "Latin-ASCII")]

# * fix city mispelling ----

tmp_dt2[muni == "SANTANA DO LIVRAMENTO"       , muni := "SANT'ANA DO LIVRAMENTO"]
tmp_dt2[muni == "MOGI-GUACU"                  , muni := "MOGI GUACU"]
tmp_dt2[muni == "MOGI MIRIM"                  , muni := "MOJI MIRIM"]
tmp_dt2[muni == "ENTRE IJUIS"                 , muni := "ENTRE-IJUIS"]
tmp_dt2[muni == "SAO LOURENCO D'OESTE"        , muni := "SAO LOURENCO DO OESTE"]
tmp_dt2[muni == "SAO MIGUEL D'OESTE"          , muni := "SAO MIGUEL DO OESTE"]
tmp_dt2[muni == "SANTA CRUZ DO MONTE CASTELO" , muni := "SANTA CRUZ DE MONTE CASTELO"]
tmp_dt2[muni == "PASSA VINTE"                 , muni := "PASSA-VINTE"]
tmp_dt2[muni == "BALNEARIO DE PICARRAS"       , muni := "BALNEARIO PICARRAS"]
tmp_dt2[muni == "SAO LUIZ DO PARAITINGA"      , muni := "SAO LUIS DO PARAITINGA"]
tmp_dt2[muni == "ARMACAO DE BUZIOS"           , muni := "ARMACAO DOS BUZIOS"]
tmp_dt2[muni == "BARAO D0 MONTE ALTO"         , muni := "BARAO DE MONTE ALTO"]
tmp_dt2[muni == "BALNEARIO RINCAO"            , muni := "RINCAO"]
tmp_dt2[muni == "PARATI"                      , muni := "PARATY"]
tmp_dt2[muni == "QUELUZITA"                   , muni := "QUELUZITO"]
tmp_dt2[muni == "ITAPAJE"                     , muni := "ITAPAGE"]
tmp_dt2[muni == "PINHAL DO SAO BENTO"         , muni := "PINHAL DE SAO BENTO"]
tmp_dt2[muni == "MUNHOZ DE MELLO"             , muni := "MUNHOZ DE MELO"]
tmp_dt2[muni == "SAO TOME DAS LETRAS"         , muni := "SAO THOME DAS LETRAS"]
tmp_dt2[muni == "FLORINEA"                    , muni := "FLORINIA"]
tmp_dt2[muni == "COUTO DE MAGALHAES"          , muni := "COUTO MAGALHAES"]
tmp_dt2[muni == "NOVA DO MAMORE"              , muni := "NOVA MAMORE"]
tmp_dt2[muni == "TRAJANO DE MORAIS"           , muni := "TRAJANO DE MORAES"]
tmp_dt2[muni == "AMPARO DA SERRA"             , muni := "AMPARO DO SERRA"]
tmp_dt2[muni == "DONA EUZEBIA"                , muni := "DONA EUSEBIA"]
tmp_dt2[muni == "GOUVEA"                      , muni := "GOUVEIA"]
tmp_dt2[muni == "PINGO D'AGUA"                , muni := "PINGO-D'AGUA"]
tmp_dt2[muni == "BELA VISTA DO CAROBA"        , muni := "BELA VISTA DA CAROBA"]
tmp_dt2[muni == "SAO VALERIO DA NATIVIDADE"   , muni := "SAO VALERIO"]
tmp_dt2[muni == "BELEM DE SAO FRANCISCO"      , muni := "BELEM DO SAO FRANCISCO"]
tmp_dt2[muni == "CERRO-CORA"                  , muni := "CERRO CORA"]
tmp_dt2[muni == "LAGOA DO ITAENGA"            , muni := "LAGOA DE ITAENGA"]
tmp_dt2[muni == "LAGOA DANTA"                 , muni := "LAGOA D'ANTA"]
tmp_dt2[muni == "PINDARE MIRIM"               , muni := "PINDARE-MIRIM"]
tmp_dt2[muni == "LAGEDO DO TABOCAL"           , muni := "LAJEDO DO TABOCAL"]
tmp_dt2[muni == "MUQUEM DO SAO FRANCISCO"     , muni := "MUQUEM DE SAO FRANCISCO"]
tmp_dt2[muni == "GRACCHO CARDOSO"             , muni := "GRACHO CARDOSO"]
tmp_dt2[muni == "LAGEADO GRANDE"             , muni := "LAJEADO GRANDE"]
tmp_dt2[muni == "OLHO D'AGUA DO BORGES"      , muni := "OLHO-D'AGUA DO BORGES"]
tmp_dt2[muni == "LAGEADO GRANDE"             , muni := "LAJEADO GRANDE"]



# * update city_area names ----


tmp_dt2[tmp_muni,on = c("muni" = "name_muni"
                        ,"abbrev_state" = "abbrev_state")
        ,code_muni := i.code_muni]

# > tmp_dt2[is.na(code_muni)]$muni %>% unique()

# [1] "PINTO BANDEIRA"
# [2] "OLHO D'AGUA DO BORGES"
# [3] "RINCAO"
# [4] "ASSU"
# [5] "PESCARIA BRAVA"
# [6] "BOM JESUS"
# [7] "PRESIDENTE CASTELO BRANCO"
# [8] "SERRA CAIADA"
# [9] "SAO VICENTE DO SERIDO"
# [10] "MOJUI DOS CAMPOS"
# [11] "BOA SAUDE"
# [12] "PARAISO DAS AGUAS"
# [13] "SAO DOMINGOS DE POMBAL"
# [14] "CAMPO GRANDE"

# * filter by urban concentration cities ----

tmp_uca <- geobr_urban_area %>%
  data.table::copy() %>%
  data.table::setDT()

tmp_dt2[tmp_uca,on = c("code_muni"
                       ,"abbrev_state")
        ,code_urban_concentration := i.code_urban_concentration]

tmp_dt2 <- tmp_dt2[!is.na(code_urban_concentration),]


# * sum fleet, by urban_concentration ----


tmp_dt3 <- data.table::copy(tmp_dt2) %>%
  .[ano_fabr <= 2021,] %>%
  .[,age := 2021 - ano_fabr] %>%
  .[,lapply(.SD,sum,na.rm = TRUE)
    ,by = .(age,classe,code_urban_concentration)
    ,.SDcols = "V1"]


tmp_dt4 <- data.table::copy(tmp_dt3) %>%
  .[,norm_fleet := V1/sum(V1),by = .(classe,code_urban_concentration)]


dir.create("../../data/urbanformbr/denatran")

# Save passenger, by age ----

fleet_carro <- data.table::copy(tmp_dt4) %>%
  .[classe == "carro"]

readr::write_rds(x = fleet_carro
                 ,file = "data/emissions/fleet_pc_by_age.rds")

# Save motorcycle, by age ----

fleet_mc <- data.table::copy(tmp_dt4) %>%
  .[classe == "moto"]


readr::write_rds(x = fleet_mc
                 ,file = "data/emissions/fleet_mc_by_age.rds")


