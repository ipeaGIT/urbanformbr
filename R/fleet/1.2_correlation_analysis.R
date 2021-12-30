
rm(list=ls())
library(ggplot2)
library(magrittr)
library(data.table)
library(Hmisc)




dt2020 <- data.table::fread("../../data-raw/frota_denatran/I_Frota_por_UF_Municipio_Marca_e_Modelo_Ano_Fevereiro_2020.txt"
                        ,encoding = "UTF-8")

names(dt2020) <- janitor::make_clean_names(names(dt2020))

sum2020 <- dt2020[,sum(qtd_veiculos),by = .(uf,municipio)]
sum2020




readxl::excel_sheets("../../data-raw/frota_denatran/Frota Munic Abr2010.xls")
dt2010 <- readxl::read_xls("../../data-raw/frota_denatran/Frota Munic Abr2010.xls"
                           ,sheet = "ABR_2010",skip = 2) %>%
  data.table::as.data.table()

names(dt2010) <- janitor::make_clean_names(names(dt2010))

sum2010 <- dt2010[,sum(total),by = .(uf,municipio)]


geobr_st <- geobr::read_state() %>%
  data.table::as.data.table()
geobr_st <- geobr_st[,name_state := stringi::stri_trans_general(str = name_state,
                                                       id = "Latin-ASCII")]
geobr_st <- geobr_st[,name_state := toupper(name_state)]
geobr_st[1]

sum2010[geobr_st, on = c("uf"="abbrev_state"),uf := i.name_state]
sum2010[sum2020, on = c("uf","municipio"),V1_2020 := i.V1]

cor.test(sum2010$V1,sum2010$V1_2020)
