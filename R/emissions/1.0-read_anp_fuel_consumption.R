# 1) load packages--------
rm(list=ls())
library(magrittr)
library(data.table)
library(geobr)

# 2) read files-----------

uca <- geobr::read_urban_concentrations(simplified = TRUE) %>%
  data.table::setDT() %>%
  .[,code_muni := as.character(code_muni)]

br <- geobr::read_municipality() %>%
  data.table::setDT() %>%
  .[,code_muni := as.character(code_muni)]

df_etanol_raw <- data.table::fread("../../data-raw/ANP/vendas-anuais-de-etanol-hidratado-por-municipio.csv",encoding = "UTF-8")
df_gasolina_raw <- data.table::fread("../../data-raw/ANP/vendas-anuais-de-gasolina-c-por-municipio.csv",encoding = "UTF-8")

# organize ANP data

names(df_etanol_raw) <- janitor::make_clean_names(names(df_etanol_raw))
names(df_gasolina_raw) <- janitor::make_clean_names(names(df_gasolina_raw))
df_etanol_raw[1]
df_gasolina_raw[1]
anp <- rbind(df_gasolina_raw,df_etanol_raw)

# 3) processing -------

anp2 <- data.table::dcast(data = anp[ano == 2010]
                          , formula =  codigo_ibge + municipio ~ produto
                          , value.var = "vendas")
names(anp2) <- janitor::make_clean_names(names(anp2))

# fix columns
anp2[,codigo_ibge := as.character(codigo_ibge)]

anp2[,gasolina_c := as.numeric(gasolina_c)]
anp2[,etanol := as.numeric(etanol)]

anp2[is.na(etanol), etanol := 0]
anp2[is.na(gasolina_c), gasolina_c := 0]

anp2[,gasolina_c := units::set_units(gasolina_c,"liters") %>% units::set_units("m^3")]
anp2[,etanol := units::set_units(etanol,"liters") %>% units::set_units("m^3")]


# add uca info
anp2[uca, on = c("codigo_ibge" = "code_muni")
     , code_urban_concentration := i.code_urban_concentration]

anp2 <- anp2[!is.na(code_urban_concentration),]

# sum values
varCols <- c("etanol","gasolina_c")
anp3 <- data.table::copy(anp2)[,
                               lapply(.SD,sum,na.rm = TRUE)
                               , .SDcols = varCols
                               , by = code_urban_concentration]


anp3[uca, on = "code_urban_concentration"
     , name_urban_concentration := i.name_urban_concentration]

anp3[,ratio_gasoline := gasolina_c / (gasolina_c + etanol)]
anp3[,ratio_gasoline := as.numeric(ratio_gasoline)]


# save ----

readr::write_rds(anp3,"data/fuel_urban_areas.rds")
