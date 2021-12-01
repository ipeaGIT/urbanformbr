rm(list=ls())
source('R/setup.R')


### projection pop1970 ----
# sidrar::info_sidra(x = 202)
#
# pop1970 <-  sidrar::get_sidra(x = 202
#                               , variable = 93
#                               , period = "1970"
#                               , geo = "City"
#                               , classific = c("c1"))
# data.table::setDT(pop1970)
# pop1970 <- pop1970[Sexo == "Total" & `Situação do domicílio` == "Total"]
#
# ### census pop 2010 ----
# sidrar::info_sidra(x = 1702)
#
# pop2010 <- sidrar::get_sidra(x = 1702
#                              , variable = 134
#                              , classific  = c('c455')
#                              , category = list(0)
#                              , period = as.character("2010")
#                              , geo = "City"
# )
# data.table::setDT(pop2010)
# pop2010
# sum(pop2010$Valor)
# #> 190072903
#
#
# # fix names
#
# names(pop2010) <- janitor::make_clean_names(names(pop2010))

# add 2015 projection on census 1970 to 2010 data------

pop <- readr::read_rds("../../data/urbanformbr/population/population_muni_ibge.rds")

pop_muni <- pop[ano %in% c(1970,2010)]

pop_muni[,c("situacao_do_domicilio",
            "situacao_do_domicilio_codigo",
            "sexo_codigo","sexo") := NULL]


# read amc---------
amc_muni <- readr::read_rds("../../data/urbanformbr/population/comparable_areas_ibge.rds")


# merge amc data to muni_data
pop_muni[amc_muni,on = c("municipio_codigo" = "code_muni_2010"),
         code_amc := i.code_amc]



# add urban concentration
uca <- geobr::read_urban_concentrations(simplified = F) %>% setDT()
uca[, code_muni := as.character(code_muni)]

pop_muni[uca,on = c("municipio_codigo" = "code_muni"),
         code_urban_concentration := i.code_urban_concentration]

# remove cities not related to urban concentration
pop_muni <- pop_muni[!is.na(code_urban_concentration)]


pop_muni[code_urban_concentration == 1100122]
pop_muni[code_amc == 1001]
#
amc_plot <- amc_muni[code_amc == 1001] %>% sf::st_as_sf() %>%
  sf::st_transform(4326)
urb_plot <- uca[code_urban_concentration %in% c(1100122,1100205)] %>%
  sf::st_as_sf() %>%
  sf::st_transform(4326)

dev.off()
plot(amc_plot$geom)
plot(urb_plot$geom,lty = 3,add = TRUE,type = "p",col = "red")



pop_muni[,uniqueN(code_urban_concentration),by = code_amc][V1 > 1]
# calcula a populacao por amc
pop_muni_df <- data.table::copy(pop_muni)[, lapply(.SD,sum,na.rm=TRUE),
                                          by = .(code_urban_concentration,
                                                 #code_amc,
                                                 ano),
                                          .SDcols = "valor"]
pop_muni_df[code_urban_concentration == 1100122,]


pop_muni_df <- pop_muni_df[!is.na(valor) & valor != 0]

pop_muni_df <- data.table::dcast(data = pop_muni_df
                                 , formula =  code_urban_concentration ~ ano
                                 , drop = FALSE)

pop_muni_df[!is.na(`1970`),]

# long to wide




## save
readr::write_rds(pop_muni_df,"../../data/urbanformbr/pca_regression_df/1970-2015_pop.rds",
                 compress = "gz")
