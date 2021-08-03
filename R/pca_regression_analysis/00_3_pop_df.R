rm(list=ls())
source('R/setup.R')


### projection 2015 ----
sidrar::info_sidra(x = 6579)

pop2015 <- sidrar::get_sidra(x = 6579
                             , variable = 9324
                             , period = as.character("2015")
                             , geo = "City"
)


# fix names

data.table::setDT(pop2015)
names(pop2015) <- janitor::make_clean_names(names(pop2015))

# add 2015 projection on census 1970 to 2010 data------

pop <- readr::read_rds("../../data/urbanformbr/population/population_muni_ibge.rds")

pop_muni <- list(pop[ano == 1975],pop2015) %>%
  data.table::rbindlist(use.names = TRUE,fill = TRUE)

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


# calcula a populacao por amc
pop_muni_df <- data.table::copy(pop_muni)[, lapply(.SD,sum,na.rm=TRUE),
                                          by = .(code_urban_concentration,
                                                 ano),
                                          .SDcols = "valor"]

pop_muni_df <- pop_muni_df[!is.na(valor) & valor != 0]

# setnames
names(pop_muni_df) <- c("code_urban_concentration",
                        "ano",
                        "pop")


## save
readr::write_rds(pop_muni_df,"../../data/urbanformbr/pca_regression_df/1970-2015_pop.rds",
                 compress = "gz")
