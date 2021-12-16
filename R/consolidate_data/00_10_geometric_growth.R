rm(list=ls())
source('R/setup.R')


### projection 2015 ----
sidrar::info_sidra(x = 6579)

pop2015 <- sidrar::get_sidra(x = 6579
                             , variable = 9324
                             , period = as.character("2015")
                             , geo = "City")


# fix names

data.table::setDT(pop2015)
names(pop2015) <- janitor::make_clean_names(names(pop2015))

# census 2000---

pop2000 <- readr::read_rds("../../data/urbanformbr/population/population_muni_ibge.rds")
pop2000 <- pop2000[ano == 2000,]

# cbind 2000 with 2015


merge_pop <- data.table::merge.data.table(x = pop2000
                                          , y = pop2015
                                          , by = "municipio_codigo")
merge_pop <- merge_pop[,.(municipio_codigo,valor.x,valor.y)]
data.table::setnames(merge_pop
                     , old = c("valor.x","valor.y")
                     , new = c("pop2000","pop2015"))
merge_pop[,municipio_codigo := as.character(municipio_codigo)]

# agregate by urban concentration

tmp_urb <- geobr::read_urban_concentrations()
tmp_urb <- data.table::as.data.table(tmp_urb)
tmp_urb <- tmp_urb[,.(code_urban_concentration,code_muni)]
tmp_urb[,code_muni := as.character(code_muni)]

merge_pop[tmp_urb,on= c("municipio_codigo" = "code_muni"),
          code_urban_concentration := i.code_urban_concentration]

# sum population by code_urban_concentration

merge_urban <- data.table::copy(merge_pop)[
  !is.na(code_urban_concentration), # remove NA
  lapply(.SD,sum)
  ,by = .(code_urban_concentration)
  ,.SDcols = c("pop2000","pop2015")]

# geometric growth rate

merge_urban[,r15_00 := 100 * ((pop2015 / pop2000 )^(1/(2015-2000)) - 1)]

## save
readr::write_rds(merge_urban,"../../data/urbanformbr/pca_regression_df/pop_growth.rds",
                 compress = "gz")
