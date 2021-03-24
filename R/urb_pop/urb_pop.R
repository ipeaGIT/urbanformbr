#
# download sidra R
#

library(ggrepel)
source('R/colours.R')
source("R/setup.R")
source("R/urb_pop/colors_plot.R")
source("R/style.R")
source("R/urb_pop/aop_style1.R")

`%nin%` = Negate(`%in%`)
`%nlike%` <- Negate(`%like%`)
library(ggplot2)
library(devtools)
#devtools::install_github("rpradosiqueira/sidrar")
library("sidrar")
library(magrittr)
library(mapview)
library(data.table)
#
#
# Tabela 6579 - População residente estimada
# Tabela 202 - População residente, por sexo e situação do domicílio
#

info2020 <- sidrar::info_sidra(x = 202)



#
# fig1
#
pop202 <- sidrar::get_sidra(x = 202
                            , variable = 1000093
                            , period = as.character(c(1970, 1980, 1991, 2000, 2010))
                            #, geo = "Brazil"
                            #, geo.filter = "City"

)

data.table::setDT(pop202)
names(pop202) <- janitor::make_clean_names(names(pop202))

pop202[1]

ggplot() +
  geom_bar(data = pop202[situacao_do_domicilio != "Total" & sexo == "Total"],
           stat = "identity",
           aes(x = ano, y = valor,fill = situacao_do_domicilio))+
  scale_fill_manual(values = c(
    as.vector(aop_colors$qualitativas[1])
    , as.vector(aop_colors$cinzas[3])
  )) +
  labs(x = NULL,y = "Taxa de crescimento (%)",
       #title = "Taxa de motorização",
       subtitle = "Taxa de crescimento conforme situação do domicílio e faixa populacional",
       color="Tamanho do \nmunicípio",fill = NULL) +
  aop_style1() +
  theme(legend.position = "right",
        strip.placement = "inside",
        legend.text = element_text(size = 10, colour = "#808080"),
        legend.title = element_text(size = 8, colour = "#575757"),
        axis.text.x = element_text(angle = 0, vjust = +1.55, hjust=1)) +
  guides(fill = guide_legend(override.aes = list(shape = NA)))

ggsave("figures/urb_pop/urb_pop_oveview.png",scale=1,
       width = 22,height = 15,dpi = 300,units = "cm")


#
# fig 2
#

label_classpop <- c("< 5 mil","5 mil  - 10 mil","10 mil - 20 mil","20 mil - 50 mil",
                    "50 mil - 100 mil","100 mil - 500 mil","> 500 mil")

pop_function <- function(i){
  pop2022 <- sidrar::get_sidra(x = 202
                               , variable = 93
                               , period = as.character(i)
                               , geo = "City"
                               , classific = c("c1")
                               #, geo.filter = "City"

  )
  data.table::setDT(pop2022)
  names(pop2022) <- janitor::make_clean_names(names(pop2022))

  pop2022 <- pop2022[situacao_do_domicilio != "Total" & sexo == "Total" & !is.na(valor),]
  pop2022[,valor_muni_total := sum(valor,na.rm = TRUE),by = .(municipio_codigo,ano)]

  pop2022[valor_muni_total < 5e3,
          class_pop := label_classpop[1]]
  pop2022[valor_muni_total >= 5e3 & valor_muni_total < 10e3,
          class_pop := label_classpop[2]]
  pop2022[valor_muni_total >= 10e3 & valor_muni_total < 20e3,
          class_pop := label_classpop[3]]
  pop2022[valor_muni_total >= 20e3 & valor_muni_total < 50e3,
          class_pop := label_classpop[4]]
  pop2022[valor_muni_total >= 50e3 & valor_muni_total < 100e3,
          class_pop := label_classpop[5]]
  pop2022[valor_muni_total >= 100e3 & valor_muni_total < 500e3,
          class_pop := label_classpop[6]]
  pop2022[valor_muni_total >= 500e3,
          class_pop := label_classpop[7]]

  return(pop2022)
}

pop_all <- lapply(c(1970, 1980, 1991, 2000, 2010), pop_function) %>%
  data.table::rbindlist()


temp <- data.table::copy(pop_all)[,lapply(.SD,sum,na.rm=TRUE),
                                  by = .(ano,situacao_do_domicilio,class_pop),
                                  .SDcols = c('valor')]
temp <- temp[,valor_rel := valor/sum(valor,na.rm = TRUE),by = .(ano,class_pop)]
temp[, valor_rel := round(100* valor_rel,2)]

temp_br <- data.table::melt(data = temp,
                            id.vars = c('ano','class_pop','situacao_do_domicilio'),
                            measure.vars =  c('valor','valor_rel'))

temp_br[,ano := as.numeric(ano)]
temp_br$class_pop <- factor(temp_br$class_pop,label_classpop)

myfrank <- function(x,r){x[rank(x)==r]}
temp_br[,`:=`(min = min(value),
              thrid = myfrank(value,3),
              median = median(value),
              fourth = myfrank(value,4),
              fifth = myfrank(value,5),
              sixth = myfrank(value,6),
              max = max(value)), by = .(variable,ANO)]

ggplot(temp_br[variable %like% "rel"],
       aes(x = ano,y = value, group = class_pop)) +
  geom_line(aes(color = class_pop), size=.8, alpha=.8) +
  geom_point(aes(color = class_pop), size=.95, alpha=.8) +
  scale_color_brewer(palette = "Reds",direction = -1,
                     labels = label_classpop) +
  labs(x = NULL,y = "Taxa de crescimento (%)",
       #title = "Taxa de motorização",
       subtitle = "Taxa de crescimento conforme situação do domicílio e faixa populacional",
       color="Tamanho do \nmunicípio",fill = NULL) +
  facet_wrap(facets = vars(situacao_do_domicilio),nrow = 2,
             #labeller = as_labeller(c('value' = "Situação do domicílio"))
  )+
  ylim(c(0, 1.1* max(max(temp_br[variable %like% "rel",value])))) +
  aop_style1() +
  theme(legend.position = "right",
        strip.placement = "inside",
        legend.text = element_text(size = 10, colour = "#808080"),
        legend.title = element_text(size = 8, colour = "#575757"),
        axis.text.x = element_text(angle = 0, vjust = +1.05, hjust=0)) +
  guides(fill = guide_legend(override.aes = list(shape = NA)))

ggsave("figures/urb_pop/urb_pop_by_citysize.png",scale=1,
       width = 20,height = 15,dpi = 300,units = "cm")


#
# fig3
#

cities10 <- pop_all[ano==2010 & situacao_do_domicilio == "Urbana",][order(valor_muni_total,decreasing = TRUE),]
cities10 <- cities10[1:12, municipio_codigo]

metro_area <- geobr::read_metro_area() %>% data.table::setDT()
metro_munis <- data.table::copy(metro_area)[code_muni %in% cities10,]$name_metro
code_munis <- metro_area[name_metro %in% metro_munis]
code_munis[,code_muni := as.character(code_muni)]
pop10 <- data.table::copy(pop_all)[code_munis,on = c("municipio_codigo" = "code_muni"),
                                   name_metro := i.name_metro]



temp <- pop10[!is.na(name_metro)][,lapply(.SD,sum,na.rm=TRUE),
                                  by = .(ano,situacao_do_domicilio,name_metro),
                                  .SDcols = c('valor')]
temp <- temp[,valor_rel := valor/sum(valor,na.rm = TRUE),by = .(ano,name_metro)]
temp[, valor_rel := round(100* valor_rel,2)]

temp_br <- data.table::melt(data = temp,
                            id.vars = c('ano','name_metro','situacao_do_domicilio'),
                            measure.vars =  c('valor','valor_rel'))

temp_br[,ano := as.numeric(ano)]
#temp_br$class_pop <- factor(temp_br$class_pop,label_classpop)

# myfrank <- function(x,r){x[rank(x)==r]}
# temp_br[,`:=`(min = min(value),
#               thrid = myfrank(value,3),
#               median = median(value),
#               fourth = myfrank(value,4),
#               fifth = myfrank(value,5),
#               sixth = myfrank(value,6),
#               max = max(value)), by = .(variable,ANO)]

ggplot(temp_br[variable %like% "rel"],
       aes(x = ano,y = value, group = situacao_do_domicilio)) +
  geom_area(aes(fill = situacao_do_domicilio), size=.95, alpha=.8) +
  #geom_line(aes(color = situacao_do_domicilio), size=.8, alpha=.8) +
  # geom_point(aes(color = situacao_do_domicilio), size=.95, alpha=.8) +
  #scale_color_manual(values = c(
  #  as.vector(aop_colors$qualitativas[1])
  #  , as.vector(aop_colors$cinzas[3])
    #    as.vector(aop_colors$qualitativas[c(2)]))
  #)) +
  scale_fill_manual(values = c(
    as.vector(aop_colors$qualitativas[1])
    , as.vector(aop_colors$cinzas[3])
    #    as.vector(aop_colors$qualitativas[c(2)]))
  )) +
  labs(x = NULL,y = "Taxa de crescimento (%)",
       #title = "Taxa de motorização",
       subtitle = "Taxa de crescimento conforme situação do domicílio e faixa populacional",
       color="Tamanho do \nmunicípio",fill = NULL) +
  facet_wrap(facets = vars(name_metro),nrow = 4,
             #labeller = as_labeller(c('value' = "Situação do domicílio"))
  )+
  ylim(c(0, 1.1* max(max(temp_br[variable %like% "rel",value])))) +
  aop_style1() +
  theme(legend.position = "right",
        strip.placement = "inside",
        legend.text = element_text(size = 10, colour = "#808080"),
        legend.title = element_text(size = 8, colour = "#575757"),
        axis.text.x = element_text(angle = 45, vjust = +1.55, hjust=1)) +
  guides(fill = guide_legend(override.aes = list(shape = NA)))

ggsave("figures/urb_pop/urb_pop_by_rm.png",scale=1,
       width = 22,height = 15,dpi = 300,units = "cm")


temp_br[,value_million := value/10^6]
ggplot(temp_br[variable %nlike% "rel"],
       aes(x = ano,y = value_million, group = situacao_do_domicilio)) +
  geom_area(aes(fill = situacao_do_domicilio), size=.95, alpha=.8) +
  #geom_line(aes(color = situacao_do_domicilio), size=.8, alpha=.8) +
  # geom_point(aes(color = situacao_do_domicilio), size=.95, alpha=.8) +
  #scale_color_manual(values = c(
  #  as.vector(aop_colors$qualitativas[1])
  #  , as.vector(aop_colors$cinzas[3])
  #    as.vector(aop_colors$qualitativas[c(2)]))
  #)) +
  scale_fill_manual(values = c(
    as.vector(aop_colors$qualitativas[1])
    , as.vector(aop_colors$cinzas[3])
    #    as.vector(aop_colors$qualitativas[c(2)]))
  )) +
  labs(x = NULL,y = "População (milhões)",
       #title = "Taxa de motorização",
       subtitle = "Taxa de crescimento conforme situação do domicílio e faixa populacional",
       color="Tamanho do \nmunicípio",fill = NULL) +
  facet_wrap(facets = vars(name_metro),nrow = 4,scales = "free"
             #labeller = as_labeller(c('value' = "Situação do domicílio"))
  )+
 # ylim(c(0, 1.1* max(max(temp_br[variable %nlike% "rel",value])))) +
  aop_style1() +
  theme(legend.position = "right",
        strip.placement = "inside",
        legend.text = element_text(size = 10, colour = "#808080"),
        legend.title = element_text(size = 8, colour = "#575757"),
        axis.text.x = element_text(angle = 45, vjust = +1.55, hjust=1)) +
  guides(fill = guide_legend(override.aes = list(shape = NA)))

ggsave("figures/urb_pop/urb_pop_by_rm_absoluto.png",scale=1,
       width = 22,height = 15,dpi = 300,units = "cm")

cities10
info1378 <- sidrar::info_sidra(1378)
info1378
sidrar::get_sidra(1378,
                  variable = 93,
                  geo = c("State","City"),
                  geo.filter = list("Region" = 3, "Region" = 3),
                  classific = c("c1"),
                  category = list(1))
