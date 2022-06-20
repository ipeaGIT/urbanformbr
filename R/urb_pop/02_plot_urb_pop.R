#
# load packages-----
#

rm(list=ls())
library(ggrepel)
library(magrittr)
library(data.table)
library("geobr")
source("R/urb_pop/colors_plot.R")
source("R/urb_pop/aop_style1.R")
source("R/fun_support/style.R")
source("R/fun_support/colours.R")

mapview::mapviewOptions(platform = 'mapdeck')

options(mc.cores=20)


# read  -----
## read PNAD -----

pnadc <- readr::read_rds("data/pnadc.rds")

## read Projection----

temp_proj <- readr::read_rds("data/table_6579_ibge.rds")
temp_proj <- temp_proj[,sum(valor),by = ano]

## read CENSO -----

temp_pop <- readr::read_rds("data/table_202_ibge.rds")
temp_pop[,tipo_valor := "total"][,valor := valor/10^6]

# Prep data-----
# filter and remove
temp_pop <- temp_pop[situacao_do_domicilio != "Total" & sexo == "Total"]
temp_pop <- temp_pop[,.(variavel,valor,ano,situacao_do_domicilio)]

temp_pop1 <- data.table::copy(temp_pop)[,valor := round(100*valor/sum(valor),2)
                                        , by = .(ano)]
temp_pop1[,variavel := "Proporção da população"]

# rbindlist
temp_pop2 <- list(temp_pop,temp_pop1) %>% data.table::rbindlist()


pnadc_temp <- data.table::data.table("variavel" = "Proporção da população"
                                     , "valor" = 100 * pnadc$mean
                                     , "ano" = 2020
                                     , "situacao_do_domicilio" = c("Urbana","Rural"))
pnadc_temp1 <- data.table::data.table("variavel" = "População residente"
                                      , "valor" = pnadc$mean * temp_proj$V1 /10^6
                                      , "ano" = 2020
                                      , "situacao_do_domicilio" = c("Urbana","Rural"))

pnadc_temp2  <- list(pnadc_temp1,pnadc_temp) %>% data.table::rbindlist()

# Rbindlist CENSO and PNAD----

situacao <-  list(pnadc_temp2,temp_pop2) %>% data.table::rbindlist()
situacao$variavel <- factor(situacao$variavel
                            , levels = c("População residente","Proporção da população"))

situacao[variavel == "Proporção da população",label := paste(round(valor,1),"%")]
situacao[variavel == "Proporção da população"
         ,valor_label := data.table::fifelse(valor == max(valor)
                                             ,max(valor)-10
                                             ,max(valor)+5.5),by = ano]


# Plot populacao  ----
ggplot(data = situacao) +
  geom_bar(stat = "identity",
           aes(x = ano, y = valor,fill = situacao_do_domicilio))+
  scale_fill_manual(values = c(
    as.vector(aop_colors$qualitativas[1])
    , as.vector(aop_colors$cinzas[3])
  )) +
  facet_wrap(~variavel,ncol=1,scales = "free_y",
             labeller = as_labeller(c('Proporção da população' = "Proporção da população (%)",
                                      'População residente' = "População total (milhões)")))+
  labs(x = NULL, y = NULL,
       #subtitle = "População conforme situação de domicílio",
       caption = 'Fonte: Censos IBGE (1970, 1980, 1991, 2010) e PNAD (2020)',
       fill="Situação do \ndomicílio",fill = NULL) +
  aop_style1() +
  theme(legend.position = "right",
        strip.placement = "inside",
        legend.text = element_text(size = 10, colour = "#808080"),
        legend.title = element_text(size = 8, colour = "#575757"),
        axis.text.x = element_text(angle = 0, vjust = +1.55, hjust=1)) +
  geom_text(data = situacao,aes(x = ano, y = valor_label,label = label
                                ,color=situacao_do_domicilio),size = 3.0)+
  scale_color_manual(values = c("white","black")) +
  guides(fill = guide_legend(override.aes = list(shape = NA)),
         color = FALSE)

dir.create("figures")
dir.create("figures/urb_pop")

ggsave("figures/figura_5_urb_pop_oveview.pdf",
       width = 15,height = 15,dpi = 300,units = "cm")


# AMC analysis-------
gc(reset = T, full = T)
options(mc.cores=20)# set number of cores
# future::plan(strategy = 'multisession', workers=10)

# add 2020 projection on census 1970 - 2010 data

pop <- readr::read_rds("data/population_muni_ibge.rds")
pop_proj_2020 <- readr::read_rds("data/table_6579_ibge.rds")
pop_muni <- list(pop,pop_proj_2020) %>% data.table::rbindlist(use.names = TRUE
                                                              ,fill = TRUE)

pop_muni[,c("situacao_do_domicilio",
            "situacao_do_domicilio_codigo",
            "sexo_codigo","sexo") := NULL]

# read amc
amc_muni <- readr::read_rds("data/comparable_areas_ibge.rds")
# merge amc data to muni_data
pop_muni[amc_muni,on = c("municipio_codigo" = "code_muni_2010"),
         code_amc := i.code_amc]
# add state amd great regions on data
gr_geobr <- geobr::read_state() %>% setDT()
muni_geobr <- geobr::read_municipality() %>% setDT() %>%
  dplyr::mutate(code_muni = as.character(code_muni))
pop_muni[muni_geobr, on = c("municipio_codigo" = "code_muni"),
         abbrev_state := i.abbrev_state]
pop_muni[gr_geobr, on = c("abbrev_state"),
         name_region := i.name_region]
# add metro area
metro_area <- geobr::read_metro_area() %>% setDT() %>%
  dplyr::mutate(code_muni= as.character(code_muni))
pop_muni[metro_area,
         on = c("municipio_codigo"="code_muni"),
         name_metro := i.name_metro]

# pop_muni[ano_codigo %in% c(2010,2020) &
#            name_metro %like% "Manaus"][
#              order(municipio_codigo)][,
#                                       .(valor,municipio,ano_codigo)]
# check conta antes
# sum(pop_muni$valor,na.rm = TRUE)
# [1] 931277871

# calcula a populacao por amc
pop_muni_df <- data.table::copy(pop_muni)[
  , lapply(.SD,sum,na.rm=TRUE)
  , by = .(code_amc,abbrev_state,name_region,ano)
  ,.SDcols = "valor"]

# confere conta depois
# sum(pop_muni$valor,na.rm = TRUE)
# [1] 931277871

# categoriza por populacao

label_classpop <- c("< 5 mil","5 mil  - 10 mil","10 mil - 20 mil","20 mil - 50 mil",
                    "50 mil - 100 mil","100 mil - 500 mil","> 500 mil")



pop_2010 <- data.table::copy(pop_muni_df)[ano == 2010]
pop_2010[valor < 5e3,
         class_pop_2010 := label_classpop[1]]
pop_2010[valor >= 5e3 & valor < 10e3 ,
         class_pop_2010 := label_classpop[2]]
pop_2010[valor >= 10e3 & valor < 20e3,
         class_pop_2010 := label_classpop[3]]
pop_2010[valor >= 20e3 & valor < 50e3,
         class_pop_2010 := label_classpop[4]]
pop_2010[valor >= 50e3 & valor < 100e3,
         class_pop_2010 := label_classpop[5]]
pop_2010[valor >= 100e3 & valor < 500e3,
         class_pop_2010 := label_classpop[6]]
pop_2010[valor >= 500e3 ,
         class_pop_2010 := label_classpop[7]]

pop_muni_df[pop_2010,on = "code_amc", class_pop_2010 := i.class_pop_2010]

rm(pop_2010)

### taxa geometrica de crescimento

# add 1980-1970

dt_1970 <- data.table::copy(pop_muni_df)[ano == "1970",]
dt_1980 <- data.table::copy(pop_muni_df)[ano == "1980",]
dt_1991 <- data.table::copy(pop_muni_df)[ano == "1991",]
dt_2000 <- data.table::copy(pop_muni_df)[ano == "2000",]
dt_2010 <- data.table::copy(pop_muni_df)[ano == "2010",]
dt_2020 <- data.table::copy(pop_muni_df)[ano == "2020",][,valor_2020 := valor][,valor := NULL]

dt_2020[dt_1970,on = "code_amc",valor_1970 := i.valor]#[,ano_1970 := 1970];rm(dt_1970)
dt_2020[dt_1980,on = "code_amc",valor_1980 := i.valor]#[,ano_1980 := 1980];rm(dt_1980)
dt_2020[dt_1991,on = "code_amc",valor_1991 := i.valor]#[,ano_1991 := 1991];rm(dt_1991)
dt_2020[dt_2000,on = "code_amc",valor_2000 := i.valor]#[,ano_2000 := 2000];rm(dt_2000)
dt_2020[dt_2010,on = "code_amc",valor_2010 := i.valor]#[,ano_2010 := 2010];rm(dt_2010)
#dt_2020[, ano_2020 := 2020][,ano := NULL]

# weight
dt_2020[,w70_80 := 0.5 * (valor_1970 + valor_1980)]
dt_2020[,w80_91 := 0.5 * (valor_1980 + valor_1991)]
dt_2020[,w91_00 := 0.5 * (valor_1991 + valor_2000)]
dt_2020[,w00_10 := 0.5 * (valor_2000 + valor_2010)]
dt_2020[,w10_20 := 0.5 * (valor_2010 + valor_2020)]

dt_2020[,r70_80 := 100 * ((valor_1980 / valor_1970)^(1/(1980-1970)) - 1)]
dt_2020[,r80_91 := 100 * ((valor_1991 / valor_1980)^(1/(1991-1980)) - 1)]
dt_2020[,r91_00 := 100 * ((valor_2000 / valor_1991)^(1/(2000-1991)) - 1)]
dt_2020[,r00_10 := 100 * ((valor_2010 / valor_2000)^(1/(2010-2000)) - 1)]
dt_2020[,r10_20 := 100 * ((valor_2020 / valor_2010)^(1/(2020-2010)) - 1)]

dt_2020[is.infinite(r70_80) & is.nan(r70_80),r70_80 := NA]
dt_2020[is.infinite(r80_91) & is.nan(r80_91),r80_91 := NA]
dt_2020[is.infinite(r91_00) & is.nan(r91_00),r91_00 := NA]
dt_2020[is.infinite(r00_10) & is.nan(r00_10),r00_10 := NA]
dt_2020[is.infinite(r10_20) & is.nan(r10_20),r10_20 := NA]

# dt_2020[,r70_80_adj := weighted.mean(x = r70_80,w = w70_80) %>% round(4), by = class_pop_2010]
# dt_2020[,r80_91_adj := weighted.mean(x = r80_91,w = w80_91) %>% round(4), by = class_pop_2010]
# dt_2020[,r91_00_adj := weighted.mean(x = r91_00,w = w91_00) %>% round(4), by = class_pop_2010]
# dt_2020[,r00_10_adj := weighted.mean(x = r00_10,w = w00_10) %>% round(4), by = class_pop_2010]
# dt_2020[,r10_20_adj := weighted.mean(x = r10_20,w = w10_20) %>% round(4), by = class_pop_2010]
# dt_2020[,.SD[1],by = class_pop_2010][!is.na(class_pop_2010),
#                                      .(class_pop_2010,r70_80_adj,r80_91_adj,r91_00_adj,r00_10_adj,r10_20_adj)]


tmp_plot <- data.table::copy(dt_2020)

tmp_plot <- data.table::melt(data = tmp_plot,
                             id_vars = c('code_amc','abbrev_state','name_region',
                                         'class_pop_2010'),
                             measure.vars = list( 'taxa' = c('r70_80','r80_91','r91_00',
                                                             'r00_10','r10_20')),
                             variable.name = "posicao_taxa",
                             value.name = "taxa")

tmp_plot[ , c(names(tmp_plot)[names(tmp_plot) %like% "valor"]) := NULL]
tmp_plot[, ano := NULL]

tmp_plot
# estimate statisc on population intervals
tmp_plot <- tmp_plot[!is.na(class_pop_2010)][, .(median = median(taxa, na.rm=T),
                                                 q25 = quantile(x = taxa,probs = .25, na.rm=T),
                                                 q75 = quantile(x = taxa,probs = .75, na.rm=T),
                                                 lo = mean(taxa, na.rm=T) - 2*sd(taxa, na.rm=T),
                                                 hi = mean(taxa, na.rm=T) + 2*sd(taxa, na.rm=T)),
                                             by= .(posicao_taxa, class_pop_2010)]
## adjust factors
tmp_plot$class_pop_2010_f <- factor(tmp_plot$class_pop_2010,
                                    levels = c("< 5 mil",
                                               "5 mil  - 10 mil",
                                               "10 mil - 20 mil",
                                               "20 mil - 50 mil",
                                               "50 mil - 100 mil",
                                               "100 mil - 500 mil",
                                               "> 500 mil"),
                                    labels = c("< 5 mil",
                                               "5 mil  - 10 mil",
                                               "10 mil - 20 mil",
                                               "20 mil - 50 mil",
                                               "50 mil - 100 mil",
                                               "100 mil - 500 mil",
                                               "> 500 mil"))
tmp_plot$posicao_taxa_f <- factor(tmp_plot$posicao_taxa,
                                  levels = c("r70_80",
                                             "r80_91",
                                             "r91_00",
                                             'r00_10',
                                             "r10_20"),
                                  labels = c("1970 - 1980",
                                             "1980 - 1991",
                                             "1991 - 2000",
                                             "2000 - 2010",
                                             "2010 - 2020"))

## plot pop
ggplot(data = tmp_plot) +
  geom_errorbar( aes(x = posicao_taxa, ymax = q25,
                     ymin = q75,color= class_pop_2010_f),
                 position = "dodge",
                 size = .5, width = 0.5)+#, color='gray40') +
  geom_point(aes(x = posicao_taxa,y = median,
                 color = class_pop_2010_f),
             position =  position_dodge(width = 0.5),
             size = 1.5) +
  geom_line(aes(x = posicao_taxa,y = median, color = class_pop_2010_f,
                group = class_pop_2010_f),
            position =  position_dodge(width = 0.5),size=0.75) +
  geom_hline(yintercept = 0, color= "grey55", size =1, linetype= "dotted") +
  scale_color_brewer(palette = "Reds",direction = -1) +
  labs(x = "Intervalo de análise",
       y = "Taxa de crescimento (%)",
       #title = "Taxa de crescimento",
       color = "Faixa populacional",
       #subtitle = "Crescimento geométrico conforme faixa populacional desde 1970"
       , caption = 'Taxa apresentada para média, 1º e 3º quartil;\nFonte: Censos IBGE (1970, 1980, 1991, 2010) e projeção populacional (2020).\nFaixas populacionais definidas conforme Áreas Mínimas Comparáveis (AMC), desde 1970.'
  ) +
  scale_y_continuous(sec.axis = sec_axis(~ .))+
  scale_x_discrete(breaks = c("r70_80",
                              "r80_91",
                              "r91_00",
                              'r00_10',
                              "r10_20"),
                   labels = c("1970 - 1980",
                              "1980 - 1991",
                              "1991 - 2000",
                              "2000 - 2010",
                              "2010 - 2020")) +
  #aop_style1() +
  theme_bw()+
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 0, hjust = 0.5,size=8),
        axis.text.y = element_text(angle = 0, hjust = 1,size=8),
        axis.line.x = element_line(size = 0.5, color = "grey"),
        #axis.ticks = element_line(
        #  colour = "grey15",
        #  size = 0.5,
        #  linetype = 1),
        #panel.grid.major.x = element_line(size = 0.15, color = "grey"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
#coord_cartesian(xlim = limits_x, expand = FALSE)

ggsave("figures/figura_7_faixa_br.pdf",scale=1,
       width = 27*0.7,height = 18*0.7,dpi = 300,units = "cm")




# figura 8----------



most_pop <- pop_muni[ano == 2010][order(valor,decreasing = TRUE)][1:12,municipio_codigo]

get_name_metro <- metro_area[code_muni %in% most_pop,name_metro]
get_name_muni <- metro_area[name_metro %in% get_name_metro,]

pop_muni_df <- data.table::copy(pop_muni)[name_metro %in% get_name_metro,]
pop_muni_df[,status_metro := fifelse(municipio_codigo %in% most_pop,
                                     "Principal","Periferia metropolitana")]

# calcula a populacao por amc
pop_muni_df2 <- data.table::copy(pop_muni_df)[, lapply(.SD,sum,na.rm=TRUE),
                                              by = .(code_amc,abbrev_state,
                                                     name_region,ano,
                                                     name_metro,status_metro),
                                              .SDcols = "valor"]

pop_muni_df2[ano %in% c(2010,2020) &
               name_metro %like% "Manaus"][order(code_amc)][code_amc == "1025"]


dt_1970 <- data.table::copy(pop_muni_df2)[ano == "1970",]
dt_1980 <- data.table::copy(pop_muni_df2)[ano == "1980",]
dt_1991 <- data.table::copy(pop_muni_df2)[ano == "1991",]
dt_2000 <- data.table::copy(pop_muni_df2)[ano == "2000",]
dt_2010 <- data.table::copy(pop_muni_df2)[ano == "2010",]
dt_2020 <- data.table::copy(pop_muni_df2)[ano == "2020",][,valor_2020 := valor][,valor := NULL]

dt_2020[dt_1970,on = c("code_amc","status_metro"),valor_1970 := i.valor]#[,ano_1970 := 1970];rm(dt_1970)
dt_2020[dt_1980,on = c("code_amc","status_metro"),valor_1980 := i.valor]#[,ano_1980 := 1980];rm(dt_1980)
dt_2020[dt_1991,on = c("code_amc","status_metro"),valor_1991 := i.valor]#[,ano_1991 := 1991];rm(dt_1991)
dt_2020[dt_2000,on = c("code_amc","status_metro"),valor_2000 := i.valor]#[,ano_2000 := 2000];rm(dt_2000)
dt_2020[dt_2010,on = c("code_amc","status_metro"),valor_2010 := i.valor]#[,ano_2010 := 2010];rm(dt_2010)
#dt_2020[, ano_2020 := 2020][,ano := NULL]

# weight
dt_2020[,w70_80 := 0.5 * (valor_1970 + valor_1980)]
dt_2020[,w80_91 := 0.5 * (valor_1980 + valor_1991)]
dt_2020[,w91_00 := 0.5 * (valor_1991 + valor_2000)]
dt_2020[,w00_10 := 0.5 * (valor_2000 + valor_2010)]
dt_2020[,w10_20 := 0.5 * (valor_2010 + valor_2020)]

dt_2020[,r70_80 := 100 * ((valor_1980 / valor_1970)^(1/(1980-1970)) - 1)]
dt_2020[,r80_91 := 100 * ((valor_1991 / valor_1980)^(1/(1991-1980)) - 1)]
dt_2020[,r91_00 := 100 * ((valor_2000 / valor_1991)^(1/(2000-1991)) - 1)]
dt_2020[,r00_10 := 100 * ((valor_2010 / valor_2000)^(1/(2010-2000)) - 1)]
dt_2020[,r10_20 := 100 * ((valor_2020 / valor_2010)^(1/(2020-2010)) - 1)]

dt_2020[is.infinite(r70_80) | is.nan(r70_80), r70_80 := NA]
dt_2020[is.infinite(r80_91) | is.nan(r80_91), r80_91 := NA]
dt_2020[is.infinite(r91_00) | is.nan(r91_00), r91_00 := NA]
dt_2020[is.infinite(r00_10) | is.nan(r00_10), r00_10 := NA]
dt_2020[is.infinite(r10_20) | is.nan(r10_20), r10_20 := NA]

dt_2020[,r70_80_adj := weighted.mean(x = r70_80,w = w70_80, na.rm = TRUE) %>% round(4), by = .(name_metro,status_metro)]
dt_2020[,r80_91_adj := weighted.mean(x = r80_91,w = w80_91, na.rm = TRUE) %>% round(4), by = .(name_metro,status_metro)]
dt_2020[,r91_00_adj := weighted.mean(x = r91_00,w = w91_00, na.rm = TRUE) %>% round(4), by = .(name_metro,status_metro)]
dt_2020[,r00_10_adj := weighted.mean(x = r00_10,w = w00_10, na.rm = TRUE) %>% round(4), by = .(name_metro,status_metro)]
dt_2020[,r10_20_adj := weighted.mean(x = r10_20,w = w10_20, na.rm = TRUE) %>% round(4), by = .(name_metro,status_metro)]
dt_2020 <- data.table::copy(dt_2020)[,.SD[1],by = .(name_metro,status_metro)]

dt_2020[1:3]
tmp_plot <- data.table::copy(dt_2020)

tmp_plot <- data.table::melt(data = tmp_plot,
                             id_vars = c('code_amc','abbrev_state','name_region',
                                         'name_metro','status_metro'),
                             # measure.vars = list( 'taxa' = c('r70_80','r80_91','r91_00',
                             #                                 'r00_10','r10_20')),
                             measure.vars = list( 'taxa' = c('r70_80_adj','r80_91_adj','r91_00_adj',
                                                             'r00_10_adj','r10_20_adj')),
                             variable.name = "posicao_taxa",
                             value.name = "taxa")

tmp_plot[ , c(names(tmp_plot)[names(tmp_plot) %like% "valor"]) := NULL]
tmp_plot[, ano := NULL]

# estimate statisc on population intervals
tmp_plot <- data.table::copy(tmp_plot)[,
                                       .(taxa = mean(taxa, na.rm=T)),
                                       by= .(posicao_taxa, name_metro,status_metro)]

tmp_plot$status_metro_f <- factor(tmp_plot$status_metro,
                                  levels = c("Periferia metropolitana","Principal"),
                                  labels = c("Periferia \nmetropolitana","Principal"))
tmp_plot$name_metro_f <- factor(tmp_plot$name_metro,
                                levels = c("RIDE - Região Integrada de Desenvolvimento do Distrito Federal e Entorno",
                                           "RM Belém",
                                           "RM Belo Horizonte",
                                           "RM Curitiba",
                                           "RM Fortaleza",
                                           "RM Goiânia",
                                           "RM Manaus",
                                           "RM Porto Alegre",
                                           "RM Recife",
                                           "RM Rio de Janeiro",
                                           "RM Salvador",
                                           "RM São Paulo"),
                                labels = c("Brasília",
                                           "Belém",
                                           "Belo Horizonte",
                                           "Curitiba",
                                           "Fortaleza",
                                           "Goiânia",
                                           "Manaus",
                                           "Porto Alegre",
                                           "Recife",
                                           "Rio de Janeiro",
                                           "Salvador",
                                           "São Paulo"))
ggplot(data = tmp_plot) +
  geom_point(aes(x = posicao_taxa,y = taxa,
                 color = status_metro_f),
             #position =  position_dodge(width = 0.5),
             size = 1.5) +
  geom_line(aes(x = posicao_taxa,
                y = taxa,
                color = status_metro_f,
                group = status_metro_f),
            #position =  position_dodge(width = 0.5),
            size=0.75) +
  geom_hline(yintercept = 0, color= "grey55", size =0, linetype= "dotted") +
  facet_wrap(vars(name_metro_f),nrow = 4)+
  #scale_color_brewer(palette = "Reds",direction = -1) +
  scale_color_manual(values = c(
    as.vector(aop_colors$cinzas[3]),
    as.vector(aop_colors$qualitativas[1])
    #    as.vector(aop_colors$qualitativas[c(2)]))
  )) +
  labs(x = "Intervalo de análise",
       y = "Taxa de crescimento (%)",
      # title = "Taxa de crescimento",
       color = "Municípios",
      #subtitle = "Crescimento geométrico conforme categoria de municípios da Região Metropolitana"
       , caption = 'Fonte: Censos IBGE (1970, 1980, 1991, 2010) e projeção populacional (2020).\nFaixas populacionais definidas conforme Áreas Mínimas Comparáveis (AMC), desde 1970.'
  ) +
  scale_x_discrete(breaks = c("r70_80_adj",
                              "r80_91_adj",
                              "r91_00_adj",
                              'r00_10_adj',
                              "r10_20_adj"),
                   labels = c("1970-\n1980",
                              "1980-\n1991",
                              "1991-\n2000",
                              "2000-\n2010",
                              "2010-\n2020")) +
  theme_bw()+
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 0, hjust = 0.5,size=8),
        axis.text.y = element_text(angle = 0, hjust = 1,size=8),
        axis.line.x = element_line(size = 0.5, color = "grey"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("figures/figura_8_urb_pop_by_rm.pdf",
       width = 20,height = 20,dpi = 300,units = "cm")


# figura 6 -------
# calcula a populacao por amc
pop_muni_df2 <- list(data.table::copy(pop_muni)[,escopo := "regional"],
                     data.table::copy(pop_muni)[,escopo := "Brasil"][,name_region := "Brasil"]) %>%
  data.table::rbindlist()
pop_muni_df2 <- data.table::copy(pop_muni_df2)[, lapply(.SD,sum,na.rm=TRUE),
                                               by = .(code_amc,abbrev_state,
                                                      name_region,ano,escopo),
                                               .SDcols = "valor"]
pop_muni_df2 <- pop_muni_df2[!is.na(name_region)]



dt_1970 <- data.table::copy(pop_muni_df2)[ano == "1970",]
dt_1980 <- data.table::copy(pop_muni_df2)[ano == "1980",]
dt_1991 <- data.table::copy(pop_muni_df2)[ano == "1991",]
dt_2000 <- data.table::copy(pop_muni_df2)[ano == "2000",]
dt_2010 <- data.table::copy(pop_muni_df2)[ano == "2010",]
dt_2020 <- data.table::copy(pop_muni_df2)[ano == "2020",][,valor_2020 := valor][,valor := NULL]

dt_2020[dt_1970,on = c("code_amc" , "escopo"),valor_1970 := i.valor]#[,ano_1970 := 1970];rm(dt_1970)
dt_2020[dt_1980,on = c("code_amc" , "escopo"),valor_1980 := i.valor]#[,ano_1980 := 1980];rm(dt_1980)
dt_2020[dt_1991,on = c("code_amc" , "escopo"),valor_1991 := i.valor]#[,ano_1991 := 1991];rm(dt_1991)
dt_2020[dt_2000,on = c("code_amc" , "escopo"),valor_2000 := i.valor]#[,ano_2000 := 2000];rm(dt_2000)
dt_2020[dt_2010,on = c("code_amc" , "escopo"),valor_2010 := i.valor]#[,ano_2010 := 2010];rm(dt_2010)
#dt_2020[, ano_2020 := 2020][,ano := NULL]

# weight
dt_2020[,w70_80 := 0.5 * (valor_1970 + valor_1980)]
dt_2020[,w80_91 := 0.5 * (valor_1980 + valor_1991)]
dt_2020[,w91_00 := 0.5 * (valor_1991 + valor_2000)]
dt_2020[,w00_10 := 0.5 * (valor_2000 + valor_2010)]
dt_2020[,w10_20 := 0.5 * (valor_2010 + valor_2020)]

dt_2020[,r70_80 := 100 * ((valor_1980 / valor_1970)^(1/(1980-1970)) - 1)]
dt_2020[,r80_91 := 100 * ((valor_1991 / valor_1980)^(1/(1991-1980)) - 1)]
dt_2020[,r91_00 := 100 * ((valor_2000 / valor_1991)^(1/(2000-1991)) - 1)]
dt_2020[,r00_10 := 100 * ((valor_2010 / valor_2000)^(1/(2010-2000)) - 1)]
dt_2020[,r10_20 := 100 * ((valor_2020 / valor_2010)^(1/(2020-2010)) - 1)]

dt_2020[is.infinite(r70_80) | is.nan(r70_80), r70_80 := NA]
dt_2020[is.infinite(r80_91) | is.nan(r80_91), r80_91 := NA]
dt_2020[is.infinite(r91_00) | is.nan(r91_00), r91_00 := NA]
dt_2020[is.infinite(r00_10) | is.nan(r00_10), r00_10 := NA]
dt_2020[is.infinite(r10_20) | is.nan(r10_20), r10_20 := NA]

dt_2020[,r70_80_adj := weighted.mean(x = r70_80,w = w70_80, na.rm = TRUE) %>% round(4), by = .(name_region, escopo)]
dt_2020[,r80_91_adj := weighted.mean(x = r80_91,w = w80_91, na.rm = TRUE) %>% round(4), by = .(name_region, escopo)]
dt_2020[,r91_00_adj := weighted.mean(x = r91_00,w = w91_00, na.rm = TRUE) %>% round(4), by = .(name_region, escopo)]
dt_2020[,r00_10_adj := weighted.mean(x = r00_10,w = w00_10, na.rm = TRUE) %>% round(4), by = .(name_region, escopo)]
dt_2020[,r10_20_adj := weighted.mean(x = r10_20,w = w10_20, na.rm = TRUE) %>% round(4), by = .(name_region, escopo)]
dt_2020 <- data.table::copy(dt_2020)[,.SD[1],by = .(name_region,escopo)]



tmp_plot <- data.table::copy(dt_2020)

tmp_plot <- data.table::melt(data = tmp_plot,
                             id_vars = c('abbrev_state','name_region','escopo'),
                             # measure.vars = list( 'taxa' = c('r70_80','r80_91','r91_00',
                             #                                 'r00_10','r10_20')),
                             measure.vars = list( 'taxa' = c('r70_80_adj','r80_91_adj','r91_00_adj',
                                                             'r00_10_adj','r10_20_adj')),
                             variable.name = "posicao_taxa",
                             value.name = "taxa")

tmp_plot[ , c(names(tmp_plot)[names(tmp_plot) %like% "valor"]) := NULL]
tmp_plot[, ano := NULL]
tmp_plot[escopo %in% "Brasil",]
tmp_plot
# estimate statisc on population intervals
tmp_plot$name_region_f <- factor(tmp_plot$name_region,
                                 levels = c("Norte","Nordeste",
                                            "Centro Oeste","Sudeste","Sul","Brasil"))



ggplot(data = tmp_plot,aes(x = posicao_taxa,y = taxa,
                           color = name_region_f)) +
  geom_point(size = 1.5,alpha = 0.75) +
  geom_line(aes(group = name_region_f,alpha=0.75),size=0.75) +
  geom_hline(yintercept = 0, color= "grey55",
             size =0, linetype= "dotted") +
  scale_color_manual(values =
                       c(as.vector(aop_colors$qualitativo[c(3)])
                         , as.vector(aop_colors$qualitativas[6])
                         , "palegreen3"
                         , as.vector(aop_colors$caqui[5])
                         , as.vector(aop_colors$qualitativas[c(1)])
                         , as.vector(aop_colors$cinzas[3]))
  ) +
  labs(x = "Intervalo de análise",
       y = "Taxa de crescimento (%)",
       #title = "Taxa de crescimento",
       color = "Regiões",
       #subtitle = "Crescimento geométrico conforme regiões brasileiras"
       , caption = 'Fonte: Censos IBGE (1970, 1980, 1991, 2010) e projeção populacional (2020).\nFaixas populacionais definidas conforme Áreas Mínimas Comparáveis (AMC), desde 1970.'
  ) +
  guides(alpha = "none")+
  #scale_y_continuous(sec.axis = sec_axis(~ .))+
  scale_x_discrete(breaks = c("r70_80_adj",
                              "r80_91_adj",
                              "r91_00_adj",
                              'r00_10_adj',
                              "r10_20_adj"),
                   labels = c("1970-1980",
                              "1980-1991",
                              "1991-2000",
                              "2000-2010",
                              "2010-2020")) +
  theme_bw()+
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 0, hjust = 0.5,size=8),
        axis.text.y = element_text(angle = 0, hjust = 1,size=8),
        axis.line.x = element_line(size = 0.5, color = "grey"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("figures/figura_6_urb_pop_regioes.pdf",scale=1,
       width = 27*0.7,height = 18*0.7,dpi = 300,units = "cm")

# Figura 7 ------
rm(list=ls())
gc(reset = T, full = T)

# add 2020 projection on census 1970 - 2010 data

pop <- readr::read_rds("../../data/urbanformbr/population/population_muni_ibge.rds")
pop_proj_2020 <- readr::read_rds("../../data/urbanformbr/population/table_6579_ibge.rds")
pop_muni <- list(pop,pop_proj_2020) %>% data.table::rbindlist(use.names = TRUE
                                                              ,fill = TRUE)

pop_muni[,c("situacao_do_domicilio",
            "situacao_do_domicilio_codigo",
            "sexo_codigo","sexo") := NULL]

# read amc
amc_muni <- readr::read_rds("../../data/urbanformbr/population/comparable_areas_ibge.rds")

# merge amc data to muni_data
pop_muni[amc_muni,on = c("municipio_codigo" = "code_muni_2010"),
         code_amc := i.code_amc]

# add state and great regions to data
gr_geobr <- geobr::read_state() %>% setDT()
muni_geobr <- geobr::read_municipality() %>% setDT() %>%
  dplyr::mutate(code_muni = as.character(code_muni))
pop_muni[muni_geobr, on = c("municipio_codigo" = "code_muni"),
         abbrev_state := i.abbrev_state]
pop_muni[gr_geobr, on = c("abbrev_state"),
         name_region := i.name_region]

# add metro area
metro_area <- geobr::read_metro_area() %>% setDT() %>%
  dplyr::mutate(code_muni= as.character(code_muni))
pop_muni[metro_area,
         on = c("municipio_codigo"="code_muni"),
         name_metro := i.name_metro]

# pop_muni[ano_codigo %in% c(2010,2020) &
#            name_metro %like% "Manaus"][
#              order(municipio_codigo)][,
#                                       .(valor,municipio,ano_codigo)]
# check conta antes
# sum(pop_muni$valor,na.rm = TRUE)
# [1] 931277871

# calcula a populacao por amc
pop_muni_df <- data.table::copy(pop_muni)[, lapply(.SD,sum,na.rm=TRUE),
                                          by = .(code_amc,abbrev_state,
                                                 name_region,ano),
                                          .SDcols = "valor"]

# confere conta depois
# sum(pop_muni$valor,na.rm = TRUE)
# [1] 931277871

# categoriza por populacao

label_classpop <- c("< 5 mil","5 mil  - 10 mil","10 mil - 20 mil","20 mil - 50 mil",
                    "50 mil - 100 mil","100 mil - 500 mil","> 500 mil")

pop_2010 <- data.table::copy(pop_muni_df)[ano == 2010]
pop_2010[valor < 5e3,
         class_pop_2010 := label_classpop[1]]
pop_2010[valor >= 5e3 & valor < 10e3 ,
         class_pop_2010 := label_classpop[2]]
pop_2010[valor >= 10e3 & valor < 20e3,
         class_pop_2010 := label_classpop[3]]
pop_2010[valor >= 20e3 & valor < 50e3,
         class_pop_2010 := label_classpop[4]]
pop_2010[valor >= 50e3 & valor < 100e3,
         class_pop_2010 := label_classpop[5]]
pop_2010[valor >= 100e3 & valor < 500e3,
         class_pop_2010 := label_classpop[6]]
pop_2010[valor >= 500e3 ,
         class_pop_2010 := label_classpop[7]]

pop_muni_df[pop_2010,on = "code_amc", class_pop_2010 := i.class_pop_2010]
rm(pop_2010)

table(pop_muni_df$class_pop_2010, useNA = 'always')
subset(pop_muni_df, is.na(class_pop_2010))


### taxa geometrica de crescimento

# Spread data from long to wide
dt_2020 <- data.table::dcast(pop_muni_df,
                             code_amc + class_pop_2010 ~ ano,
                             value.var = "valor", fun = sum)
names(dt_2020)[3:8] <- paste0('valor_', names(dt_2020)[3:8])
head(dt_2020)

# calculate growth rates
dt_2020[,r70_80 := 100 * ((valor_1980 / valor_1970)^(1/(1980-1970)) - 1)]
dt_2020[,r80_91 := 100 * ((valor_1991 / valor_1980)^(1/(1991-1980)) - 1)]
dt_2020[,r91_00 := 100 * ((valor_2000 / valor_1991)^(1/(2000-1991)) - 1)]
dt_2020[,r00_10 := 100 * ((valor_2010 / valor_2000)^(1/(2010-2000)) - 1)]
dt_2020[,r10_20 := 100 * ((valor_2020 / valor_2010)^(1/(2020-2010)) - 1)]

dt_2020[is.infinite(r70_80) | is.nan(r70_80),r70_80 := NA]
dt_2020[is.infinite(r80_91) | is.nan(r80_91),r80_91 := NA]
dt_2020[is.infinite(r91_00) | is.nan(r91_00),r91_00 := NA]
dt_2020[is.infinite(r00_10) | is.nan(r00_10),r00_10 := NA]
dt_2020[is.infinite(r10_20) | is.nan(r10_20),r10_20 := NA]

tmp_plot <- data.table::copy(dt_2020)

tmp_plot <- data.table::melt(data = tmp_plot,
                             id_vars = c('code_amc','abbrev_state','name_region',
                                         'class_pop_2010'),
                             measure.vars = list( 'taxa' = c('r70_80','r80_91','r91_00',
                                                             'r00_10','r10_20')),
                             variable.name = "posicao_taxa",
                             value.name = "taxa")


tmp_plot[taxa >20]

## adjust factors
tmp_plot$class_pop_2010_f <- factor(tmp_plot$class_pop_2010,
                                    levels = c("< 5 mil",
                                               "5 mil  - 10 mil",
                                               "10 mil - 20 mil",
                                               "20 mil - 50 mil",
                                               "50 mil - 100 mil",
                                               "100 mil - 500 mil",
                                               "> 500 mil"),
                                    labels = c("< 5 mil",
                                               "5 mil  - 10 mil",
                                               "10 mil - 20 mil",
                                               "20 mil - 50 mil",
                                               "50 mil - 100 mil",
                                               "100 mil - 500 mil",
                                               "> 500 mil"))
tmp_plot$posicao_taxa_f <- factor(tmp_plot$posicao_taxa,
                                  levels = c("r70_80",
                                             "r80_91",
                                             "r91_00",
                                             'r00_10',
                                             "r10_20"),
                                  labels = c("1970 - 1980",
                                             "1980 - 1991",
                                             "1991 - 2000",
                                             "2000 - 2010",
                                             "2010 - 2020"))


## plot pop

ggplot(data=subset(tmp_plot, !is.na(class_pop_2010_f))) +
  geom_hline(yintercept = 0, color= "grey55", size =1, linetype= "dotted") +
  geom_boxplot(aes(x=posicao_taxa_f, y=taxa, color=class_pop_2010_f),
               outlier.alpha= .1) +
  ylim(c(-10, 12)) +
  labs(x = "Período",
       y = "Taxa de crescimento (%)",
      color = "Faixa populacional\n em 2010",
      , caption = 'Fonte: Censos IBGE (1970, 1980, 1991, 2010) e projeção populacional (2020).<br>Faixas populacionais definidas conforme Áreas Mínimas Comparáveis (AMC) entre 1970-2010'
  ) +
  scale_colour_aop(reverse=T) +
  aop_style1() +
  theme(legend.position = 'right')


ggsave("./figures/figura_7_faixa_br_box.pdf",scale=1,
       width = 16, height = 12,dpi = 300,units = "cm",bg = "white")


# end ----
