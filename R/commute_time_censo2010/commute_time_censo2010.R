###' calculate average commute time for Brazil's
###' 187 urban concentration areas


# load libraries
source('./R/setup.R')




###### Get census data ------------------
#' V0001	UNIDADE DA FEDERAÇÃO
#' V0002	CÓDIGO DO MUNICÍPIO
#' V0011	ÁREA DE PONDERAÇÃO
#' V0010	PESO AMOSTRAL
#' V6531	RENDIMENTO DOMICILIAR (DOMICÍLIO PARTICULAR) PER CAPITA EM JULHO DE 2010
#' V0661	"RETORNA DO TRABALHO PARA CASA DIARIAMENTE:
#'       1- Sim
#'       2- Não
#'       Branco"
#' V0662	"QUAL É O TEMPO HABITUAL GASTO DE DESLOCAMENTO DE SUA CASA ATÉ O TRABALHO:
#'       1- Até 05 minutos
#'       2- De 06 minutos até meia hora
#'       3- Mais de meia hora até uma hora
#'       4- Mais de uma hora até duas horas
#'       5- Mais de duas horas
#'       Branco"
cols_to_read <- c('V0001', 'V0002', 'V0010', 'V0661', 'V0662')

censo <- fread('//storage6/bases2/NINSOC/Bases/Censo_Demografico/2010/CSV/censo_2010.pessoas.csv.bz2',
               # nrows = 5,
               select=cols_to_read
               #, colClasses = 'character'
               )


# check total population
sum(censo$V0010, na.rm=T)


###### recode census data ------------------

# fix muni code
censo[, code_muni := (V0001*100000) + V0002]
head(censo)

# only keep workers with daily commute
censo <- subset(censo, V0661 == 1)

# recode commute time
censo[, commute_time := fcase( V0662 == 1, 5,
                               V0662 == 2, 15,
                               V0662 == 3, 45,
                               V0662 == 4, 90,
                               V0662 == 5, 120)]

summary(censo$commute_time)


###### Bring info on urban concentration areas ------------------
urban_areas <- geobr::read_urban_concentrations()
setDT(urban_areas)

censo_urban <- subset(censo, code_muni %in% urban_areas$code_muni)

# merge
censo_urban[urban_areas, on='code_muni', code_urban_concentration := i.code_urban_concentration]
head(censo_urban)


###### calculate average travel time ------------------

df_commute <- censo_urban[, .(avg_commute_time = weighted.mean(x=commute_time, w=V0010),
                              commute_longer_60min = sum(V0010[which(commute_time >=60)]) / sum(V0010),
                              commute_longer_30min = sum(V0010[which(commute_time >=30)]) / sum(V0010)),
                          by= code_urban_concentration]

unique(df_commute$code_urban_concentration) %>% length()
head(df_commute)

summary(df_commute)

# save
readr::write_rds(df_commute, '../../data/urbanformbr/commute_time_censo2010/commute_time_censo2010.rds',compress = 'gz')






##### explore ------------------------------
df_commut <- readr::read_rds('../../data/urbanformbr/commute_time_censo2010/commute_time_censo2010.rds')
dens <- readr::read_rds('../../data/urbanformbr/density-experienced/density-experienced_urban-concentrations.rds')

# calculate total area
urban_areas <- geobr::read_urban_concentrations(simplified = F)
urban_areas$area_km2 <- as.numeric( st_area(urban_areas) / 1000000 )
area_total <- setDT(urban_areas)[, .(area_total_km2 = sum(area_km2)), by= code_urban_concentration]


# total Pop vs avg Density
setDT(dens)
df_densid <- dens[, .(pop_total = sum(POP),
         density10km2 = weighted.mean(x=pop_density10km2, w=POP),
         density05km2 = weighted.mean(x=pop_density05km2, w=POP),
         density01km2 = weighted.mean(x=pop_density01km2, w=POP)), by=code_urban_concentration ]

### merge
head(df_densid)
head(df_commut)
setDT(df_densid)
setDT(df_commut)

df <- merge(df_densid, df_commut, by='code_urban_concentration')
df <- merge(df, area_total, by='code_urban_concentration')
head(df)

# raw density
df[, density_total := pop_total  / area_total_km2]


####################### plot -----------------------------

# GET EQUATION AND R-SQUARED AS STRING
# SOURCE: https://stackoverflow.com/questions/7549694/add-regression-line-equation-and-r2-on-graph
my.formula <- log(y) ~ log(x)

ggplot(data=df, aes(x=density10km2, y=avg_commute_time)) +
  geom_point( alpha=.4) +
  scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')+
  ggpmisc::stat_poly_eq(formula = my.formula,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE)

ggplot(data=df, aes(x=density10km2, y=commute_longer_30min)) +
  geom_point( alpha=.4) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggpmisc::stat_poly_eq(formula = my.formula,
                        aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                        parse = TRUE)


summary(df$density_total)
summary(df$commute_longer_30min)

