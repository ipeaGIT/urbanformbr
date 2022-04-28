
library('data.table')
library('tidyverse')
library('vtable')

##### --- READ AND PREPARE DATA
df_UF <- fread("df_fuel.csv",dec = ,)

## Essas aqui sao as variaveis q entraram no modelo.
var_modelo <- c("y_energy_per_capita","f_compact_contig_inter_dens","x_circuity_avg",
  "x_density_pop_02km_2014","x_land_use_mix",
  "x_normalized_closeness_centrality_avg","x_mean_fleet_age","x_mean_slope", 
  "x_pop_2010","x_prop_dom_urban","x_prop_high_educ","x_prop_razao_dep",#"x_state",
  "x_total_pop_growth_1990_2014","x_wghtd_mean_household_income_per_capita")

df_UF_reg <- dplyr::select(df_UF,var_modelo)

colnames(df_UF_reg) <- c(
  "Energia per capita","Fator de contiguidade, compacidade intersecção",
 "Sinuosidade das vias","Densidade populacional experienciada 2km","Uso do Solo",
  "Centralidade por proximidade","Idade Média da frota de veículos",
 "Declividade do solo","População Total em 2010",
 "Percentual de domicílios urbanos","Percentual de população com ensino superior",
 "Razão de dependência financeira",#"Dummy de estado",
 "Taxa de crescimento geométrico entre 1990 e 2015", "Renda média per capita")

#### PLOTTING DATA ----
DStat <- st(df_UF_reg,title = "Estatística descritiva",
   summ =  c('mean(x)',   'sd(x)', 'min(x)','max(x)'),summ.names = c("Média",
   "Desvio padrão","Mínimo","Máximo"))

#### SAVING DATA ----
write.csv(DStat, "Estatistica_Descritiva")
