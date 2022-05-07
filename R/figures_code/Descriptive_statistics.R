
library('data.table')
library('tidyverse')
library('vtable')

##### --- READ AND PREPARE DATA
df_UF <- fread("../../data/urbanformbr/consolidated_data/urbanformbr_metrics_full.csv", encoding = 'UTF-8')

## Essas aqui sao as variaveis q entraram no modelo.
var_modelo <- c("y_energy_per_capita","f_compact_contig_inter_dens","x_circuity_avg",
  "x_density_pop_02km_2014","x_land_use_mix",
  "x_normalized_closeness_centrality_avg","x_mean_fleet_age","x_mean_slope",
  "x_pop_2010","x_prop_dom_urban","x_prop_high_educ","x_prop_razao_dep",#"x_state",
  "x_total_pop_growth_1990_2014","x_wghtd_mean_household_income_per_capita")

# transform
df_UF$y_energy_per_capita <- df_UF$y_energy_per_capita*10000
df_UF$x_prop_dom_urban <- df_UF$x_prop_dom_urban*100
df_UF$x_prop_high_educ <- df_UF$x_prop_high_educ*100
df_UF$x_total_pop_growth_1990_2014 <- df_UF$x_total_pop_growth_1990_2014*100

df_UF_reg <- dplyr::select(df_UF, var_modelo)

colnames(df_UF_reg) <- c(
  "Energia per capita x 10000","Fator de contiguidade, compacidade e intersecção de vias",
 "Sinuosidade das vias","Densidade populacional experienciada 2km","Mix de uso do Solo",
  "Centralidade por proximidade","Idade Média da frota de veículos",
 "Declividade média da topografia","População total em 2010",
 "Percentual de domicílios urbanos (%)","Percentual de população com ensino superior (%)",
 "Razão de dependência demográfica",#"Dummy de estado",
 "Taxa média de crescimento populacional anual entre 1990 e 2015 (%)", "Renda média per capita (R$)")

#### PLOTTING DATA ----
DStat <- st(df_UF_reg,title = "Estatística descritiva",
   summ =  c('round(min(x),1)',
             'round(mean(x),1)',
             'round(max(x),1)',
             'round(sd(x),1)'),
   summ.names = c("Mínimo", "Média", "Máximo", "Desvio padrão"), out = "return")

DStat

#### SAVING DATA ----
writexl::write_xlsx(DStat, "./output/regression_output/descritptive_statiscs.xlsx")
