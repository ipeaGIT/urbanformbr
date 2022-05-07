library(ggdag)
library(dagitty)
library(ggm)
library(fixest)
library(stargazer)


# dag model ----------------------------------
#' http://dagitty.net/dags.html?id=Uxpjl7

d <- dagitty('dag {
bb="0,0,1,1"
VKT [latent,pos="0.803,0.258"]
f_compact_contig_inter_dens [exposure,pos="0.477,0.036"]
trip_mode [latent,pos="0.760,0.579"]
x_circuity_avg [exposure,pos="0.494,0.213"]
x_density_pop_02km_2014 [exposure,pos="0.472,0.331"]
x_land_use_mix [exposure,pos="0.440,0.532"]
x_mean_fleet_age [pos="0.601,0.923"]
x_mean_slope [pos="0.354,0.268"]
x_normalized_closeness_centrality_avg [exposure,pos="0.488,0.123"]
x_pop_2010 [adjusted,pos="0.055,0.404"]
x_prop_autos_dom [pos="0.575,0.471"]
x_prop_dom_urban [pos="0.343,0.438"]
x_prop_high_educ [pos="0.223,0.675"]
x_prop_razao_dep [pos="0.581,0.802"]
x_state [pos="0.139,0.539"]
x_street_pop [pos="0.528,0.645"]
x_total_pop_growth_1990_2014 [pos="0.126,0.052"]
x_wghtd_mean_household_income_per_capita [pos="0.215,0.879"]
y_energy_per_capita [outcome,pos="0.918,0.429"]
VKT -> y_energy_per_capita
f_compact_contig_inter_dens -> VKT
trip_mode -> y_energy_per_capita
x_circuity_avg -> VKT
x_density_pop_02km_2014 -> VKT
x_density_pop_02km_2014 -> trip_mode
x_density_pop_02km_2014 -> x_land_use_mix
x_density_pop_02km_2014 -> x_prop_autos_dom
x_density_pop_02km_2014 -> x_street_pop
x_land_use_mix -> VKT
x_land_use_mix -> trip_mode
x_land_use_mix -> x_prop_autos_dom
x_mean_fleet_age -> y_energy_per_capita
x_mean_slope -> x_circuity_avg
x_mean_slope -> x_prop_autos_dom
x_mean_slope -> y_energy_per_capita
x_normalized_closeness_centrality_avg -> VKT
x_pop_2010 -> f_compact_contig_inter_dens
x_pop_2010 -> x_density_pop_02km_2014
x_pop_2010 -> x_land_use_mix
x_pop_2010 -> x_normalized_closeness_centrality_avg
x_pop_2010 -> x_prop_dom_urban
x_pop_2010 -> x_street_pop
x_pop_2010 -> x_wghtd_mean_household_income_per_capita
x_prop_autos_dom -> VKT
x_prop_autos_dom -> trip_mode
x_prop_autos_dom -> x_street_pop
x_prop_dom_urban -> VKT
x_prop_dom_urban -> trip_mode
x_prop_dom_urban -> x_density_pop_02km_2014
x_prop_dom_urban -> x_prop_high_educ
x_prop_high_educ -> x_prop_razao_dep
x_prop_razao_dep -> VKT
x_prop_razao_dep -> trip_mode
x_prop_razao_dep -> x_prop_autos_dom
x_state -> x_mean_slope
x_state -> x_prop_dom_urban
x_state -> x_prop_high_educ
x_state -> x_wghtd_mean_household_income_per_capita
x_street_pop -> VKT
x_street_pop -> trip_mode
x_total_pop_growth_1990_2014 -> f_compact_contig_inter_dens
x_total_pop_growth_1990_2014 -> x_normalized_closeness_centrality_avg
x_total_pop_growth_1990_2014 -> x_pop_2010
x_total_pop_growth_1990_2014 -> x_prop_dom_urban
x_wghtd_mean_household_income_per_capita -> trip_mode
x_wghtd_mean_household_income_per_capita -> x_mean_fleet_age
x_wghtd_mean_household_income_per_capita -> x_prop_autos_dom
x_wghtd_mean_household_income_per_capita -> x_street_pop
}')


#### test Implications ---------------------
test <- dagitty::localTests(x = d, data = cov( df_fuel ))
setDT(test)[p.value > 0.01] %>% nrow() / nrow(test)
setDT(test)[p.value > 0.05] %>% nrow() / nrow(test)
setDT(test)[p.value > 0.10] %>% nrow() / nrow(test)

testImplications <- function( covariance.matrix, sample.size ){
  library(ggm)
  tst <- function(i){ pcor.test( pcor(i,covariance.matrix), length(i)-2, sample.size )$pvalue }
  tos <- function(i){ paste(i,collapse=" ") }
  implications <- list(c("f_compact_contig_inter_dens","x_circuity_avg"),
                       c("f_compact_contig_inter_dens","x_density_pop_02km_2014","x_pop_2010","x_prop_dom_urban"),
                       c("f_compact_contig_inter_dens","x_density_pop_02km_2014","x_total_pop_growth_1990_2014","x_pop_2010"),
                       c("f_compact_contig_inter_dens","x_land_use_mix","x_density_pop_02km_2014","x_pop_2010"),
                       c("f_compact_contig_inter_dens","x_land_use_mix","x_pop_2010","x_prop_dom_urban"),
                       c("f_compact_contig_inter_dens","x_land_use_mix","x_total_pop_growth_1990_2014","x_pop_2010"),
                       c("f_compact_contig_inter_dens","x_mean_fleet_age","x_wghtd_mean_household_income_per_capita"),
                       c("f_compact_contig_inter_dens","x_mean_fleet_age","x_pop_2010"),
                       c("f_compact_contig_inter_dens","x_mean_slope"),
                       c("f_compact_contig_inter_dens","x_normalized_closeness_centrality_avg","x_pop_2010","x_total_pop_growth_1990_2014"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_density_pop_02km_2014","x_land_use_mix","x_mean_slope","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_land_use_mix","x_mean_slope","x_prop_high_educ"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_land_use_mix","x_state"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_land_use_mix","x_prop_high_educ","x_state"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_prop_dom_urban","x_state","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_land_use_mix"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_mean_slope","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_pop_2010"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_mean_slope","x_prop_high_educ","x_pop_2010"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_density_pop_02km_2014","x_prop_razao_dep","x_pop_2010","x_state"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_density_pop_02km_2014","x_pop_2010","x_state","x_prop_high_educ"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_mean_slope","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_prop_dom_urban"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_mean_slope","x_prop_high_educ","x_pop_2010","x_prop_dom_urban"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_prop_dom_urban","x_state","x_pop_2010"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_pop_2010","x_total_pop_growth_1990_2014"),
                       c("f_compact_contig_inter_dens","x_prop_dom_urban","x_total_pop_growth_1990_2014","x_pop_2010"),
                       c("f_compact_contig_inter_dens","x_prop_high_educ","x_prop_dom_urban","x_state"),
                       c("f_compact_contig_inter_dens","x_prop_high_educ","x_total_pop_growth_1990_2014","x_pop_2010"),
                       c("f_compact_contig_inter_dens","x_prop_razao_dep","x_prop_high_educ"),
                       c("f_compact_contig_inter_dens","x_prop_razao_dep","x_prop_dom_urban","x_state"),
                       c("f_compact_contig_inter_dens","x_prop_razao_dep","x_total_pop_growth_1990_2014","x_pop_2010"),
                       c("f_compact_contig_inter_dens","x_state"),
                       c("f_compact_contig_inter_dens","x_street_pop","x_density_pop_02km_2014","x_pop_2010","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita"),
                       c("f_compact_contig_inter_dens","x_street_pop","x_mean_slope","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_pop_2010"),
                       c("f_compact_contig_inter_dens","x_street_pop","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_mean_slope","x_prop_high_educ","x_pop_2010"),
                       c("f_compact_contig_inter_dens","x_street_pop","x_prop_razao_dep","x_density_pop_02km_2014","x_state","x_pop_2010"),
                       c("f_compact_contig_inter_dens","x_street_pop","x_density_pop_02km_2014","x_prop_high_educ","x_state","x_pop_2010"),
                       c("f_compact_contig_inter_dens","x_street_pop","x_mean_slope","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_prop_dom_urban"),
                       c("f_compact_contig_inter_dens","x_street_pop","x_wghtd_mean_household_income_per_capita","x_mean_slope","x_prop_high_educ","x_pop_2010","x_prop_dom_urban"),
                       c("f_compact_contig_inter_dens","x_street_pop","x_prop_dom_urban","x_state","x_pop_2010"),
                       c("f_compact_contig_inter_dens","x_street_pop","x_pop_2010","x_total_pop_growth_1990_2014"),
                       c("f_compact_contig_inter_dens","x_wghtd_mean_household_income_per_capita","x_pop_2010"),
                       c("x_circuity_avg","x_density_pop_02km_2014","x_pop_2010","x_prop_dom_urban"),
                       c("x_circuity_avg","x_density_pop_02km_2014","x_state"),
                       c("x_circuity_avg","x_density_pop_02km_2014","x_mean_slope"),
                       c("x_circuity_avg","x_land_use_mix","x_density_pop_02km_2014","x_pop_2010"),
                       c("x_circuity_avg","x_land_use_mix","x_pop_2010","x_prop_dom_urban"),
                       c("x_circuity_avg","x_land_use_mix","x_state"),
                       c("x_circuity_avg","x_land_use_mix","x_mean_slope"),
                       c("x_circuity_avg","x_mean_fleet_age","x_wghtd_mean_household_income_per_capita"),
                       c("x_circuity_avg","x_mean_fleet_age","x_state"),
                       c("x_circuity_avg","x_mean_fleet_age","x_mean_slope"),
                       c("x_circuity_avg","x_normalized_closeness_centrality_avg"),
                       c("x_circuity_avg","x_pop_2010"),
                       c("x_circuity_avg","x_prop_autos_dom","x_mean_slope"),
                       c("x_circuity_avg","x_prop_dom_urban","x_state"),
                       c("x_circuity_avg","x_prop_dom_urban","x_mean_slope"),
                       c("x_circuity_avg","x_prop_high_educ","x_state"),
                       c("x_circuity_avg","x_prop_high_educ","x_mean_slope"),
                       c("x_circuity_avg","x_prop_razao_dep","x_prop_high_educ"),
                       c("x_circuity_avg","x_prop_razao_dep","x_state"),
                       c("x_circuity_avg","x_prop_razao_dep","x_mean_slope"),
                       c("x_circuity_avg","x_state","x_mean_slope"),
                       c("x_circuity_avg","x_street_pop","x_density_pop_02km_2014","x_pop_2010","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita"),
                       c("x_circuity_avg","x_street_pop","x_prop_dom_urban","x_state","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_land_use_mix","x_density_pop_02km_2014"),
                       c("x_circuity_avg","x_street_pop","x_state","x_prop_high_educ","x_density_pop_02km_2014","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_land_use_mix"),
                       c("x_circuity_avg","x_street_pop","x_prop_razao_dep","x_state","x_density_pop_02km_2014","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_land_use_mix"),
                       c("x_circuity_avg","x_street_pop","x_mean_slope"),
                       c("x_circuity_avg","x_total_pop_growth_1990_2014"),
                       c("x_circuity_avg","x_wghtd_mean_household_income_per_capita","x_state"),
                       c("x_circuity_avg","x_wghtd_mean_household_income_per_capita","x_mean_slope"),
                       c("x_density_pop_02km_2014","x_mean_fleet_age","x_wghtd_mean_household_income_per_capita"),
                       c("x_density_pop_02km_2014","x_mean_fleet_age","x_pop_2010","x_state"),
                       c("x_density_pop_02km_2014","x_mean_fleet_age","x_pop_2010","x_prop_dom_urban"),
                       c("x_density_pop_02km_2014","x_mean_slope","x_state"),
                       c("x_density_pop_02km_2014","x_mean_slope","x_prop_dom_urban","x_pop_2010"),
                       c("x_density_pop_02km_2014","x_normalized_closeness_centrality_avg","x_pop_2010","x_total_pop_growth_1990_2014"),
                       c("x_density_pop_02km_2014","x_normalized_closeness_centrality_avg","x_prop_dom_urban","x_pop_2010"),
                       c("x_density_pop_02km_2014","x_prop_high_educ","x_prop_dom_urban","x_state"),
                       c("x_density_pop_02km_2014","x_prop_high_educ","x_prop_dom_urban","x_pop_2010"),
                       c("x_density_pop_02km_2014","x_prop_razao_dep","x_prop_high_educ"),
                       c("x_density_pop_02km_2014","x_prop_razao_dep","x_prop_dom_urban","x_state"),
                       c("x_density_pop_02km_2014","x_prop_razao_dep","x_prop_dom_urban","x_pop_2010"),
                       c("x_density_pop_02km_2014","x_state","x_prop_dom_urban","x_pop_2010"),
                       c("x_density_pop_02km_2014","x_total_pop_growth_1990_2014","x_prop_dom_urban","x_pop_2010"),
                       c("x_density_pop_02km_2014","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_state"),
                       c("x_density_pop_02km_2014","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_prop_dom_urban"),
                       c("x_land_use_mix","x_mean_fleet_age","x_wghtd_mean_household_income_per_capita"),
                       c("x_land_use_mix","x_mean_fleet_age","x_pop_2010","x_state"),
                       c("x_land_use_mix","x_mean_fleet_age","x_pop_2010","x_prop_dom_urban"),
                       c("x_land_use_mix","x_mean_fleet_age","x_density_pop_02km_2014","x_pop_2010"),
                       c("x_land_use_mix","x_mean_slope","x_state"),
                       c("x_land_use_mix","x_mean_slope","x_pop_2010","x_prop_dom_urban"),
                       c("x_land_use_mix","x_mean_slope","x_density_pop_02km_2014","x_pop_2010"),
                       c("x_land_use_mix","x_normalized_closeness_centrality_avg","x_pop_2010","x_total_pop_growth_1990_2014"),
                       c("x_land_use_mix","x_normalized_closeness_centrality_avg","x_prop_dom_urban","x_pop_2010"),
                       c("x_land_use_mix","x_normalized_closeness_centrality_avg","x_pop_2010","x_density_pop_02km_2014"),
                       c("x_land_use_mix","x_prop_dom_urban","x_pop_2010","x_density_pop_02km_2014"),
                       c("x_land_use_mix","x_prop_high_educ","x_prop_dom_urban","x_state"),
                       c("x_land_use_mix","x_prop_high_educ","x_prop_dom_urban","x_pop_2010"),
                       c("x_land_use_mix","x_prop_high_educ","x_pop_2010","x_density_pop_02km_2014"),
                       c("x_land_use_mix","x_prop_razao_dep","x_prop_high_educ"),
                       c("x_land_use_mix","x_prop_razao_dep","x_prop_dom_urban","x_state"),
                       c("x_land_use_mix","x_prop_razao_dep","x_prop_dom_urban","x_pop_2010"),
                       c("x_land_use_mix","x_prop_razao_dep","x_pop_2010","x_density_pop_02km_2014"),
                       c("x_land_use_mix","x_state","x_pop_2010","x_prop_dom_urban"),
                       c("x_land_use_mix","x_state","x_density_pop_02km_2014","x_pop_2010"),
                       c("x_land_use_mix","x_street_pop","x_density_pop_02km_2014","x_pop_2010","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita"),
                       c("x_land_use_mix","x_total_pop_growth_1990_2014","x_prop_dom_urban","x_pop_2010"),
                       c("x_land_use_mix","x_total_pop_growth_1990_2014","x_pop_2010","x_density_pop_02km_2014"),
                       c("x_land_use_mix","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_state"),
                       c("x_land_use_mix","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_prop_dom_urban"),
                       c("x_land_use_mix","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_pop_2010"),
                       c("x_mean_fleet_age","x_mean_slope","x_state"),
                       c("x_mean_fleet_age","x_mean_slope","x_wghtd_mean_household_income_per_capita"),
                       c("x_mean_fleet_age","x_normalized_closeness_centrality_avg","x_pop_2010"),
                       c("x_mean_fleet_age","x_normalized_closeness_centrality_avg","x_wghtd_mean_household_income_per_capita"),
                       c("x_mean_fleet_age","x_pop_2010","x_wghtd_mean_household_income_per_capita"),
                       c("x_mean_fleet_age","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita"),
                       c("x_mean_fleet_age","x_prop_dom_urban","x_pop_2010","x_state"),
                       c("x_mean_fleet_age","x_prop_dom_urban","x_wghtd_mean_household_income_per_capita"),
                       c("x_mean_fleet_age","x_prop_high_educ","x_prop_dom_urban","x_state"),
                       c("x_mean_fleet_age","x_prop_high_educ","x_pop_2010","x_state"),
                       c("x_mean_fleet_age","x_prop_high_educ","x_wghtd_mean_household_income_per_capita"),
                       c("x_mean_fleet_age","x_prop_razao_dep","x_prop_high_educ"),
                       c("x_mean_fleet_age","x_prop_razao_dep","x_prop_dom_urban","x_state"),
                       c("x_mean_fleet_age","x_prop_razao_dep","x_pop_2010","x_state"),
                       c("x_mean_fleet_age","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita"),
                       c("x_mean_fleet_age","x_state","x_wghtd_mean_household_income_per_capita"),
                       c("x_mean_fleet_age","x_street_pop","x_wghtd_mean_household_income_per_capita"),
                       c("x_mean_fleet_age","x_total_pop_growth_1990_2014","x_pop_2010"),
                       c("x_mean_fleet_age","x_total_pop_growth_1990_2014","x_wghtd_mean_household_income_per_capita"),
                       c("x_mean_slope","x_normalized_closeness_centrality_avg"),
                       c("x_mean_slope","x_pop_2010"),
                       c("x_mean_slope","x_prop_dom_urban","x_state"),
                       c("x_mean_slope","x_prop_high_educ","x_state"),
                       c("x_mean_slope","x_prop_razao_dep","x_prop_high_educ"),
                       c("x_mean_slope","x_prop_razao_dep","x_state"),
                       c("x_mean_slope","x_street_pop","x_density_pop_02km_2014","x_pop_2010","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita"),
                       c("x_mean_slope","x_street_pop","x_prop_dom_urban","x_state","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_land_use_mix","x_density_pop_02km_2014"),
                       c("x_mean_slope","x_street_pop","x_state","x_prop_high_educ","x_density_pop_02km_2014","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_land_use_mix"),
                       c("x_mean_slope","x_street_pop","x_prop_razao_dep","x_state","x_density_pop_02km_2014","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_land_use_mix"),
                       c("x_mean_slope","x_total_pop_growth_1990_2014"),
                       c("x_mean_slope","x_wghtd_mean_household_income_per_capita","x_state"),
                       c("x_normalized_closeness_centrality_avg","x_prop_autos_dom","x_density_pop_02km_2014","x_land_use_mix","x_mean_slope","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita"),
                       c("x_normalized_closeness_centrality_avg","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_land_use_mix","x_mean_slope","x_prop_high_educ"),
                       c("x_normalized_closeness_centrality_avg","x_prop_autos_dom","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_land_use_mix","x_state"),
                       c("x_normalized_closeness_centrality_avg","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_land_use_mix","x_prop_high_educ","x_state"),
                       c("x_normalized_closeness_centrality_avg","x_prop_autos_dom","x_prop_dom_urban","x_state","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_land_use_mix"),
                       c("x_normalized_closeness_centrality_avg","x_prop_autos_dom","x_mean_slope","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_pop_2010"),
                       c("x_normalized_closeness_centrality_avg","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_mean_slope","x_prop_high_educ","x_pop_2010"),
                       c("x_normalized_closeness_centrality_avg","x_prop_autos_dom","x_density_pop_02km_2014","x_prop_razao_dep","x_pop_2010","x_state"),
                       c("x_normalized_closeness_centrality_avg","x_prop_autos_dom","x_density_pop_02km_2014","x_pop_2010","x_state","x_prop_high_educ"),
                       c("x_normalized_closeness_centrality_avg","x_prop_autos_dom","x_mean_slope","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_prop_dom_urban"),
                       c("x_normalized_closeness_centrality_avg","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_mean_slope","x_prop_high_educ","x_pop_2010","x_prop_dom_urban"),
                       c("x_normalized_closeness_centrality_avg","x_prop_autos_dom","x_prop_dom_urban","x_state","x_pop_2010"),
                       c("x_normalized_closeness_centrality_avg","x_prop_autos_dom","x_pop_2010","x_total_pop_growth_1990_2014"),
                       c("x_normalized_closeness_centrality_avg","x_prop_dom_urban","x_total_pop_growth_1990_2014","x_pop_2010"),
                       c("x_normalized_closeness_centrality_avg","x_prop_high_educ","x_prop_dom_urban","x_state"),
                       c("x_normalized_closeness_centrality_avg","x_prop_high_educ","x_total_pop_growth_1990_2014","x_pop_2010"),
                       c("x_normalized_closeness_centrality_avg","x_prop_razao_dep","x_prop_high_educ"),
                       c("x_normalized_closeness_centrality_avg","x_prop_razao_dep","x_prop_dom_urban","x_state"),
                       c("x_normalized_closeness_centrality_avg","x_prop_razao_dep","x_total_pop_growth_1990_2014","x_pop_2010"),
                       c("x_normalized_closeness_centrality_avg","x_state"),
                       c("x_normalized_closeness_centrality_avg","x_street_pop","x_density_pop_02km_2014","x_pop_2010","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita"),
                       c("x_normalized_closeness_centrality_avg","x_street_pop","x_mean_slope","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_pop_2010"),
                       c("x_normalized_closeness_centrality_avg","x_street_pop","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_mean_slope","x_prop_high_educ","x_pop_2010"),
                       c("x_normalized_closeness_centrality_avg","x_street_pop","x_prop_razao_dep","x_density_pop_02km_2014","x_state","x_pop_2010"),
                       c("x_normalized_closeness_centrality_avg","x_street_pop","x_density_pop_02km_2014","x_prop_high_educ","x_state","x_pop_2010"),
                       c("x_normalized_closeness_centrality_avg","x_street_pop","x_mean_slope","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_prop_dom_urban"),
                       c("x_normalized_closeness_centrality_avg","x_street_pop","x_wghtd_mean_household_income_per_capita","x_mean_slope","x_prop_high_educ","x_pop_2010","x_prop_dom_urban"),
                       c("x_normalized_closeness_centrality_avg","x_street_pop","x_prop_dom_urban","x_state","x_pop_2010"),
                       c("x_normalized_closeness_centrality_avg","x_street_pop","x_pop_2010","x_total_pop_growth_1990_2014"),
                       c("x_normalized_closeness_centrality_avg","x_wghtd_mean_household_income_per_capita","x_pop_2010"),
                       c("x_pop_2010","x_prop_autos_dom","x_density_pop_02km_2014","x_land_use_mix","x_mean_slope","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita"),
                       c("x_pop_2010","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_land_use_mix","x_mean_slope","x_prop_high_educ"),
                       c("x_pop_2010","x_prop_autos_dom","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_land_use_mix","x_state"),
                       c("x_pop_2010","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_land_use_mix","x_prop_high_educ","x_state"),
                       c("x_pop_2010","x_prop_autos_dom","x_prop_dom_urban","x_state","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_land_use_mix"),
                       c("x_pop_2010","x_prop_high_educ","x_prop_dom_urban","x_state"),
                       c("x_pop_2010","x_prop_razao_dep","x_prop_high_educ"),
                       c("x_pop_2010","x_prop_razao_dep","x_prop_dom_urban","x_state"),
                       c("x_pop_2010","x_state"),
                       c("x_pop_2010","y_energy_per_capita","x_density_pop_02km_2014","x_land_use_mix","x_normalized_closeness_centrality_avg","x_prop_autos_dom","x_prop_dom_urban","x_prop_razao_dep","x_street_pop","f_compact_contig_inter_dens","x_mean_slope","x_wghtd_mean_household_income_per_capita"),
                       c("x_pop_2010","y_energy_per_capita","x_density_pop_02km_2014","x_land_use_mix","x_normalized_closeness_centrality_avg","x_prop_autos_dom","x_prop_dom_urban","x_street_pop","f_compact_contig_inter_dens","x_mean_slope","x_wghtd_mean_household_income_per_capita","x_prop_high_educ"),
                       c("x_pop_2010","y_energy_per_capita","x_prop_dom_urban","x_state","x_density_pop_02km_2014","x_land_use_mix","x_normalized_closeness_centrality_avg","x_prop_autos_dom","x_street_pop","f_compact_contig_inter_dens","x_wghtd_mean_household_income_per_capita"),
                       c("x_prop_autos_dom","x_prop_dom_urban","x_pop_2010","x_state","x_prop_high_educ","x_density_pop_02km_2014"),
                       c("x_prop_autos_dom","x_prop_dom_urban","x_pop_2010","x_state","x_prop_razao_dep","x_density_pop_02km_2014"),
                       c("x_prop_autos_dom","x_prop_dom_urban","x_pop_2010","x_wghtd_mean_household_income_per_capita","x_prop_high_educ","x_mean_slope","x_density_pop_02km_2014"),
                       c("x_prop_autos_dom","x_prop_dom_urban","x_pop_2010","x_wghtd_mean_household_income_per_capita","x_mean_slope","x_prop_razao_dep","x_density_pop_02km_2014"),
                       c("x_prop_autos_dom","x_prop_dom_urban","x_state","x_wghtd_mean_household_income_per_capita","x_land_use_mix","x_density_pop_02km_2014","x_prop_high_educ"),
                       c("x_prop_autos_dom","x_prop_dom_urban","x_state","x_wghtd_mean_household_income_per_capita","x_land_use_mix","x_density_pop_02km_2014","x_prop_razao_dep"),
                       c("x_prop_autos_dom","x_prop_dom_urban","x_wghtd_mean_household_income_per_capita","x_prop_high_educ","x_mean_slope","x_land_use_mix","x_density_pop_02km_2014"),
                       c("x_prop_autos_dom","x_prop_dom_urban","x_wghtd_mean_household_income_per_capita","x_mean_slope","x_land_use_mix","x_density_pop_02km_2014","x_prop_razao_dep"),
                       c("x_prop_autos_dom","x_prop_high_educ","x_prop_razao_dep","x_prop_dom_urban","x_state"),
                       c("x_prop_autos_dom","x_prop_high_educ","x_prop_dom_urban","x_pop_2010","x_wghtd_mean_household_income_per_capita","x_mean_slope","x_prop_razao_dep"),
                       c("x_prop_autos_dom","x_prop_high_educ","x_pop_2010","x_state","x_density_pop_02km_2014","x_prop_razao_dep"),
                       c("x_prop_autos_dom","x_prop_high_educ","x_pop_2010","x_wghtd_mean_household_income_per_capita","x_mean_slope","x_density_pop_02km_2014","x_prop_razao_dep"),
                       c("x_prop_autos_dom","x_prop_high_educ","x_state","x_wghtd_mean_household_income_per_capita","x_land_use_mix","x_density_pop_02km_2014","x_prop_razao_dep"),
                       c("x_prop_autos_dom","x_prop_high_educ","x_wghtd_mean_household_income_per_capita","x_land_use_mix","x_density_pop_02km_2014","x_mean_slope","x_prop_razao_dep"),
                       c("x_prop_autos_dom","x_state","x_pop_2010","x_prop_dom_urban","x_wghtd_mean_household_income_per_capita","x_prop_high_educ","x_mean_slope"),
                       c("x_prop_autos_dom","x_state","x_pop_2010","x_prop_dom_urban","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_mean_slope"),
                       c("x_prop_autos_dom","x_state","x_prop_high_educ","x_density_pop_02km_2014","x_pop_2010","x_wghtd_mean_household_income_per_capita","x_mean_slope"),
                       c("x_prop_autos_dom","x_state","x_density_pop_02km_2014","x_pop_2010","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_mean_slope"),
                       c("x_prop_autos_dom","x_state","x_prop_high_educ","x_density_pop_02km_2014","x_wghtd_mean_household_income_per_capita","x_land_use_mix","x_mean_slope"),
                       c("x_prop_autos_dom","x_state","x_density_pop_02km_2014","x_wghtd_mean_household_income_per_capita","x_land_use_mix","x_prop_razao_dep","x_mean_slope"),
                       c("x_prop_autos_dom","x_total_pop_growth_1990_2014","x_pop_2010","x_prop_dom_urban","x_state"),
                       c("x_prop_autos_dom","x_total_pop_growth_1990_2014","x_wghtd_mean_household_income_per_capita","x_prop_high_educ","x_mean_slope","x_pop_2010","x_prop_dom_urban"),
                       c("x_prop_autos_dom","x_total_pop_growth_1990_2014","x_prop_razao_dep","x_prop_dom_urban","x_wghtd_mean_household_income_per_capita","x_mean_slope","x_pop_2010"),
                       c("x_prop_autos_dom","x_total_pop_growth_1990_2014","x_state","x_prop_high_educ","x_density_pop_02km_2014","x_pop_2010"),
                       c("x_prop_autos_dom","x_total_pop_growth_1990_2014","x_prop_razao_dep","x_state","x_density_pop_02km_2014","x_pop_2010"),
                       c("x_prop_autos_dom","x_total_pop_growth_1990_2014","x_wghtd_mean_household_income_per_capita","x_prop_high_educ","x_mean_slope","x_pop_2010","x_density_pop_02km_2014"),
                       c("x_prop_autos_dom","x_total_pop_growth_1990_2014","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_mean_slope","x_pop_2010","x_density_pop_02km_2014"),
                       c("x_prop_autos_dom","x_total_pop_growth_1990_2014","x_state","x_wghtd_mean_household_income_per_capita","x_prop_dom_urban","x_land_use_mix","x_density_pop_02km_2014"),
                       c("x_prop_autos_dom","x_total_pop_growth_1990_2014","x_state","x_prop_high_educ","x_density_pop_02km_2014","x_wghtd_mean_household_income_per_capita","x_land_use_mix"),
                       c("x_prop_autos_dom","x_total_pop_growth_1990_2014","x_prop_razao_dep","x_state","x_density_pop_02km_2014","x_wghtd_mean_household_income_per_capita","x_land_use_mix"),
                       c("x_prop_autos_dom","x_total_pop_growth_1990_2014","x_wghtd_mean_household_income_per_capita","x_prop_high_educ","x_mean_slope","x_density_pop_02km_2014","x_land_use_mix"),
                       c("x_prop_autos_dom","x_total_pop_growth_1990_2014","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_mean_slope","x_density_pop_02km_2014","x_land_use_mix"),
                       c("x_prop_dom_urban","x_prop_razao_dep","x_prop_high_educ"),
                       c("x_prop_dom_urban","x_street_pop","x_density_pop_02km_2014","x_pop_2010","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita"),
                       c("x_prop_dom_urban","x_street_pop","x_mean_slope","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_pop_2010"),
                       c("x_prop_dom_urban","x_street_pop","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_mean_slope","x_prop_high_educ","x_pop_2010"),
                       c("x_prop_dom_urban","x_street_pop","x_prop_razao_dep","x_density_pop_02km_2014","x_state","x_pop_2010"),
                       c("x_prop_dom_urban","x_street_pop","x_density_pop_02km_2014","x_prop_high_educ","x_state","x_pop_2010"),
                       c("x_prop_dom_urban","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_state"),
                       c("x_prop_high_educ","x_street_pop","x_density_pop_02km_2014","x_pop_2010","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita"),
                       c("x_prop_high_educ","x_street_pop","x_prop_dom_urban","x_state","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_land_use_mix","x_density_pop_02km_2014"),
                       c("x_prop_high_educ","x_street_pop","x_mean_slope","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_pop_2010"),
                       c("x_prop_high_educ","x_street_pop","x_prop_razao_dep","x_density_pop_02km_2014","x_state","x_pop_2010"),
                       c("x_prop_high_educ","x_street_pop","x_mean_slope","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_prop_dom_urban"),
                       c("x_prop_high_educ","x_street_pop","x_prop_dom_urban","x_state","x_prop_razao_dep"),
                       c("x_prop_high_educ","x_total_pop_growth_1990_2014","x_state","x_prop_dom_urban"),
                       c("x_prop_high_educ","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_state"),
                       c("x_prop_high_educ","x_wghtd_mean_household_income_per_capita","x_prop_dom_urban","x_state"),
                       c("x_prop_high_educ","y_energy_per_capita","x_density_pop_02km_2014","x_land_use_mix","x_normalized_closeness_centrality_avg","x_prop_autos_dom","x_prop_dom_urban","x_prop_razao_dep","x_street_pop","f_compact_contig_inter_dens","x_mean_slope","x_wghtd_mean_household_income_per_capita"),
                       c("x_prop_high_educ","y_energy_per_capita","x_normalized_closeness_centrality_avg","x_prop_dom_urban","x_prop_razao_dep","f_compact_contig_inter_dens","x_mean_slope","x_wghtd_mean_household_income_per_capita","x_pop_2010"),
                       c("x_prop_high_educ","y_energy_per_capita","x_prop_dom_urban","x_prop_razao_dep","x_pop_2010","x_total_pop_growth_1990_2014","x_mean_slope","x_wghtd_mean_household_income_per_capita"),
                       c("x_prop_high_educ","y_energy_per_capita","x_prop_dom_urban","x_state","x_prop_razao_dep"),
                       c("x_prop_razao_dep","x_state","x_prop_high_educ"),
                       c("x_prop_razao_dep","x_street_pop","x_density_pop_02km_2014","x_pop_2010","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita"),
                       c("x_prop_razao_dep","x_street_pop","x_prop_dom_urban","x_state","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_land_use_mix","x_density_pop_02km_2014"),
                       c("x_prop_razao_dep","x_street_pop","x_state","x_density_pop_02km_2014","x_prop_high_educ","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_land_use_mix"),
                       c("x_prop_razao_dep","x_street_pop","x_density_pop_02km_2014","x_prop_high_educ","x_wghtd_mean_household_income_per_capita","x_mean_slope","x_prop_autos_dom","x_land_use_mix"),
                       c("x_prop_razao_dep","x_total_pop_growth_1990_2014","x_state","x_prop_dom_urban"),
                       c("x_prop_razao_dep","x_total_pop_growth_1990_2014","x_prop_high_educ"),
                       c("x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_state"),
                       c("x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_prop_dom_urban","x_state"),
                       c("x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_prop_high_educ"),
                       c("x_state","x_street_pop","x_density_pop_02km_2014","x_pop_2010","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita"),
                       c("x_state","x_street_pop","x_mean_slope","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_pop_2010"),
                       c("x_state","x_street_pop","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_mean_slope","x_prop_high_educ","x_pop_2010"),
                       c("x_state","x_street_pop","x_mean_slope","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_prop_dom_urban"),
                       c("x_state","x_street_pop","x_wghtd_mean_household_income_per_capita","x_mean_slope","x_prop_high_educ","x_pop_2010","x_prop_dom_urban"),
                       c("x_state","x_total_pop_growth_1990_2014"),
                       c("x_state","y_energy_per_capita","x_density_pop_02km_2014","x_land_use_mix","x_normalized_closeness_centrality_avg","x_prop_autos_dom","x_prop_dom_urban","x_prop_razao_dep","x_street_pop","f_compact_contig_inter_dens","x_mean_slope","x_wghtd_mean_household_income_per_capita"),
                       c("x_state","y_energy_per_capita","x_normalized_closeness_centrality_avg","x_prop_dom_urban","x_prop_razao_dep","f_compact_contig_inter_dens","x_mean_slope","x_wghtd_mean_household_income_per_capita","x_pop_2010"),
                       c("x_state","y_energy_per_capita","x_prop_dom_urban","x_prop_razao_dep","x_pop_2010","x_total_pop_growth_1990_2014","x_mean_slope","x_wghtd_mean_household_income_per_capita"),
                       c("x_state","y_energy_per_capita","x_density_pop_02km_2014","x_land_use_mix","x_normalized_closeness_centrality_avg","x_prop_autos_dom","x_prop_dom_urban","x_street_pop","f_compact_contig_inter_dens","x_mean_slope","x_wghtd_mean_household_income_per_capita","x_prop_high_educ"),
                       c("x_state","y_energy_per_capita","x_normalized_closeness_centrality_avg","x_prop_dom_urban","f_compact_contig_inter_dens","x_mean_slope","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_prop_high_educ"),
                       c("x_state","y_energy_per_capita","x_prop_dom_urban","x_pop_2010","x_total_pop_growth_1990_2014","x_mean_slope","x_wghtd_mean_household_income_per_capita","x_prop_high_educ"),
                       c("x_street_pop","x_total_pop_growth_1990_2014","x_prop_dom_urban","x_pop_2010","x_state"),
                       c("x_street_pop","x_total_pop_growth_1990_2014","x_wghtd_mean_household_income_per_capita","x_prop_high_educ","x_mean_slope","x_pop_2010","x_prop_dom_urban"),
                       c("x_street_pop","x_total_pop_growth_1990_2014","x_prop_razao_dep","x_prop_dom_urban","x_wghtd_mean_household_income_per_capita","x_mean_slope","x_pop_2010"),
                       c("x_street_pop","x_total_pop_growth_1990_2014","x_state","x_prop_high_educ","x_density_pop_02km_2014","x_pop_2010"),
                       c("x_street_pop","x_total_pop_growth_1990_2014","x_prop_razao_dep","x_state","x_density_pop_02km_2014","x_pop_2010"),
                       c("x_street_pop","x_total_pop_growth_1990_2014","x_wghtd_mean_household_income_per_capita","x_prop_high_educ","x_mean_slope","x_pop_2010","x_density_pop_02km_2014"),
                       c("x_street_pop","x_total_pop_growth_1990_2014","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_mean_slope","x_pop_2010","x_density_pop_02km_2014"),
                       c("x_street_pop","x_total_pop_growth_1990_2014","x_wghtd_mean_household_income_per_capita","x_prop_autos_dom","x_density_pop_02km_2014","x_pop_2010"),
                       c("x_total_pop_growth_1990_2014","x_wghtd_mean_household_income_per_capita","x_pop_2010"),
                       c("x_total_pop_growth_1990_2014","y_energy_per_capita","x_density_pop_02km_2014","x_land_use_mix","x_normalized_closeness_centrality_avg","x_prop_autos_dom","x_prop_dom_urban","x_prop_razao_dep","x_street_pop","f_compact_contig_inter_dens","x_mean_slope","x_wghtd_mean_household_income_per_capita"),
                       c("x_total_pop_growth_1990_2014","y_energy_per_capita","x_density_pop_02km_2014","x_land_use_mix","x_normalized_closeness_centrality_avg","x_prop_autos_dom","x_prop_dom_urban","x_street_pop","f_compact_contig_inter_dens","x_mean_slope","x_wghtd_mean_household_income_per_capita","x_prop_high_educ"),
                       c("x_total_pop_growth_1990_2014","y_energy_per_capita","x_prop_dom_urban","x_state","x_density_pop_02km_2014","x_land_use_mix","x_normalized_closeness_centrality_avg","x_prop_autos_dom","x_street_pop","f_compact_contig_inter_dens","x_wghtd_mean_household_income_per_capita"),
                       c("x_total_pop_growth_1990_2014","y_energy_per_capita","x_normalized_closeness_centrality_avg","x_prop_dom_urban","x_prop_razao_dep","f_compact_contig_inter_dens","x_mean_slope","x_wghtd_mean_household_income_per_capita","x_pop_2010"),
                       c("x_total_pop_growth_1990_2014","y_energy_per_capita","x_normalized_closeness_centrality_avg","x_prop_dom_urban","f_compact_contig_inter_dens","x_mean_slope","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_prop_high_educ"),
                       c("x_total_pop_growth_1990_2014","y_energy_per_capita","x_prop_dom_urban","x_state","x_pop_2010","f_compact_contig_inter_dens","x_normalized_closeness_centrality_avg"))
  data.frame( implication=unlist(lapply(implications,tos)),
              pvalue=unlist( lapply( implications, tst ) ) )

}

test <- testImplications( covariance.matrix = cov( df_fuel ),
                          sample.size = nrow(df_fuel))

setDT(test)[pvalue > 0.05] %>% nrow() / nrow(test)
#' cor(df_fuel$f_compact_contig_inter_dens , df_fuel$x_street_pop )
#' cor(df_fuel$x_pop_2010  , df_fuel$x_density_pop_02km_2014)
cor(df_fuel$x_pop_2010  , df_fuel$x_normalized_closeness_centrality_avg)
cor(df_fuel$x_circuity_avg  , df_fuel$x_normalized_closeness_centrality_avg)
cor(df_fuel$x_mean_slope  , df_fuel$x_prop_autos_dom)

cor(df_fuel$x_prop_autos_dom  , df_fuel$x_density_pop_02km_2014)
cor(df_fuel$x_prop_autos_dom  , df_fuel$x_normalized_closeness_centrality_avg)

cor(df_fuel$x_normalized_closeness_centrality_avg  , df_fuel$x_prop_autos_dom)

aaaaaa <- subset(test, implication %like% 'x_prop_autos_dom')
aaaaaa <- subset(aaaaaa, implication %like% 'x_normalized_closeness_centrality_avg')




  # vars mais repetidas
  implication <- setDT(test)[pvalue < 0.1]$implication

  aaa <- test[implication %like% 'street_pop']

  all_vars <- str_split(implication, pattern = ' ')
  all_vars <- unlist(all_vars)
  table(all_vars) %>% sort()

  ggplot(data=df_fuel, aes(y=y_energy_per_capita, x=x_wghtd_mean_household_income_per_capita)) +
    geom_point() +
    geom_smooth() +
    theme_classic()

  ggplot(data=df_fuel, aes(x=y_energy_per_capita)) +
    geom_density()



#### DAG Figure ---------------------
#
# # convert DAG to tidy df
# dag_tidy <- tidy_dagitty(d)
#
# # label of variables
# vars_urban_form <- c('f_compact_contig_inter_dens', 'x_circuity_avg', 'x_density_pop_02km_2014', 'x_land_use_mix', 'x_normalized_closeness_centrality_avg', 'I(x_pop_2010 * f_compact_contig_inter_dens)')
# setDT(dag_tidy$data)
# dag_tidy$data[, var_type := fcase(name %in% 'y_energy_per_capita', 'outcome',
#                                     name %in% vars_urban_form, 'expousure')]
#
# # factor variables with lables
# dag_tidy$data$name <- factor(dag_tidy$data$name,
#                              levels= unique(dag_tidy$data$name),
#                              labels=c('VKT'
#                                     , 'f.compact_contg_interDenst'
#                                     , 'sinuosidade'
#                                     , 'densidade_pop'
#                                     , 'mix_uso_solo'
#                                     , 'idade_frota'
#                                     , 'declividade'
#                                     , 'closeness_centrl'
#                                     , 'pop_total'
#                                     , 'prop_dom_auto'
#                                     , 'prop_dom_urban'
#                                     , 'prop_alta_escol'
#                                     , 'pop_razao_dependnc'
#                                     , 'unidade_federacao'
#                                     , 'ruas_km_por_pop'
#                                     , 'pop_cresc_1990-2014'
#                                     , 'renda'
#                                     , 'energia_pct')
#                              )
#
#
# temp_fig <-ggplot(data = dag_tidy,
#                    aes( x = x, y = y, xend = xend, yend = yend, color=var_type), ) +
#               geom_dag_point(alpha=.7, size=10) +
#               geom_dag_edges( edge_colour='gray40', edge_width=.6, edge_alpha=.7,
#                               arrow_directed = grid::arrow(length = grid::unit(5, "pt"), type = "closed"),
#                               ) +
#               geom_dag_text(size=2.5, col = "black", nudge_y=-.06) +
#               theme_dag() +
#   theme(legend.position='none')
#
#
# ggsave(temp_fig, file='./figures/dag_simple.png', dpi=300,
#        width = 16, height = 12, units = 'cm')


#### determine adjustments (model specification) ---------------------

# function to generate model specifications
generate_model <- function(exposure_vars=NULL, type='canonical', interaction=FALSE){

  # control vars
  cols <- dagitty::adjustmentSets(d,
                                  exposure = exposure_vars,
                                  type = type, # 'minimal' 'canonical'
                                  effect = 'total')

  cols <- as.character(cols)
  cols <- gsub('"', '', cols, fixed=TRUE)
  cols <- gsub("^c\\(|\\)$", "", cols)
  cols <- gsub(",", " +", gsub("\\.", "", cols))

  # exposure vars
  cols_exp <- exposure_vars
  if( is.null(cols_exp)) { cols_exp <- dagitty::exposures(d) }
  cols_exp <- paste0(cols_exp, collapse = ' + ')

  # outcome var
  outc <- dagitty::outcomes(d)

  # model
  model <- paste0(outc, ' ~ ', cols_exp, '+', cols )

  # add interaction term if necessary
  if (interaction==TRUE){
  model <- lapply(X=model, FUN=function(mm){

                  if (mm %like% 'f_compact_contig_inter_dens' & mm %like% 'x_pop_2010') {
                              mm <- paste0(mm, '+ I(x_pop_2010*f_compact_contig_inter_dens)')
                            }
                    return(mm)}
                  )}

  return(model)
  }


# make sure state is categorical
df_log$x_state <- as.factor(df_log$x_state)
df_log$i_name_state <- as.factor(df_log$i_name_state)


# generate all model specifications
model_spec_all       <- generate_model(type='canonical', interaction=TRUE)
model_spec_landuse   <- generate_model(exposure_vars = 'x_land_use_mix', type='minimal', interaction=TRUE)
model_spec_density   <- generate_model(exposure_vars = 'x_density_pop_02km_2014', type='minimal', interaction=TRUE)
model_spec_fcompact  <- generate_model(exposure_vars = 'f_compact_contig_inter_dens', type='minimal', interaction=TRUE)
model_spec_circuity  <- generate_model(exposure_vars = 'x_circuity_avg', type='minimal', interaction=TRUE)
model_spec_closeness <- generate_model(exposure_vars = 'x_normalized_closeness_centrality_avg', type='minimal', interaction=TRUE)


all_models_specs <- list(model_spec_all = model_spec_all,
                         model_spec_landuse = model_spec_landuse,
                         model_spec_density = model_spec_density,
                         model_spec_fcompact = model_spec_fcompact,
                         model_spec_circuity = model_spec_circuity,
                         model_spec_closenes = model_spec_closeness)


# model_output_all_min     <- lapply(X=model_spec_all, FUN=function(m){feols( formula(m)    , data=df_fuel, vcov='hetero')})
#
# # run all models
# model_spec_fcompact2     <- lapply(X=model_spec_fcompact, FUN=function(m){feols( formula(m)    , data=df_log, cluster='i_name_region')})

# model_output_all1     <- feols( formula(model_spec_all[[1]])    , data=df_log, vcov='hetero')
# model_output_all2      <- feols( formula(model_spec_all[[1]])    , data=df_log, cluster='i_name_region')





### check  heteroscedasticity-------------
#' https://evalf21.classes.andrewheiss.com/example/standard-errors/

# run base model
model_output_all_lm <- lm( formula(model_spec_all[[1]]), data=df_fuel)

# see results
summary(model_output_all_lm)
stargazer::stargazer( model_output_all_lm,   type = 'text')

# check 1
performance::check_heteroscedasticity(model_output_all_lm)

# check 2
# There is heteroscedasticity if p-values are smaller than 0.05
lmtest::bptest(model_output_all_lm) # Breusch-Pagan test
car::ncvTest(model_output_all_lm)  # NCV Test

# check 3
# top and bottom left: if cloud of points are random, then no heteroscedasticity
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(model_output_all_lm)

# check 4
fitted_data <- broom::augment(model_output_all_lm, data = df_log)

# check 4.1
# if distribution is normal,  then isn’t actually heteroskedastic
ggplot(fitted_data, aes(x = .resid)) +
  geom_histogram(binwidth = .05, color = "white", boundary = 1000)

# check 4.2
# Look at relationship between fitted values and residuals
# to check if errors are clustered
ggplot(fitted_data, aes(x = .fitted, y = .resid)) +
  geom_point(aes(color = x_state)) +
  facet_wrap(. ~i_name_region) +            # x_state i_name_region
  geom_smooth(method = "lm") +
  # scale_color_distiller(palette = 'grays') +
  theme_minimal()

#>>>> It seems errors are clustered by region


# in case there is heteroscedasticity, use ols with robust standard errors
mrobust <- feols(formula(model_spec_all[[1]]), data = df_log, cluster = "i_name_region")
summary(mrobust)



### check  multicollinearity-------------
library(mctest)
library(qgraph)

#VIF
vif_test <- mctest::imcdiag(model_output_all_lm, method ='VIF')
vif_test

# identify multicolinear variables
colinear_varnames <- subset(as.data.frame(vif_test$alldiag), VIF   ==T) %>% rownames()
colinear_varnames <- names(df_fuel)[names(df_fuel) %in% colinear_varnames]
qgraph::qgraph( cor( dplyr::select(df_fuel, colinear_varnames) ) ,
                #theme='gray',
                vsize=10,
                label.cex=4,
                labels=names(dplyr::select(df_fuel, colinear_varnames)),
                edge.labels = TRUE,
                layout='spring')


# check partial correlations
library(ppcor)
pcor(df_fuel, method = "pearson")



# check state Vs region controls / clusters --------------------------
ct_region_cluster_region <- "y_energy_per_capita ~f_compact_contig_inter_dens+x_circuity_avg+x_density_pop_02km_2014+x_land_use_mix+x_normalized_closeness_centrality_avg+ x_mean_fleet_age+ x_mean_slope+ x_pop_2010+ x_prop_dom_urban+ x_prop_high_educ+ x_prop_razao_dep+ x_total_pop_growth_1990_2014+ x_wghtd_mean_household_income_per_capita + i_name_region + x_pop_2010*f_compact_contig_inter_dens"
  ct_state_cluster_state <- "y_energy_per_capita ~f_compact_contig_inter_dens+x_circuity_avg+x_density_pop_02km_2014+x_land_use_mix+x_normalized_closeness_centrality_avg+ x_mean_fleet_age+ x_mean_slope+ x_pop_2010+ x_prop_dom_urban+ x_prop_high_educ+ x_prop_razao_dep+ x_total_pop_growth_1990_2014+ x_wghtd_mean_household_income_per_capita + x_state       + x_pop_2010*f_compact_contig_inter_dens"

# i_name_region
# x_state

ct_region_cluster_region <- feols(formula(ct_region_cluster_region), data = df_log, cluster = "i_name_region")
ct_state_cluster_region  <- feols(formula(ct_state_cluster_state), data = df_log, cluster = "i_name_region")
ct_state_cluster_state   <- feols(formula(ct_state_cluster_state), data = df_log, cluster = "x_state")

ll <- list(ct_region_cluster_region, ct_state_cluster_region , ct_state_cluster_state)
etable(ll)

# com interaction
# controle por regiao, cluster por regiao: land use and density ok sig.
# controle por estado, cluster por regiao: land use and density ok sig.
# controle por estado, cluster por estado: no sig

# sem interaction
# controle por regiao, cluster por regiao: land use and density ok sig.
# controle por estado, cluster por regiao: no sig land use
# controle por estado, cluster por estado: land use and density ok sig.


#### check interaction --------------
#' cor(df_fuel$x_pop_2010  , df_fuel$f_compact_contig_inter_dens)
library(interactions)
a <- lm(formula(model_spec_all[[1]]), data = df_log)
interact_plot(a, pred = 'f_compact_contig_inter_dens',
              modx = 'x_pop_2010' ,
              plot.points = TRUE) # interval = TRUE

interact_plot(a, pred = 'f_compact_contig_inter_dens', modx = 'x_pop_2010', linearity.check = TRUE,
              plot.points = TRUE)



### 666 feols pra valer ------------------------------------

# feols( formula(  all_models_specs[[1]][[1]] ) , data=df_log, cluster = "i_name_region")

for ( i in names(all_models_specs) ){# i = names(all_models_specs)[1]

  specs <- all_models_specs[i]
  specs <- unlist(specs)
  temp_model <- lapply(X=specs, FUN=function(m){feols( formula(m), data=df_log, cluster='i_name_region')})
  saveRDS(temp_model, file = paste0('./output/regression_output/', i,'.rds'))

  }


# # check marginal effects ----------------------

library(ggplot2)
library(margins)
library(marginaleffects)

# inverse hyperbolic sine
invs <- function(x){ log(x + sqrt(x^2 + 1) ) }


all_models_specs[1]

mlog <- feols(fml = log(y_energy_per_capita) ~ invs(f_compact_contig_inter_dens) + log(x_circuity_avg) + log(x_density_pop_02km_2014) + log(x_land_use_mix) + log(x_normalized_closeness_centrality_avg) + log(x_mean_fleet_age) + log(x_mean_slope) + log(x_pop_2010) + log(x_prop_dom_urban) + log(x_prop_high_educ) + log( x_prop_razao_dep) + log(x_state) + invs(x_total_pop_growth_1990_2014) + log(x_wghtd_mean_household_income_per_capita) + I(log(x_pop_2010)*invs(f_compact_contig_inter_dens))
              , data=df_raw, cluster='i_name_region')

summary(mlog)

plot_cme(mlog, effect = "f_compact_contig_inter_dens",
         condition = 'x_pop_2010')



# # compare models performance ----------------------
# library(performance)
#
# l <- list(ct_region_cluster_region, ct_state_cluster_state)
#
# tem_model <- lapply(X=l, FUN= lm, data=df_log)
# performance::check_model(tem_model)
# performance::compare_performance(tem_model, rank = T)  %>% plot()











