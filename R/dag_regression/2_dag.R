library(ggdag)
library(dagitty)
library(ggm)
library(fixest)
library(stargazer)


# dag model ----------------------------------

d <- dagitty('dag {
bb="0,0,1,1"
f_compact_contig_inter_dens [exposure,pos="0.528,0.034"]
x_circuity_avg [exposure,pos="0.524,0.244"]
x_density_pop_02km_2014 [exposure,pos="0.503,0.365"]
x_land_use_mix [exposure,pos="0.575,0.769"]
x_mean_fleet_age [pos="0.582,0.891"]
x_mean_slope [pos="0.323,0.262"]
x_normalized_closeness_centrality_avg [exposure,pos="0.523,0.137"]
x_pop_2010 [adjusted,pos="0.075,0.333"]
x_prop_autos_dom [pos="0.624,0.651"]
x_prop_dom_urban [pos="0.311,0.409"]
x_prop_high_educ [pos="0.325,0.646"]
x_prop_razao_dep [pos="0.410,0.560"]
x_state [pos="0.166,0.530"]
x_street_pop [pos="0.597,0.466"]
x_total_pop_growth_1990_2014 [pos="0.052,0.060"]
x_wghtd_mean_household_income_per_capita [pos="0.200,0.824"]
y_energy_per_capita [outcome,pos="0.928,0.429"]
f_compact_contig_inter_dens -> y_energy_per_capita
x_circuity_avg -> y_energy_per_capita
x_density_pop_02km_2014 -> x_land_use_mix
x_density_pop_02km_2014 -> x_prop_autos_dom
x_density_pop_02km_2014 -> x_street_pop
x_density_pop_02km_2014 -> y_energy_per_capita
x_land_use_mix -> x_prop_autos_dom
x_land_use_mix -> y_energy_per_capita
x_mean_fleet_age -> y_energy_per_capita
x_mean_slope -> x_circuity_avg
x_mean_slope -> y_energy_per_capita
x_normalized_closeness_centrality_avg -> y_energy_per_capita
x_pop_2010 -> f_compact_contig_inter_dens
x_pop_2010 -> x_density_pop_02km_2014
x_pop_2010 -> x_land_use_mix
x_pop_2010 -> x_normalized_closeness_centrality_avg
x_pop_2010 -> x_prop_dom_urban
x_pop_2010 -> x_street_pop
x_pop_2010 -> x_wghtd_mean_household_income_per_capita
x_prop_autos_dom -> x_street_pop
x_prop_autos_dom -> y_energy_per_capita
x_prop_dom_urban -> x_density_pop_02km_2014
x_prop_dom_urban -> x_prop_high_educ
x_prop_dom_urban -> y_energy_per_capita
x_prop_high_educ -> x_prop_razao_dep
x_prop_razao_dep -> x_prop_autos_dom
x_prop_razao_dep -> y_energy_per_capita
x_state -> x_mean_slope
x_state -> x_prop_dom_urban
x_state -> x_prop_high_educ
x_state -> x_wghtd_mean_household_income_per_capita
x_street_pop -> y_energy_per_capita
x_total_pop_growth_1990_2014 -> f_compact_contig_inter_dens
x_total_pop_growth_1990_2014 -> x_pop_2010
x_total_pop_growth_1990_2014 -> x_prop_dom_urban
x_wghtd_mean_household_income_per_capita -> x_mean_fleet_age
x_wghtd_mean_household_income_per_capita -> x_prop_autos_dom
x_wghtd_mean_household_income_per_capita -> x_street_pop
x_wghtd_mean_household_income_per_capita -> y_energy_per_capita
}')


#### test Implications ---------------------
test <- dagitty::localTests(x = d, data = cov( df_fuel ))
setDT(test)[p.value > 0.1] %>% nrow() / nrow(test)

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
                       c("f_compact_contig_inter_dens","x_normalized_closeness_centrality_avg","x_pop_2010"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_density_pop_02km_2014","x_land_use_mix","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_land_use_mix","x_prop_high_educ"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_prop_dom_urban","x_state","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_land_use_mix"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_pop_2010"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_prop_high_educ","x_pop_2010"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_density_pop_02km_2014","x_prop_razao_dep","x_pop_2010","x_state"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_density_pop_02km_2014","x_pop_2010","x_state","x_prop_high_educ"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_prop_dom_urban"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_prop_high_educ","x_pop_2010","x_prop_dom_urban"),
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
                       c("f_compact_contig_inter_dens","x_street_pop","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_pop_2010"),
                       c("f_compact_contig_inter_dens","x_street_pop","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_prop_high_educ","x_pop_2010"),
                       c("f_compact_contig_inter_dens","x_street_pop","x_prop_razao_dep","x_density_pop_02km_2014","x_pop_2010","x_state"),
                       c("f_compact_contig_inter_dens","x_street_pop","x_density_pop_02km_2014","x_prop_high_educ","x_pop_2010","x_state"),
                       c("f_compact_contig_inter_dens","x_street_pop","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_prop_dom_urban"),
                       c("f_compact_contig_inter_dens","x_street_pop","x_wghtd_mean_household_income_per_capita","x_prop_high_educ","x_pop_2010","x_prop_dom_urban"),
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
                       c("x_circuity_avg","x_prop_autos_dom","x_density_pop_02km_2014","x_land_use_mix","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita"),
                       c("x_circuity_avg","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_land_use_mix","x_prop_high_educ"),
                       c("x_circuity_avg","x_prop_autos_dom","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_pop_2010"),
                       c("x_circuity_avg","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_prop_high_educ","x_pop_2010"),
                       c("x_circuity_avg","x_prop_autos_dom","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_prop_dom_urban"),
                       c("x_circuity_avg","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_prop_high_educ","x_pop_2010","x_prop_dom_urban"),
                       c("x_circuity_avg","x_prop_autos_dom","x_state"),
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
                       c("x_circuity_avg","x_street_pop","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_pop_2010"),
                       c("x_circuity_avg","x_street_pop","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_prop_high_educ","x_pop_2010"),
                       c("x_circuity_avg","x_street_pop","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_prop_dom_urban"),
                       c("x_circuity_avg","x_street_pop","x_wghtd_mean_household_income_per_capita","x_prop_high_educ","x_pop_2010","x_prop_dom_urban"),
                       c("x_circuity_avg","x_street_pop","x_state"),
                       c("x_circuity_avg","x_street_pop","x_mean_slope"),
                       c("x_circuity_avg","x_total_pop_growth_1990_2014"),
                       c("x_circuity_avg","x_wghtd_mean_household_income_per_capita","x_state"),
                       c("x_circuity_avg","x_wghtd_mean_household_income_per_capita","x_mean_slope"),
                       c("x_density_pop_02km_2014","x_mean_fleet_age","x_wghtd_mean_household_income_per_capita"),
                       c("x_density_pop_02km_2014","x_mean_fleet_age","x_pop_2010","x_state"),
                       c("x_density_pop_02km_2014","x_mean_fleet_age","x_pop_2010","x_prop_dom_urban"),
                       c("x_density_pop_02km_2014","x_mean_slope","x_state"),
                       c("x_density_pop_02km_2014","x_mean_slope","x_prop_dom_urban","x_pop_2010"),
                       c("x_density_pop_02km_2014","x_normalized_closeness_centrality_avg","x_pop_2010"),
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
                       c("x_land_use_mix","x_normalized_closeness_centrality_avg","x_pop_2010"),
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
                       c("x_mean_slope","x_prop_autos_dom","x_density_pop_02km_2014","x_land_use_mix","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita"),
                       c("x_mean_slope","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_land_use_mix","x_prop_high_educ"),
                       c("x_mean_slope","x_prop_autos_dom","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_pop_2010"),
                       c("x_mean_slope","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_prop_high_educ","x_pop_2010"),
                       c("x_mean_slope","x_prop_autos_dom","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_prop_dom_urban"),
                       c("x_mean_slope","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_prop_high_educ","x_pop_2010","x_prop_dom_urban"),
                       c("x_mean_slope","x_prop_autos_dom","x_state"),
                       c("x_mean_slope","x_prop_dom_urban","x_state"),
                       c("x_mean_slope","x_prop_high_educ","x_state"),
                       c("x_mean_slope","x_prop_razao_dep","x_prop_high_educ"),
                       c("x_mean_slope","x_prop_razao_dep","x_state"),
                       c("x_mean_slope","x_street_pop","x_density_pop_02km_2014","x_pop_2010","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita"),
                       c("x_mean_slope","x_street_pop","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_pop_2010"),
                       c("x_mean_slope","x_street_pop","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_prop_high_educ","x_pop_2010"),
                       c("x_mean_slope","x_street_pop","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_prop_dom_urban"),
                       c("x_mean_slope","x_street_pop","x_wghtd_mean_household_income_per_capita","x_prop_high_educ","x_pop_2010","x_prop_dom_urban"),
                       c("x_mean_slope","x_street_pop","x_state"),
                       c("x_mean_slope","x_total_pop_growth_1990_2014"),
                       c("x_mean_slope","x_wghtd_mean_household_income_per_capita","x_state"),
                       c("x_normalized_closeness_centrality_avg","x_prop_autos_dom","x_density_pop_02km_2014","x_land_use_mix","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita"),
                       c("x_normalized_closeness_centrality_avg","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_land_use_mix","x_prop_high_educ"),
                       c("x_normalized_closeness_centrality_avg","x_prop_autos_dom","x_prop_dom_urban","x_state","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_land_use_mix"),
                       c("x_normalized_closeness_centrality_avg","x_prop_autos_dom","x_pop_2010"),
                       c("x_normalized_closeness_centrality_avg","x_prop_dom_urban","x_pop_2010"),
                       c("x_normalized_closeness_centrality_avg","x_prop_high_educ","x_prop_dom_urban","x_state"),
                       c("x_normalized_closeness_centrality_avg","x_prop_high_educ","x_pop_2010"),
                       c("x_normalized_closeness_centrality_avg","x_prop_razao_dep","x_prop_high_educ"),
                       c("x_normalized_closeness_centrality_avg","x_prop_razao_dep","x_prop_dom_urban","x_state"),
                       c("x_normalized_closeness_centrality_avg","x_prop_razao_dep","x_pop_2010"),
                       c("x_normalized_closeness_centrality_avg","x_state"),
                       c("x_normalized_closeness_centrality_avg","x_street_pop","x_pop_2010"),
                       c("x_normalized_closeness_centrality_avg","x_total_pop_growth_1990_2014","x_pop_2010"),
                       c("x_normalized_closeness_centrality_avg","x_wghtd_mean_household_income_per_capita","x_pop_2010"),
                       c("x_pop_2010","x_prop_autos_dom","x_density_pop_02km_2014","x_land_use_mix","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita"),
                       c("x_pop_2010","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_land_use_mix","x_prop_high_educ"),
                       c("x_pop_2010","x_prop_autos_dom","x_prop_dom_urban","x_state","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_land_use_mix"),
                       c("x_pop_2010","x_prop_high_educ","x_prop_dom_urban","x_state"),
                       c("x_pop_2010","x_prop_razao_dep","x_prop_high_educ"),
                       c("x_pop_2010","x_prop_razao_dep","x_prop_dom_urban","x_state"),
                       c("x_pop_2010","x_state"),
                       c("x_pop_2010","y_energy_per_capita","x_mean_slope","x_normalized_closeness_centrality_avg","x_prop_autos_dom","x_prop_dom_urban","x_prop_razao_dep","x_street_pop","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_density_pop_02km_2014","x_land_use_mix"),
                       c("x_pop_2010","y_energy_per_capita","x_street_pop","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_density_pop_02km_2014","x_land_use_mix","x_mean_slope","x_normalized_closeness_centrality_avg","x_prop_autos_dom","x_prop_dom_urban","x_prop_high_educ"),
                       c("x_pop_2010","y_energy_per_capita","x_prop_dom_urban","x_state","x_street_pop","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_density_pop_02km_2014","x_land_use_mix","x_normalized_closeness_centrality_avg","x_prop_autos_dom"),
                       c("x_prop_autos_dom","x_prop_dom_urban","x_pop_2010","x_state","x_prop_high_educ","x_density_pop_02km_2014"),
                       c("x_prop_autos_dom","x_prop_dom_urban","x_pop_2010","x_wghtd_mean_household_income_per_capita","x_prop_high_educ","x_density_pop_02km_2014"),
                       c("x_prop_autos_dom","x_prop_dom_urban","x_pop_2010","x_state","x_prop_razao_dep","x_density_pop_02km_2014"),
                       c("x_prop_autos_dom","x_prop_dom_urban","x_pop_2010","x_wghtd_mean_household_income_per_capita","x_prop_razao_dep","x_density_pop_02km_2014"),
                       c("x_prop_autos_dom","x_prop_dom_urban","x_wghtd_mean_household_income_per_capita","x_prop_high_educ","x_land_use_mix","x_density_pop_02km_2014"),
                       c("x_prop_autos_dom","x_prop_dom_urban","x_wghtd_mean_household_income_per_capita","x_land_use_mix","x_density_pop_02km_2014","x_prop_razao_dep"),
                       c("x_prop_autos_dom","x_prop_high_educ","x_prop_razao_dep","x_prop_dom_urban","x_state"),
                       c("x_prop_autos_dom","x_prop_high_educ","x_prop_dom_urban","x_pop_2010","x_wghtd_mean_household_income_per_capita","x_prop_razao_dep"),
                       c("x_prop_autos_dom","x_prop_high_educ","x_pop_2010","x_state","x_density_pop_02km_2014","x_prop_razao_dep"),
                       c("x_prop_autos_dom","x_prop_high_educ","x_pop_2010","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_prop_razao_dep"),
                       c("x_prop_autos_dom","x_prop_high_educ","x_wghtd_mean_household_income_per_capita","x_land_use_mix","x_density_pop_02km_2014","x_prop_razao_dep"),
                       c("x_prop_autos_dom","x_state","x_pop_2010","x_prop_dom_urban","x_wghtd_mean_household_income_per_capita","x_prop_high_educ"),
                       c("x_prop_autos_dom","x_state","x_pop_2010","x_prop_dom_urban","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita"),
                       c("x_prop_autos_dom","x_state","x_prop_high_educ","x_density_pop_02km_2014","x_pop_2010","x_wghtd_mean_household_income_per_capita"),
                       c("x_prop_autos_dom","x_state","x_density_pop_02km_2014","x_pop_2010","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita"),
                       c("x_prop_autos_dom","x_state","x_prop_high_educ","x_density_pop_02km_2014","x_wghtd_mean_household_income_per_capita","x_land_use_mix"),
                       c("x_prop_autos_dom","x_state","x_density_pop_02km_2014","x_wghtd_mean_household_income_per_capita","x_land_use_mix","x_prop_razao_dep"),
                       c("x_prop_autos_dom","x_total_pop_growth_1990_2014","x_pop_2010","x_prop_dom_urban","x_state"),
                       c("x_prop_autos_dom","x_total_pop_growth_1990_2014","x_wghtd_mean_household_income_per_capita","x_prop_high_educ","x_pop_2010","x_prop_dom_urban"),
                       c("x_prop_autos_dom","x_total_pop_growth_1990_2014","x_prop_razao_dep","x_prop_dom_urban","x_wghtd_mean_household_income_per_capita","x_pop_2010"),
                       c("x_prop_autos_dom","x_total_pop_growth_1990_2014","x_state","x_prop_high_educ","x_density_pop_02km_2014","x_pop_2010"),
                       c("x_prop_autos_dom","x_total_pop_growth_1990_2014","x_wghtd_mean_household_income_per_capita","x_prop_high_educ","x_pop_2010","x_density_pop_02km_2014"),
                       c("x_prop_autos_dom","x_total_pop_growth_1990_2014","x_prop_razao_dep","x_state","x_density_pop_02km_2014","x_pop_2010"),
                       c("x_prop_autos_dom","x_total_pop_growth_1990_2014","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_density_pop_02km_2014"),
                       c("x_prop_autos_dom","x_total_pop_growth_1990_2014","x_state","x_wghtd_mean_household_income_per_capita","x_prop_dom_urban","x_land_use_mix","x_density_pop_02km_2014"),
                       c("x_prop_autos_dom","x_total_pop_growth_1990_2014","x_wghtd_mean_household_income_per_capita","x_prop_high_educ","x_density_pop_02km_2014","x_land_use_mix"),
                       c("x_prop_autos_dom","x_total_pop_growth_1990_2014","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_land_use_mix"),
                       c("x_prop_dom_urban","x_prop_razao_dep","x_prop_high_educ"),
                       c("x_prop_dom_urban","x_street_pop","x_density_pop_02km_2014","x_pop_2010","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita"),
                       c("x_prop_dom_urban","x_street_pop","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_pop_2010"),
                       c("x_prop_dom_urban","x_street_pop","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_prop_high_educ","x_pop_2010"),
                       c("x_prop_dom_urban","x_street_pop","x_prop_razao_dep","x_density_pop_02km_2014","x_pop_2010","x_state"),
                       c("x_prop_dom_urban","x_street_pop","x_density_pop_02km_2014","x_prop_high_educ","x_pop_2010","x_state"),
                       c("x_prop_dom_urban","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_state"),
                       c("x_prop_high_educ","x_street_pop","x_density_pop_02km_2014","x_pop_2010","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita"),
                       c("x_prop_high_educ","x_street_pop","x_prop_dom_urban","x_state","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_land_use_mix","x_density_pop_02km_2014"),
                       c("x_prop_high_educ","x_street_pop","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_pop_2010"),
                       c("x_prop_high_educ","x_street_pop","x_prop_razao_dep","x_density_pop_02km_2014","x_pop_2010","x_state"),
                       c("x_prop_high_educ","x_street_pop","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_prop_dom_urban"),
                       c("x_prop_high_educ","x_street_pop","x_prop_dom_urban","x_state","x_prop_razao_dep"),
                       c("x_prop_high_educ","x_total_pop_growth_1990_2014","x_state","x_prop_dom_urban"),
                       c("x_prop_high_educ","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_state"),
                       c("x_prop_high_educ","x_wghtd_mean_household_income_per_capita","x_prop_dom_urban","x_state"),
                       c("x_prop_high_educ","y_energy_per_capita","x_mean_slope","x_normalized_closeness_centrality_avg","x_prop_autos_dom","x_prop_dom_urban","x_prop_razao_dep","x_street_pop","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_density_pop_02km_2014","x_land_use_mix"),
                       c("x_prop_high_educ","y_energy_per_capita","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_mean_slope","x_prop_dom_urban","x_prop_razao_dep","x_pop_2010"),
                       c("x_prop_high_educ","y_energy_per_capita","x_wghtd_mean_household_income_per_capita","x_mean_slope","x_prop_dom_urban","x_prop_razao_dep","x_pop_2010","x_total_pop_growth_1990_2014"),
                       c("x_prop_high_educ","y_energy_per_capita","x_prop_dom_urban","x_state","x_prop_razao_dep"),
                       c("x_prop_razao_dep","x_state","x_prop_high_educ"),
                       c("x_prop_razao_dep","x_street_pop","x_density_pop_02km_2014","x_pop_2010","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita"),
                       c("x_prop_razao_dep","x_street_pop","x_prop_dom_urban","x_state","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_land_use_mix","x_density_pop_02km_2014"),
                       c("x_prop_razao_dep","x_street_pop","x_density_pop_02km_2014","x_prop_high_educ","x_wghtd_mean_household_income_per_capita","x_prop_autos_dom","x_land_use_mix"),
                       c("x_prop_razao_dep","x_total_pop_growth_1990_2014","x_state","x_prop_dom_urban"),
                       c("x_prop_razao_dep","x_total_pop_growth_1990_2014","x_prop_high_educ"),
                       c("x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_state"),
                       c("x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_prop_dom_urban","x_state"),
                       c("x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_prop_high_educ"),
                       c("x_state","x_street_pop","x_density_pop_02km_2014","x_pop_2010","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita"),
                       c("x_state","x_street_pop","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_pop_2010"),
                       c("x_state","x_street_pop","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2014","x_prop_high_educ","x_pop_2010"),
                       c("x_state","x_street_pop","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_prop_dom_urban"),
                       c("x_state","x_street_pop","x_wghtd_mean_household_income_per_capita","x_prop_high_educ","x_pop_2010","x_prop_dom_urban"),
                       c("x_state","x_total_pop_growth_1990_2014"),
                       c("x_state","y_energy_per_capita","x_mean_slope","x_normalized_closeness_centrality_avg","x_prop_autos_dom","x_prop_dom_urban","x_prop_razao_dep","x_street_pop","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_density_pop_02km_2014","x_land_use_mix"),
                       c("x_state","y_energy_per_capita","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_mean_slope","x_prop_dom_urban","x_prop_razao_dep","x_pop_2010"),
                       c("x_state","y_energy_per_capita","x_wghtd_mean_household_income_per_capita","x_mean_slope","x_prop_dom_urban","x_prop_razao_dep","x_pop_2010","x_total_pop_growth_1990_2014"),
                       c("x_state","y_energy_per_capita","x_street_pop","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_density_pop_02km_2014","x_land_use_mix","x_mean_slope","x_normalized_closeness_centrality_avg","x_prop_autos_dom","x_prop_dom_urban","x_prop_high_educ"),
                       c("x_state","y_energy_per_capita","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_mean_slope","x_prop_dom_urban","x_pop_2010","x_prop_high_educ"),
                       c("x_state","y_energy_per_capita","x_wghtd_mean_household_income_per_capita","x_mean_slope","x_prop_dom_urban","x_pop_2010","x_prop_high_educ","x_total_pop_growth_1990_2014"),
                       c("x_street_pop","x_total_pop_growth_1990_2014","x_prop_dom_urban","x_pop_2010","x_state"),
                       c("x_street_pop","x_total_pop_growth_1990_2014","x_wghtd_mean_household_income_per_capita","x_prop_high_educ","x_pop_2010","x_prop_dom_urban"),
                       c("x_street_pop","x_total_pop_growth_1990_2014","x_prop_razao_dep","x_prop_dom_urban","x_wghtd_mean_household_income_per_capita","x_pop_2010"),
                       c("x_street_pop","x_total_pop_growth_1990_2014","x_state","x_prop_high_educ","x_density_pop_02km_2014","x_pop_2010"),
                       c("x_street_pop","x_total_pop_growth_1990_2014","x_wghtd_mean_household_income_per_capita","x_prop_high_educ","x_pop_2010","x_density_pop_02km_2014"),
                       c("x_street_pop","x_total_pop_growth_1990_2014","x_prop_razao_dep","x_state","x_density_pop_02km_2014","x_pop_2010"),
                       c("x_street_pop","x_total_pop_growth_1990_2014","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_density_pop_02km_2014"),
                       c("x_street_pop","x_total_pop_growth_1990_2014","x_wghtd_mean_household_income_per_capita","x_prop_autos_dom","x_density_pop_02km_2014","x_pop_2010"),
                       c("x_total_pop_growth_1990_2014","x_wghtd_mean_household_income_per_capita","x_pop_2010"),
                       c("x_total_pop_growth_1990_2014","y_energy_per_capita","x_mean_slope","x_normalized_closeness_centrality_avg","x_prop_autos_dom","x_prop_dom_urban","x_prop_razao_dep","x_street_pop","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_density_pop_02km_2014","x_land_use_mix"),
                       c("x_total_pop_growth_1990_2014","y_energy_per_capita","x_street_pop","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_density_pop_02km_2014","x_land_use_mix","x_mean_slope","x_normalized_closeness_centrality_avg","x_prop_autos_dom","x_prop_dom_urban","x_prop_high_educ"),
                       c("x_total_pop_growth_1990_2014","y_energy_per_capita","x_prop_dom_urban","x_state","x_street_pop","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_density_pop_02km_2014","x_land_use_mix","x_normalized_closeness_centrality_avg","x_prop_autos_dom"),
                       c("x_total_pop_growth_1990_2014","y_energy_per_capita","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_mean_slope","x_prop_dom_urban","x_prop_razao_dep","x_pop_2010"),
                       c("x_total_pop_growth_1990_2014","y_energy_per_capita","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_mean_slope","x_prop_dom_urban","x_pop_2010","x_prop_high_educ"),
                       c("x_total_pop_growth_1990_2014","y_energy_per_capita","x_prop_dom_urban","x_state","f_compact_contig_inter_dens","x_pop_2010"))
  data.frame( implication=unlist(lapply(implications,tos)),
              pvalue=unlist( lapply( implications, tst ) ) )
}

test <- testImplications( covariance.matrix = cov( df_fuel ),
                          sample.size = nrow(df_fuel))

setDT(test)[pvalue > 0.1] %>% nrow() / nrow(test)
#' cor(df_fuel$f_compact_contig_inter_dens , df_fuel$x_street_pop )
#' cor(df_fuel$x_pop_2010  , df_fuel$x_density_pop_02km_2014)

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

#>>>> It seems errors are clustered by state


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
interact_plot(a, pred = 'f_compact_contig_inter_dens', modx = 'x_pop_2010' , plot.points = TRUE) # interval = TRUE

interact_plot(a, pred = 'f_compact_contig_inter_dens', modx = 'x_pop_2010', linearity.check = TRUE,
              plot.points = TRUE)



### feols pra valer ------------------------------------

# feols( formula(  all_models_specs[[1]][[1]] ) , data=df_log, cluster = "i_name_region")

for ( i in names(all_models_specs) ){# i = names(all_models_specs)[1]

  specs <- all_models_specs[i]
  specs <- unlist(specs)
  temp_model <- lapply(X=specs, FUN=function(m){feols( formula(m), data=df_log, cluster='i_name_region')})
  saveRDS(temp_model, file = paste0('./output/regression_output/', i,'.rds'))

  }








# # compare models performance ----------------------
# library(performance)
#
# l <- list(ct_region_cluster_region, ct_state_cluster_state)
#
# tem_model <- lapply(X=l, FUN= lm, data=df_log)
# performance::check_model(tem_model)
# performance::compare_performance(tem_model, rank = T)  %>% plot()
#










