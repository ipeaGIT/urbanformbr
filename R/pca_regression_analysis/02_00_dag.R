library(ggdag)
library(dagitty)
library(ggm)

#### test Implications ---------------------

testImplications <- function( covariance.matrix, sample.size ){
  library(ggm)
  tst <- function(i){ pcor.test( pcor(i,covariance.matrix), length(i)-2, sample.size )$pvalue }
  tos <- function(i){ paste(i,collapse=" ") }
  implications <- list(c("f_compact_contig_inter_dens","x_circuity_avg"),
                       c("f_compact_contig_inter_dens","x_closeness_centrality_avg","x_pop_2010"),
                       c("f_compact_contig_inter_dens","x_land_use_mix","x_density_pop_02km_2015","x_pop_2010"),
                       c("f_compact_contig_inter_dens","x_land_use_mix","x_pop_2010","x_prop_dom_urban"),
                       c("f_compact_contig_inter_dens","x_land_use_mix","x_pop_2010","x_pop_growth_1975_2015"),
                       c("f_compact_contig_inter_dens","x_mean_slope"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_density_pop_02km_2015","x_land_use_mix","x_prop_razao_dep","x_street_pop","x_wghtd_mean_household_income_per_capita"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_street_pop","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2015","x_land_use_mix","x_prop_high_educ"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_prop_dom_urban","x_state","x_street_pop","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2015","x_land_use_mix"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2015","x_prop_razao_dep","x_pop_2010"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_prop_razao_dep","x_pop_2010","x_prop_dom_urban"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2015","x_pop_2010","x_prop_high_educ"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_prop_high_educ","x_prop_dom_urban"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_density_pop_02km_2015","x_prop_razao_dep","x_pop_2010","x_state"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_density_pop_02km_2015","x_pop_2010","x_state","x_prop_high_educ"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_prop_dom_urban","x_state","x_pop_2010"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_pop_2010","x_pop_growth_1975_2015"),
                       c("f_compact_contig_inter_dens","x_prop_dom_urban","x_pop_2010","x_pop_growth_1975_2015"),
                       c("f_compact_contig_inter_dens","x_prop_high_educ","x_prop_dom_urban","x_state"),
                       c("f_compact_contig_inter_dens","x_prop_high_educ","x_pop_2010","x_pop_growth_1975_2015"),
                       c("f_compact_contig_inter_dens","x_prop_razao_dep","x_prop_high_educ"),
                       c("f_compact_contig_inter_dens","x_prop_razao_dep","x_prop_dom_urban","x_state"),
                       c("f_compact_contig_inter_dens","x_prop_razao_dep","x_pop_2010","x_pop_growth_1975_2015"),
                       c("f_compact_contig_inter_dens","x_state"),
                       c("f_compact_contig_inter_dens","x_street_pop","x_density_pop_02km_2015","x_pop_2010","x_wghtd_mean_household_income_per_capita"),
                       c("f_compact_contig_inter_dens","x_street_pop","x_pop_2010","x_wghtd_mean_household_income_per_capita","x_prop_dom_urban"),
                       c("f_compact_contig_inter_dens","x_street_pop","x_density_pop_02km_2015","x_pop_2010","x_state"),
                       c("f_compact_contig_inter_dens","x_street_pop","x_pop_2010","x_state","x_prop_dom_urban"),
                       c("f_compact_contig_inter_dens","x_street_pop","x_pop_2010","x_pop_growth_1975_2015"),
                       c("f_compact_contig_inter_dens","x_wghtd_mean_household_income_per_capita","x_pop_2010"),
                       c("f_compact_contig_inter_dens","x_mean_age_auto","x_wghtd_mean_household_income_per_capita"),
                       c("f_compact_contig_inter_dens","x_mean_age_auto","x_pop_2010"),
                       c("f_compact_contig_inter_dens","x_density_pop_02km_2015","x_pop_2010","x_prop_dom_urban"),
                       c("f_compact_contig_inter_dens","x_density_pop_02km_2015","x_pop_2010","x_pop_growth_1975_2015"),
                       c("x_circuity_avg","x_closeness_centrality_avg"),
                       c("x_circuity_avg","x_land_use_mix","x_density_pop_02km_2015","x_pop_2010"),
                       c("x_circuity_avg","x_land_use_mix","x_pop_2010","x_prop_dom_urban"),
                       c("x_circuity_avg","x_land_use_mix","x_state"),
                       c("x_circuity_avg","x_land_use_mix","x_mean_slope"),
                       c("x_circuity_avg","x_pop_2010"),
                       c("x_circuity_avg","x_pop_growth_1975_2015"),
                       c("x_circuity_avg","x_prop_autos_dom","x_density_pop_02km_2015","x_land_use_mix","x_prop_razao_dep","x_street_pop","x_wghtd_mean_household_income_per_capita"),
                       c("x_circuity_avg","x_prop_autos_dom","x_street_pop","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2015","x_land_use_mix","x_prop_high_educ"),
                       c("x_circuity_avg","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2015","x_prop_razao_dep","x_pop_2010"),
                       c("x_circuity_avg","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2015","x_pop_2010","x_prop_high_educ"),
                       c("x_circuity_avg","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_prop_razao_dep","x_pop_2010","x_prop_dom_urban"),
                       c("x_circuity_avg","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_prop_high_educ","x_prop_dom_urban"),
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
                       c("x_circuity_avg","x_street_pop","x_density_pop_02km_2015","x_pop_2010","x_wghtd_mean_household_income_per_capita"),
                       c("x_circuity_avg","x_street_pop","x_pop_2010","x_wghtd_mean_household_income_per_capita","x_prop_dom_urban"),
                       c("x_circuity_avg","x_street_pop","x_state"),
                       c("x_circuity_avg","x_street_pop","x_mean_slope"),
                       c("x_circuity_avg","x_wghtd_mean_household_income_per_capita","x_state"),
                       c("x_circuity_avg","x_wghtd_mean_household_income_per_capita","x_mean_slope"),
                       c("x_circuity_avg","x_mean_age_auto","x_wghtd_mean_household_income_per_capita"),
                       c("x_circuity_avg","x_mean_age_auto","x_state"),
                       c("x_circuity_avg","x_mean_age_auto","x_mean_slope"),
                       c("x_circuity_avg","x_density_pop_02km_2015","x_pop_2010","x_prop_dom_urban"),
                       c("x_circuity_avg","x_density_pop_02km_2015","x_state"),
                       c("x_circuity_avg","x_density_pop_02km_2015","x_mean_slope"),
                       c("x_closeness_centrality_avg","x_land_use_mix","x_pop_2010"),
                       c("x_closeness_centrality_avg","x_mean_slope"),
                       c("x_closeness_centrality_avg","x_pop_growth_1975_2015","x_pop_2010"),
                       c("x_closeness_centrality_avg","x_prop_autos_dom","x_density_pop_02km_2015","x_land_use_mix","x_prop_razao_dep","x_street_pop","x_wghtd_mean_household_income_per_capita"),
                       c("x_closeness_centrality_avg","x_prop_autos_dom","x_street_pop","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2015","x_land_use_mix","x_prop_high_educ"),
                       c("x_closeness_centrality_avg","x_prop_autos_dom","x_prop_dom_urban","x_state","x_street_pop","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2015","x_land_use_mix"),
                       c("x_closeness_centrality_avg","x_prop_autos_dom","x_pop_2010"),
                       c("x_closeness_centrality_avg","x_prop_dom_urban","x_pop_2010"),
                       c("x_closeness_centrality_avg","x_prop_high_educ","x_prop_dom_urban","x_state"),
                       c("x_closeness_centrality_avg","x_prop_high_educ","x_pop_2010"),
                       c("x_closeness_centrality_avg","x_prop_razao_dep","x_prop_high_educ"),
                       c("x_closeness_centrality_avg","x_prop_razao_dep","x_prop_dom_urban","x_state"),
                       c("x_closeness_centrality_avg","x_prop_razao_dep","x_pop_2010"),
                       c("x_closeness_centrality_avg","x_state"),
                       c("x_closeness_centrality_avg","x_street_pop","x_pop_2010"),
                       c("x_closeness_centrality_avg","x_wghtd_mean_household_income_per_capita","x_pop_2010"),
                       c("x_closeness_centrality_avg","x_mean_age_auto","x_wghtd_mean_household_income_per_capita"),
                       c("x_closeness_centrality_avg","x_mean_age_auto","x_pop_2010"),
                       c("x_closeness_centrality_avg","x_density_pop_02km_2015","x_pop_2010"),
                       c("x_land_use_mix","x_mean_slope","x_state"),
                       c("x_land_use_mix","x_mean_slope","x_pop_2010","x_prop_dom_urban"),
                       c("x_land_use_mix","x_mean_slope","x_pop_2010","x_density_pop_02km_2015"),
                       c("x_land_use_mix","x_pop_growth_1975_2015","x_prop_dom_urban","x_pop_2010"),
                       c("x_land_use_mix","x_pop_growth_1975_2015","x_density_pop_02km_2015","x_pop_2010"),
                       c("x_land_use_mix","x_prop_dom_urban","x_pop_2010","x_density_pop_02km_2015"),
                       c("x_land_use_mix","x_prop_high_educ","x_prop_dom_urban","x_state"),
                       c("x_land_use_mix","x_prop_high_educ","x_prop_dom_urban","x_pop_2010"),
                       c("x_land_use_mix","x_prop_high_educ","x_pop_2010","x_density_pop_02km_2015"),
                       c("x_land_use_mix","x_prop_razao_dep","x_prop_high_educ"),
                       c("x_land_use_mix","x_prop_razao_dep","x_prop_dom_urban","x_state"),
                       c("x_land_use_mix","x_prop_razao_dep","x_prop_dom_urban","x_pop_2010"),
                       c("x_land_use_mix","x_prop_razao_dep","x_pop_2010","x_density_pop_02km_2015"),
                       c("x_land_use_mix","x_state","x_pop_2010","x_prop_dom_urban"),
                       c("x_land_use_mix","x_state","x_pop_2010","x_density_pop_02km_2015"),
                       c("x_land_use_mix","x_street_pop","x_pop_2010","x_density_pop_02km_2015"),
                       c("x_land_use_mix","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_state"),
                       c("x_land_use_mix","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_prop_dom_urban"),
                       c("x_land_use_mix","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_density_pop_02km_2015"),
                       c("x_land_use_mix","x_mean_age_auto","x_wghtd_mean_household_income_per_capita"),
                       c("x_land_use_mix","x_mean_age_auto","x_pop_2010","x_state"),
                       c("x_land_use_mix","x_mean_age_auto","x_pop_2010","x_prop_dom_urban"),
                       c("x_land_use_mix","x_mean_age_auto","x_pop_2010","x_density_pop_02km_2015"),
                       c("x_mean_slope","x_pop_2010"),
                       c("x_mean_slope","x_pop_growth_1975_2015"),
                       c("x_mean_slope","x_prop_autos_dom","x_density_pop_02km_2015","x_land_use_mix","x_prop_razao_dep","x_street_pop","x_wghtd_mean_household_income_per_capita"),
                       c("x_mean_slope","x_prop_autos_dom","x_street_pop","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2015","x_land_use_mix","x_prop_high_educ"),
                       c("x_mean_slope","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2015","x_prop_razao_dep","x_pop_2010"),
                       c("x_mean_slope","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2015","x_pop_2010","x_prop_high_educ"),
                       c("x_mean_slope","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_prop_razao_dep","x_pop_2010","x_prop_dom_urban"),
                       c("x_mean_slope","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_prop_high_educ","x_prop_dom_urban"),
                       c("x_mean_slope","x_prop_autos_dom","x_state"),
                       c("x_mean_slope","x_prop_dom_urban","x_state"),
                       c("x_mean_slope","x_prop_high_educ","x_state"),
                       c("x_mean_slope","x_prop_razao_dep","x_prop_high_educ"),
                       c("x_mean_slope","x_prop_razao_dep","x_state"),
                       c("x_mean_slope","x_street_pop","x_density_pop_02km_2015","x_pop_2010","x_wghtd_mean_household_income_per_capita"),
                       c("x_mean_slope","x_street_pop","x_pop_2010","x_wghtd_mean_household_income_per_capita","x_prop_dom_urban"),
                       c("x_mean_slope","x_street_pop","x_state"),
                       c("x_mean_slope","x_wghtd_mean_household_income_per_capita","x_state"),
                       c("x_mean_slope","x_mean_age_auto","x_wghtd_mean_household_income_per_capita"),
                       c("x_mean_slope","x_mean_age_auto","x_state"),
                       c("x_mean_slope","x_density_pop_02km_2015","x_pop_2010","x_prop_dom_urban"),
                       c("x_mean_slope","x_density_pop_02km_2015","x_state"),
                       c("x_pop_2010","x_prop_autos_dom","x_density_pop_02km_2015","x_land_use_mix","x_prop_razao_dep","x_street_pop","x_wghtd_mean_household_income_per_capita"),
                       c("x_pop_2010","x_prop_autos_dom","x_street_pop","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2015","x_land_use_mix","x_prop_high_educ"),
                       c("x_pop_2010","x_prop_autos_dom","x_prop_dom_urban","x_state","x_street_pop","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2015","x_land_use_mix"),
                       c("x_pop_2010","x_prop_high_educ","x_prop_dom_urban","x_state"),
                       c("x_pop_2010","x_prop_razao_dep","x_prop_high_educ"),
                       c("x_pop_2010","x_prop_razao_dep","x_prop_dom_urban","x_state"),
                       c("x_pop_2010","x_state"),
                       c("x_pop_2010","y_energy_per_capita","x_prop_dom_urban","x_prop_razao_dep","x_street_pop","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_closeness_centrality_avg","x_density_pop_02km_2015","x_land_use_mix","x_mean_slope"),
                       c("x_pop_2010","y_energy_per_capita","x_street_pop","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_closeness_centrality_avg","x_density_pop_02km_2015","x_land_use_mix","x_mean_slope","x_prop_dom_urban","x_prop_high_educ"),
                       c("x_pop_2010","y_energy_per_capita","x_prop_dom_urban","x_state","x_street_pop","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_closeness_centrality_avg","x_density_pop_02km_2015","x_land_use_mix"),
                       c("x_pop_2010","x_mean_age_auto","x_wghtd_mean_household_income_per_capita"),
                       c("x_pop_growth_1975_2015","x_prop_autos_dom","x_density_pop_02km_2015","x_land_use_mix","x_prop_razao_dep","x_street_pop","x_wghtd_mean_household_income_per_capita"),
                       c("x_pop_growth_1975_2015","x_prop_autos_dom","x_street_pop","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2015","x_land_use_mix","x_prop_high_educ"),
                       c("x_pop_growth_1975_2015","x_prop_autos_dom","x_prop_dom_urban","x_state","x_street_pop","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2015","x_land_use_mix"),
                       c("x_pop_growth_1975_2015","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2015","x_prop_razao_dep","x_pop_2010"),
                       c("x_pop_growth_1975_2015","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_prop_razao_dep","x_pop_2010","x_prop_dom_urban"),
                       c("x_pop_growth_1975_2015","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2015","x_pop_2010","x_prop_high_educ"),
                       c("x_pop_growth_1975_2015","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_prop_high_educ","x_prop_dom_urban"),
                       c("x_pop_growth_1975_2015","x_prop_autos_dom","x_density_pop_02km_2015","x_prop_razao_dep","x_pop_2010","x_state"),
                       c("x_pop_growth_1975_2015","x_prop_autos_dom","x_density_pop_02km_2015","x_pop_2010","x_state","x_prop_high_educ"),
                       c("x_pop_growth_1975_2015","x_prop_autos_dom","x_prop_dom_urban","x_state","x_pop_2010"),
                       c("x_pop_growth_1975_2015","x_prop_high_educ","x_prop_dom_urban","x_state"),
                       c("x_pop_growth_1975_2015","x_prop_razao_dep","x_prop_high_educ"),
                       c("x_pop_growth_1975_2015","x_prop_razao_dep","x_prop_dom_urban","x_state"),
                       c("x_pop_growth_1975_2015","x_state"),
                       c("x_pop_growth_1975_2015","x_street_pop","x_density_pop_02km_2015","x_pop_2010","x_wghtd_mean_household_income_per_capita"),
                       c("x_pop_growth_1975_2015","x_street_pop","x_pop_2010","x_wghtd_mean_household_income_per_capita","x_prop_dom_urban"),
                       c("x_pop_growth_1975_2015","x_street_pop","x_density_pop_02km_2015","x_pop_2010","x_state"),
                       c("x_pop_growth_1975_2015","x_street_pop","x_pop_2010","x_state","x_prop_dom_urban"),
                       c("x_pop_growth_1975_2015","x_wghtd_mean_household_income_per_capita","x_pop_2010"),
                       c("x_pop_growth_1975_2015","y_energy_per_capita","x_prop_dom_urban","x_prop_razao_dep","x_street_pop","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_closeness_centrality_avg","x_density_pop_02km_2015","x_land_use_mix","x_mean_slope"),
                       c("x_pop_growth_1975_2015","y_energy_per_capita","x_street_pop","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_closeness_centrality_avg","x_density_pop_02km_2015","x_land_use_mix","x_mean_slope","x_prop_dom_urban","x_prop_high_educ"),
                       c("x_pop_growth_1975_2015","y_energy_per_capita","x_prop_dom_urban","x_state","x_street_pop","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_closeness_centrality_avg","x_density_pop_02km_2015","x_land_use_mix"),
                       c("x_pop_growth_1975_2015","y_energy_per_capita","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_mean_slope","x_prop_dom_urban","x_prop_razao_dep","x_pop_2010"),
                       c("x_pop_growth_1975_2015","y_energy_per_capita","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_mean_slope","x_prop_dom_urban","x_pop_2010","x_prop_high_educ"),
                       c("x_pop_growth_1975_2015","y_energy_per_capita","x_prop_dom_urban","x_state","f_compact_contig_inter_dens","x_pop_2010"),
                       c("x_pop_growth_1975_2015","x_mean_age_auto","x_wghtd_mean_household_income_per_capita"),
                       c("x_pop_growth_1975_2015","x_mean_age_auto","x_pop_2010"),
                       c("x_pop_growth_1975_2015","x_density_pop_02km_2015","x_pop_2010","x_prop_dom_urban"),
                       c("x_prop_autos_dom","x_prop_dom_urban","x_state","x_pop_2010","x_prop_high_educ","x_density_pop_02km_2015"),
                       c("x_prop_autos_dom","x_prop_dom_urban","x_state","x_pop_2010","x_prop_razao_dep","x_density_pop_02km_2015"),
                       c("x_prop_autos_dom","x_prop_dom_urban","x_wghtd_mean_household_income_per_capita","x_prop_high_educ","x_pop_2010","x_density_pop_02km_2015"),
                       c("x_prop_autos_dom","x_prop_dom_urban","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_prop_razao_dep","x_density_pop_02km_2015"),
                       c("x_prop_autos_dom","x_prop_dom_urban","x_wghtd_mean_household_income_per_capita","x_prop_high_educ","x_street_pop","x_land_use_mix","x_density_pop_02km_2015"),
                       c("x_prop_autos_dom","x_prop_dom_urban","x_wghtd_mean_household_income_per_capita","x_street_pop","x_land_use_mix","x_density_pop_02km_2015","x_prop_razao_dep"),
                       c("x_prop_autos_dom","x_prop_high_educ","x_prop_razao_dep","x_prop_dom_urban","x_state"),
                       c("x_prop_autos_dom","x_prop_high_educ","x_prop_dom_urban","x_pop_2010","x_wghtd_mean_household_income_per_capita","x_prop_razao_dep"),
                       c("x_prop_autos_dom","x_prop_high_educ","x_state","x_pop_2010","x_density_pop_02km_2015","x_prop_razao_dep"),
                       c("x_prop_autos_dom","x_prop_high_educ","x_pop_2010","x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2015","x_prop_razao_dep"),
                       c("x_prop_autos_dom","x_prop_high_educ","x_wghtd_mean_household_income_per_capita","x_street_pop","x_land_use_mix","x_density_pop_02km_2015","x_prop_razao_dep"),
                       c("x_prop_autos_dom","x_state","x_pop_2010","x_prop_dom_urban","x_wghtd_mean_household_income_per_capita","x_prop_high_educ"),
                       c("x_prop_autos_dom","x_state","x_pop_2010","x_prop_dom_urban","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita"),
                       c("x_prop_autos_dom","x_state","x_pop_2010","x_prop_high_educ","x_density_pop_02km_2015","x_wghtd_mean_household_income_per_capita"),
                       c("x_prop_autos_dom","x_state","x_pop_2010","x_density_pop_02km_2015","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita"),
                       c("x_prop_autos_dom","x_state","x_prop_high_educ","x_density_pop_02km_2015","x_wghtd_mean_household_income_per_capita","x_street_pop","x_land_use_mix"),
                       c("x_prop_autos_dom","x_state","x_density_pop_02km_2015","x_wghtd_mean_household_income_per_capita","x_street_pop","x_land_use_mix","x_prop_razao_dep"),
                       c("x_prop_autos_dom","x_mean_age_auto","x_wghtd_mean_household_income_per_capita"),
                       c("x_prop_dom_urban","x_prop_razao_dep","x_prop_high_educ"),
                       c("x_prop_dom_urban","x_street_pop","x_density_pop_02km_2015","x_pop_2010","x_wghtd_mean_household_income_per_capita"),
                       c("x_prop_dom_urban","x_street_pop","x_density_pop_02km_2015","x_pop_2010","x_state"),
                       c("x_prop_dom_urban","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_state"),
                       c("x_prop_dom_urban","x_mean_age_auto","x_wghtd_mean_household_income_per_capita"),
                       c("x_prop_dom_urban","x_mean_age_auto","x_pop_2010","x_state"),
                       c("x_prop_high_educ","x_street_pop","x_density_pop_02km_2015","x_pop_2010","x_wghtd_mean_household_income_per_capita"),
                       c("x_prop_high_educ","x_street_pop","x_density_pop_02km_2015","x_pop_2010","x_state"),
                       c("x_prop_high_educ","x_street_pop","x_pop_2010","x_wghtd_mean_household_income_per_capita","x_prop_dom_urban"),
                       c("x_prop_high_educ","x_street_pop","x_state","x_prop_dom_urban"),
                       c("x_prop_high_educ","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_state"),
                       c("x_prop_high_educ","x_wghtd_mean_household_income_per_capita","x_state","x_prop_dom_urban"),
                       c("x_prop_high_educ","y_energy_per_capita","x_prop_dom_urban","x_prop_razao_dep","x_street_pop","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_closeness_centrality_avg","x_density_pop_02km_2015","x_land_use_mix","x_mean_slope"),
                       c("x_prop_high_educ","y_energy_per_capita","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_mean_slope","x_prop_dom_urban","x_prop_razao_dep","x_pop_2010"),
                       c("x_prop_high_educ","y_energy_per_capita","x_wghtd_mean_household_income_per_capita","x_mean_slope","x_prop_dom_urban","x_prop_razao_dep","x_pop_2010","x_pop_growth_1975_2015"),
                       c("x_prop_high_educ","y_energy_per_capita","x_state","x_prop_dom_urban","x_prop_razao_dep"),
                       c("x_prop_high_educ","x_mean_age_auto","x_wghtd_mean_household_income_per_capita"),
                       c("x_prop_high_educ","x_mean_age_auto","x_pop_2010","x_state"),
                       c("x_prop_high_educ","x_mean_age_auto","x_state","x_prop_dom_urban"),
                       c("x_prop_high_educ","x_density_pop_02km_2015","x_pop_2010","x_prop_dom_urban"),
                       c("x_prop_high_educ","x_density_pop_02km_2015","x_state","x_prop_dom_urban"),
                       c("x_prop_razao_dep","x_state","x_prop_high_educ"),
                       c("x_prop_razao_dep","x_street_pop","x_density_pop_02km_2015","x_pop_2010","x_wghtd_mean_household_income_per_capita"),
                       c("x_prop_razao_dep","x_street_pop","x_density_pop_02km_2015","x_pop_2010","x_state"),
                       c("x_prop_razao_dep","x_street_pop","x_pop_2010","x_wghtd_mean_household_income_per_capita","x_prop_dom_urban"),
                       c("x_prop_razao_dep","x_street_pop","x_state","x_prop_dom_urban"),
                       c("x_prop_razao_dep","x_street_pop","x_prop_high_educ"),
                       c("x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_state"),
                       c("x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_state","x_prop_dom_urban"),
                       c("x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_prop_high_educ"),
                       c("x_prop_razao_dep","x_mean_age_auto","x_wghtd_mean_household_income_per_capita"),
                       c("x_prop_razao_dep","x_mean_age_auto","x_pop_2010","x_state"),
                       c("x_prop_razao_dep","x_mean_age_auto","x_state","x_prop_dom_urban"),
                       c("x_prop_razao_dep","x_mean_age_auto","x_prop_high_educ"),
                       c("x_prop_razao_dep","x_density_pop_02km_2015","x_pop_2010","x_prop_dom_urban"),
                       c("x_prop_razao_dep","x_density_pop_02km_2015","x_state","x_prop_dom_urban"),
                       c("x_prop_razao_dep","x_density_pop_02km_2015","x_prop_high_educ"),
                       c("x_state","x_street_pop","x_density_pop_02km_2015","x_pop_2010","x_wghtd_mean_household_income_per_capita"),
                       c("x_state","x_street_pop","x_pop_2010","x_wghtd_mean_household_income_per_capita","x_prop_dom_urban"),
                       c("x_state","y_energy_per_capita","x_prop_dom_urban","x_prop_razao_dep","x_street_pop","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_closeness_centrality_avg","x_density_pop_02km_2015","x_land_use_mix","x_mean_slope"),
                       c("x_state","y_energy_per_capita","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_mean_slope","x_prop_dom_urban","x_prop_razao_dep","x_pop_2010"),
                       c("x_state","y_energy_per_capita","x_wghtd_mean_household_income_per_capita","x_mean_slope","x_prop_dom_urban","x_prop_razao_dep","x_pop_2010","x_pop_growth_1975_2015"),
                       c("x_state","y_energy_per_capita","x_street_pop","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_closeness_centrality_avg","x_density_pop_02km_2015","x_land_use_mix","x_mean_slope","x_prop_dom_urban","x_prop_high_educ"),
                       c("x_state","y_energy_per_capita","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_mean_slope","x_prop_dom_urban","x_pop_2010","x_prop_high_educ"),
                       c("x_state","y_energy_per_capita","x_wghtd_mean_household_income_per_capita","x_mean_slope","x_prop_dom_urban","x_pop_2010","x_prop_high_educ","x_pop_growth_1975_2015"),
                       c("x_state","x_mean_age_auto","x_wghtd_mean_household_income_per_capita"),
                       c("x_state","x_density_pop_02km_2015","x_pop_2010","x_prop_dom_urban"),
                       c("x_street_pop","x_mean_age_auto","x_wghtd_mean_household_income_per_capita"),
                       c("x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2015","x_pop_2010","x_prop_dom_urban"),
                       c("x_wghtd_mean_household_income_per_capita","x_density_pop_02km_2015","x_state","x_pop_2010"),
                       c("x_mean_age_auto","x_density_pop_02km_2015","x_pop_2010","x_prop_dom_urban"),
                       c("x_mean_age_auto","x_density_pop_02km_2015","x_state","x_pop_2010"),
                       c("x_mean_age_auto","x_density_pop_02km_2015","x_wghtd_mean_household_income_per_capita"))
  data.frame( implication=unlist(lapply(implications,tos)),
              pvalue=unlist( lapply( implications, tst ) ) )

}
test <- testImplications( covariance.matrix = cov( df_fuel[, -c('x_state','x_region')] ),
                  sample.size = nrow(df_fuel))




setDT(test)[pvalue > 0.1] %>% nrow() / nrow(test)
#>>> cor(df_fuel$x_mean_slope , df_fuel$x_prop_razao_dep )
#>>> cor(df_fuel$x_mean_slope , df_fuel$x_prop_autos_dom )
#>>> cor(df_fuel$x_mean_slope , df_fuel$x_prop_high_educ )


cor(df_fuel$x_prop_high_educ , df_fuel$x_prop_razao_dep )

cor(df_fuel$x_mean_slope , df_fuel$x_pop_growth_1975_2015 )
cor(df_fuel$x_prop_razao_dep , df_fuel$x_pop_growth_1975_2015 )
cor(df_fuel$x_land_use_mix , df_fuel$x_density_pop_01km_2015 )
cor(df_fuel$x_circuity_avg , df_fuel$x_closeness_centrality_avg )


cor(df_fuel$x_residents_per_household , df_fuel$x_rooms_per_household )

cor(df_fuel$x_residents_per_household , df_fuel$x_prop_autos_dom )
cor(df_fuel$x_residents_per_household , df_fuel$y_energy_per_capita )


cor(df_fuel$x_prop_black , df_fuel$x_prop_dom_urban )
cor(df_fuel$x , df_fuel$x_prop_razao_dep )



# vars mais repetidas
implication <- setDT(test)[pvalue < 0.1]$implication


aaa <- test[implication %like% 'street_pop']


all_vars <- str_split(implication, pattern = ' ')
all_vars <- unlist(all_vars)
table(all_vars) %>% sort()


cor(df_raw$x_prop_autos_dom, df_raw$x_street_pop)

ggplot(data=df_fuel, aes(x=x_pop_2010, y=x_closeness_centrality_avg)) +
  geom_point() +
  geom_smooth() +
  scale_x_log10() +
  scale_y_log10() +
  theme_classic()




# dag model ----------------------------------

d <- dagitty('dag {
bb="0,0,1,1"
f_compact_contig_inter_dens [exposure,pos="0.418,0.040"]
x_circuity_avg [exposure,pos="0.425,0.232"]
x_closeness_centrality_avg [exposure,pos="0.421,0.141"]
x_density_pop_02km_2015 [exposure,pos="0.431,0.350"]
x_land_use_mix [exposure,pos="0.481,0.802"]
x_mean_age_auto [pos="0.486,0.926"]
x_mean_slope [pos="0.287,0.262"]
x_pop_2010 [adjusted,pos="0.092,0.276"]
x_pop_growth_1975_2015 [pos="0.076,0.040"]
x_prop_autos_dom [pos="0.556,0.652"]
x_prop_dom_urban [pos="0.284,0.434"]
x_prop_high_educ [pos="0.351,0.516"]
x_prop_razao_dep [pos="0.427,0.574"]
x_state [pos="0.144,0.538"]
x_street_pop [pos="0.549,0.469"]
x_wghtd_mean_household_income_per_capita [pos="0.098,0.780"]
y_energy_per_capita [outcome,pos="0.854,0.443"]
f_compact_contig_inter_dens -> y_energy_per_capita
x_circuity_avg -> y_energy_per_capita
x_closeness_centrality_avg -> y_energy_per_capita
x_density_pop_02km_2015 -> x_land_use_mix
x_density_pop_02km_2015 -> x_prop_autos_dom
x_density_pop_02km_2015 -> x_street_pop
x_density_pop_02km_2015 -> y_energy_per_capita
x_land_use_mix -> x_prop_autos_dom
x_land_use_mix -> y_energy_per_capita
x_mean_age_auto -> y_energy_per_capita
x_mean_slope -> x_circuity_avg
x_mean_slope -> y_energy_per_capita
x_pop_2010 -> f_compact_contig_inter_dens
x_pop_2010 -> x_closeness_centrality_avg
x_pop_2010 -> x_density_pop_02km_2015
x_pop_2010 -> x_land_use_mix
x_pop_2010 -> x_prop_dom_urban
x_pop_2010 -> x_street_pop
x_pop_2010 -> x_wghtd_mean_household_income_per_capita
x_pop_growth_1975_2015 -> f_compact_contig_inter_dens
x_pop_growth_1975_2015 -> x_pop_2010
x_pop_growth_1975_2015 -> x_prop_dom_urban
x_prop_autos_dom -> y_energy_per_capita
x_prop_dom_urban -> x_density_pop_02km_2015
x_prop_dom_urban -> x_prop_high_educ
x_prop_dom_urban -> y_energy_per_capita
x_prop_high_educ -> x_prop_razao_dep
x_prop_razao_dep -> x_prop_autos_dom
x_prop_razao_dep -> y_energy_per_capita
x_state -> x_mean_slope
x_state -> x_prop_dom_urban
x_state -> x_prop_high_educ
x_state -> x_wghtd_mean_household_income_per_capita
x_street_pop -> x_prop_autos_dom
x_street_pop -> y_energy_per_capita
x_wghtd_mean_household_income_per_capita -> x_mean_age_auto
x_wghtd_mean_household_income_per_capita -> x_prop_autos_dom
x_wghtd_mean_household_income_per_capita -> x_street_pop
x_wghtd_mean_household_income_per_capita -> y_energy_per_capita
}')



#### determine adjustments ---------------------


# generate model specifications
generate_model <- function(exposure_vars=NULL){

cols <- dagitty::adjustmentSets(d,
                                exposure = exposure_vars,
                                type = 'canonical',
                                effect = 'total')

# control vars
cols <- as.character(cols)
cols <- gsub('"', '', cols, fixed=TRUE)
cols <- gsub("^c\\(|\\)$", "", cols)
cols <- gsub(",", "+", gsub("\\.", "", cols))

# exposure vars
cols_exp <- exposure_vars
if( is.null(cols_exp)) { cols_exp <- dagitty::exposures(d) }
cols_exp <- paste0(cols_exp, collapse = '+')

# model
model <- paste0('y_energy_per_capita ~', cols_exp, '+', cols )
return(model)
}

# make sure state is categorical
df_fuel$x_state <- as.factor(df_fuel$x_state)

# run all models
model_all       <- generate_model()
model_landuse   <- generate_model(exposure_vars = 'x_land_use_mix')
model_density   <- generate_model(exposure_vars = 'x_density_pop_02km_2015')
model_fcompact  <- generate_model(exposure_vars = 'f_compact_contig_inter_dens')
model_circuity  <- generate_model(exposure_vars = 'x_circuity_avg')
model_closeness <- generate_model(exposure_vars = 'x_closeness_centrality_avg')
# model_residents <- generate_model(exposure_vars = 'x_residents_per_household')
# model_rooms <- generate_model(exposure_vars = 'x_rooms_per_household')



model_all       <- lm( model_all      , data=df_fuel)
model_landuse   <- lm( model_landuse  , data=df_fuel)
model_density   <- lm( model_density  , data=df_fuel)
model_fcompact  <- lm( model_fcompact , data=df_fuel)
model_circuity  <- lm( model_circuity , data=df_fuel)
model_closeness <- lm( model_closeness, data=df_fuel)
# model_rooms <- lm( model_rooms, data=df_fuel)

all_models_output <- list(model_all, model_landuse , model_density, model_fcompact, model_circuity, model_closeness)

# get AIC values
aic_list <- lapply(all_models_output, FUN = function(i){round(AIC(i),1)})

stargazer(
  all_models_output,
  out = "energy_dag_canonical_02.html",
  column.labels= c('all', 'landuse', 'density', 'fcompact', 'circuity', 'closenes'),
  type = 'text',
  p.auto=F,
  ci=F,
  t.auto=F,
  add.lines = list(c('AIC', paste0(aic_list)))
  )



df_log$region <- substring(df_log$i_code_urban_concentration, 1,1)

ggplot() +
  geom_boxplot(data=df_log, aes(x=region, y=x_mean_slope))

ggplot() +
  geom_boxplot(data=df_log, aes(x=region, y=x_pop_2010  ))

ggplot() +
  geom_point(data=df_log, aes(x=x_prop_razao_dep, y=x_mean_slope))

cor(df_log$x_mean_slope, df_log$x_wghtd_mean_household_income_per_capita)

 cor(df_fuel$x_closeness_centrality_avg , df_fuel$x_pop_2010 )
 cor(df_fuel$x_mean_slope , df_fuel$x_prop_autos_dom )
 cor(df_fuel$x_circuity_avg , df_fuel$x_pop_2010 )

