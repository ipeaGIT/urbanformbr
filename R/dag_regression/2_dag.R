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
                       c("f_compact_contig_inter_dens","x_land_use_mix","x_density_pop_02km","x_pop_2010"),
                       c("f_compact_contig_inter_dens","x_land_use_mix","x_pop_2010","x_prop_dom_urban"),
                       c("f_compact_contig_inter_dens","x_land_use_mix","x_pop_2010","x_pop_growth_1975_2015"),
                       c("f_compact_contig_inter_dens","x_mean_age_auto","x_wghtd_mean_household_income_per_capita"),
                       c("f_compact_contig_inter_dens","x_mean_age_auto","x_pop_2010"),
                       c("f_compact_contig_inter_dens","x_mean_slope"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_density_pop_02km","x_land_use_mix","x_prop_razao_dep","x_street_pop","x_wghtd_mean_household_income_per_capita"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_street_pop","x_wghtd_mean_household_income_per_capita","x_density_pop_02km","x_land_use_mix","x_prop_high_educ"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_prop_dom_urban","x_state","x_street_pop","x_wghtd_mean_household_income_per_capita","x_density_pop_02km","x_land_use_mix"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_density_pop_02km","x_prop_razao_dep","x_pop_2010"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_prop_razao_dep","x_pop_2010","x_prop_dom_urban"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_density_pop_02km","x_pop_2010","x_prop_high_educ"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_prop_high_educ","x_prop_dom_urban"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_density_pop_02km","x_prop_razao_dep","x_pop_2010","x_state"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_density_pop_02km","x_pop_2010","x_state","x_prop_high_educ"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_prop_dom_urban","x_state","x_pop_2010"),
                       c("f_compact_contig_inter_dens","x_prop_autos_dom","x_pop_2010","x_pop_growth_1975_2015"),
                       c("f_compact_contig_inter_dens","x_prop_dom_urban","x_pop_2010","x_pop_growth_1975_2015"),
                       c("f_compact_contig_inter_dens","x_prop_high_educ","x_prop_dom_urban","x_state"),
                       c("f_compact_contig_inter_dens","x_prop_high_educ","x_pop_2010","x_pop_growth_1975_2015"),
                       c("f_compact_contig_inter_dens","x_prop_razao_dep","x_prop_high_educ"),
                       c("f_compact_contig_inter_dens","x_prop_razao_dep","x_prop_dom_urban","x_state"),
                       c("f_compact_contig_inter_dens","x_prop_razao_dep","x_pop_2010","x_pop_growth_1975_2015"),
                       c("f_compact_contig_inter_dens","x_state"),
                       c("f_compact_contig_inter_dens","x_street_pop","x_density_pop_02km","x_pop_2010","x_wghtd_mean_household_income_per_capita"),
                       c("f_compact_contig_inter_dens","x_street_pop","x_pop_2010","x_wghtd_mean_household_income_per_capita","x_prop_dom_urban"),
                       c("f_compact_contig_inter_dens","x_street_pop","x_density_pop_02km","x_pop_2010","x_state"),
                       c("f_compact_contig_inter_dens","x_street_pop","x_pop_2010","x_state","x_prop_dom_urban"),
                       c("f_compact_contig_inter_dens","x_street_pop","x_pop_2010","x_pop_growth_1975_2015"),
                       c("f_compact_contig_inter_dens","x_wghtd_mean_household_income_per_capita","x_pop_2010"),
                       c("f_compact_contig_inter_dens","x_density_pop_02km","x_pop_2010","x_prop_dom_urban"),
                       c("f_compact_contig_inter_dens","x_density_pop_02km","x_pop_2010","x_pop_growth_1975_2015"),
                       c("x_circuity_avg","x_closeness_centrality_avg"),
                       c("x_circuity_avg","x_land_use_mix","x_density_pop_02km","x_pop_2010"),
                       c("x_circuity_avg","x_land_use_mix","x_pop_2010","x_prop_dom_urban"),
                       c("x_circuity_avg","x_land_use_mix","x_state"),
                       c("x_circuity_avg","x_land_use_mix","x_mean_slope"),
                       c("x_circuity_avg","x_mean_age_auto","x_wghtd_mean_household_income_per_capita"),
                       c("x_circuity_avg","x_mean_age_auto","x_state"),
                       c("x_circuity_avg","x_mean_age_auto","x_mean_slope"),
                       c("x_circuity_avg","x_pop_2010"),
                       c("x_circuity_avg","x_pop_growth_1975_2015"),
                       c("x_circuity_avg","x_prop_autos_dom","x_density_pop_02km","x_land_use_mix","x_prop_razao_dep","x_street_pop","x_wghtd_mean_household_income_per_capita"),
                       c("x_circuity_avg","x_prop_autos_dom","x_street_pop","x_wghtd_mean_household_income_per_capita","x_density_pop_02km","x_land_use_mix","x_prop_high_educ"),
                       c("x_circuity_avg","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_density_pop_02km","x_prop_razao_dep","x_pop_2010"),
                       c("x_circuity_avg","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_density_pop_02km","x_pop_2010","x_prop_high_educ"),
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
                       c("x_circuity_avg","x_street_pop","x_density_pop_02km","x_pop_2010","x_wghtd_mean_household_income_per_capita"),
                       c("x_circuity_avg","x_street_pop","x_pop_2010","x_wghtd_mean_household_income_per_capita","x_prop_dom_urban"),
                       c("x_circuity_avg","x_street_pop","x_state"),
                       c("x_circuity_avg","x_street_pop","x_mean_slope"),
                       c("x_circuity_avg","x_wghtd_mean_household_income_per_capita","x_state"),
                       c("x_circuity_avg","x_wghtd_mean_household_income_per_capita","x_mean_slope"),
                       c("x_circuity_avg","x_density_pop_02km","x_pop_2010","x_prop_dom_urban"),
                       c("x_circuity_avg","x_density_pop_02km","x_state"),
                       c("x_circuity_avg","x_density_pop_02km","x_mean_slope"),
                       c("x_closeness_centrality_avg","x_land_use_mix","x_pop_2010"),
                       c("x_closeness_centrality_avg","x_mean_age_auto","x_wghtd_mean_household_income_per_capita"),
                       c("x_closeness_centrality_avg","x_mean_age_auto","x_pop_2010"),
                       c("x_closeness_centrality_avg","x_mean_slope"),
                       c("x_closeness_centrality_avg","x_pop_growth_1975_2015","x_pop_2010"),
                       c("x_closeness_centrality_avg","x_prop_autos_dom","x_density_pop_02km","x_land_use_mix","x_prop_razao_dep","x_street_pop","x_wghtd_mean_household_income_per_capita"),
                       c("x_closeness_centrality_avg","x_prop_autos_dom","x_street_pop","x_wghtd_mean_household_income_per_capita","x_density_pop_02km","x_land_use_mix","x_prop_high_educ"),
                       c("x_closeness_centrality_avg","x_prop_autos_dom","x_prop_dom_urban","x_state","x_street_pop","x_wghtd_mean_household_income_per_capita","x_density_pop_02km","x_land_use_mix"),
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
                       c("x_closeness_centrality_avg","x_density_pop_02km","x_pop_2010"),
                       c("x_land_use_mix","x_mean_age_auto","x_wghtd_mean_household_income_per_capita"),
                       c("x_land_use_mix","x_mean_age_auto","x_pop_2010","x_state"),
                       c("x_land_use_mix","x_mean_age_auto","x_pop_2010","x_prop_dom_urban"),
                       c("x_land_use_mix","x_mean_age_auto","x_pop_2010","x_density_pop_02km"),
                       c("x_land_use_mix","x_mean_slope","x_state"),
                       c("x_land_use_mix","x_mean_slope","x_pop_2010","x_prop_dom_urban"),
                       c("x_land_use_mix","x_mean_slope","x_pop_2010","x_density_pop_02km"),
                       c("x_land_use_mix","x_pop_growth_1975_2015","x_prop_dom_urban","x_pop_2010"),
                       c("x_land_use_mix","x_pop_growth_1975_2015","x_density_pop_02km","x_pop_2010"),
                       c("x_land_use_mix","x_prop_dom_urban","x_pop_2010","x_density_pop_02km"),
                       c("x_land_use_mix","x_prop_high_educ","x_prop_dom_urban","x_state"),
                       c("x_land_use_mix","x_prop_high_educ","x_prop_dom_urban","x_pop_2010"),
                       c("x_land_use_mix","x_prop_high_educ","x_pop_2010","x_density_pop_02km"),
                       c("x_land_use_mix","x_prop_razao_dep","x_prop_high_educ"),
                       c("x_land_use_mix","x_prop_razao_dep","x_prop_dom_urban","x_state"),
                       c("x_land_use_mix","x_prop_razao_dep","x_prop_dom_urban","x_pop_2010"),
                       c("x_land_use_mix","x_prop_razao_dep","x_pop_2010","x_density_pop_02km"),
                       c("x_land_use_mix","x_state","x_pop_2010","x_prop_dom_urban"),
                       c("x_land_use_mix","x_state","x_pop_2010","x_density_pop_02km"),
                       c("x_land_use_mix","x_street_pop","x_pop_2010","x_density_pop_02km"),
                       c("x_land_use_mix","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_state"),
                       c("x_land_use_mix","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_prop_dom_urban"),
                       c("x_land_use_mix","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_density_pop_02km"),
                       c("x_mean_age_auto","x_mean_slope","x_state"),
                       c("x_mean_age_auto","x_mean_slope","x_wghtd_mean_household_income_per_capita"),
                       c("x_mean_age_auto","x_pop_2010","x_wghtd_mean_household_income_per_capita"),
                       c("x_mean_age_auto","x_pop_growth_1975_2015","x_pop_2010"),
                       c("x_mean_age_auto","x_pop_growth_1975_2015","x_wghtd_mean_household_income_per_capita"),
                       c("x_mean_age_auto","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita"),
                       c("x_mean_age_auto","x_prop_dom_urban","x_state","x_pop_2010"),
                       c("x_mean_age_auto","x_prop_dom_urban","x_wghtd_mean_household_income_per_capita"),
                       c("x_mean_age_auto","x_prop_high_educ","x_prop_dom_urban","x_state"),
                       c("x_mean_age_auto","x_prop_high_educ","x_state","x_pop_2010"),
                       c("x_mean_age_auto","x_prop_high_educ","x_wghtd_mean_household_income_per_capita"),
                       c("x_mean_age_auto","x_prop_razao_dep","x_prop_high_educ"),
                       c("x_mean_age_auto","x_prop_razao_dep","x_prop_dom_urban","x_state"),
                       c("x_mean_age_auto","x_prop_razao_dep","x_state","x_pop_2010"),
                       c("x_mean_age_auto","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita"),
                       c("x_mean_age_auto","x_state","x_wghtd_mean_household_income_per_capita"),
                       c("x_mean_age_auto","x_street_pop","x_wghtd_mean_household_income_per_capita"),
                       c("x_mean_age_auto","x_density_pop_02km","x_pop_2010","x_prop_dom_urban"),
                       c("x_mean_age_auto","x_density_pop_02km","x_state","x_pop_2010"),
                       c("x_mean_age_auto","x_density_pop_02km","x_wghtd_mean_household_income_per_capita"),
                       c("x_mean_slope","x_pop_2010"),
                       c("x_mean_slope","x_pop_growth_1975_2015"),
                       c("x_mean_slope","x_prop_autos_dom","x_density_pop_02km","x_land_use_mix","x_prop_razao_dep","x_street_pop","x_wghtd_mean_household_income_per_capita"),
                       c("x_mean_slope","x_prop_autos_dom","x_street_pop","x_wghtd_mean_household_income_per_capita","x_density_pop_02km","x_land_use_mix","x_prop_high_educ"),
                       c("x_mean_slope","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_density_pop_02km","x_prop_razao_dep","x_pop_2010"),
                       c("x_mean_slope","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_density_pop_02km","x_pop_2010","x_prop_high_educ"),
                       c("x_mean_slope","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_prop_razao_dep","x_pop_2010","x_prop_dom_urban"),
                       c("x_mean_slope","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_prop_high_educ","x_prop_dom_urban"),
                       c("x_mean_slope","x_prop_autos_dom","x_state"),
                       c("x_mean_slope","x_prop_dom_urban","x_state"),
                       c("x_mean_slope","x_prop_high_educ","x_state"),
                       c("x_mean_slope","x_prop_razao_dep","x_prop_high_educ"),
                       c("x_mean_slope","x_prop_razao_dep","x_state"),
                       c("x_mean_slope","x_street_pop","x_density_pop_02km","x_pop_2010","x_wghtd_mean_household_income_per_capita"),
                       c("x_mean_slope","x_street_pop","x_pop_2010","x_wghtd_mean_household_income_per_capita","x_prop_dom_urban"),
                       c("x_mean_slope","x_street_pop","x_state"),
                       c("x_mean_slope","x_wghtd_mean_household_income_per_capita","x_state"),
                       c("x_mean_slope","x_density_pop_02km","x_pop_2010","x_prop_dom_urban"),
                       c("x_mean_slope","x_density_pop_02km","x_state"),
                       c("x_pop_2010","x_prop_autos_dom","x_density_pop_02km","x_land_use_mix","x_prop_razao_dep","x_street_pop","x_wghtd_mean_household_income_per_capita"),
                       c("x_pop_2010","x_prop_autos_dom","x_street_pop","x_wghtd_mean_household_income_per_capita","x_density_pop_02km","x_land_use_mix","x_prop_high_educ"),
                       c("x_pop_2010","x_prop_autos_dom","x_prop_dom_urban","x_state","x_street_pop","x_wghtd_mean_household_income_per_capita","x_density_pop_02km","x_land_use_mix"),
                       c("x_pop_2010","x_prop_high_educ","x_prop_dom_urban","x_state"),
                       c("x_pop_2010","x_prop_razao_dep","x_prop_high_educ"),
                       c("x_pop_2010","x_prop_razao_dep","x_prop_dom_urban","x_state"),
                       c("x_pop_2010","x_state"),
                       c("x_pop_2010","y_energy_per_capita","x_prop_dom_urban","x_prop_razao_dep","x_street_pop","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_closeness_centrality_avg","x_density_pop_02km","x_land_use_mix","x_mean_slope"),
                       c("x_pop_2010","y_energy_per_capita","x_street_pop","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_closeness_centrality_avg","x_density_pop_02km","x_land_use_mix","x_mean_slope","x_prop_dom_urban","x_prop_high_educ"),
                       c("x_pop_2010","y_energy_per_capita","x_prop_dom_urban","x_state","x_street_pop","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_closeness_centrality_avg","x_density_pop_02km","x_land_use_mix"),
                       c("x_pop_growth_1975_2015","x_prop_autos_dom","x_density_pop_02km","x_land_use_mix","x_prop_razao_dep","x_street_pop","x_wghtd_mean_household_income_per_capita"),
                       c("x_pop_growth_1975_2015","x_prop_autos_dom","x_street_pop","x_wghtd_mean_household_income_per_capita","x_density_pop_02km","x_land_use_mix","x_prop_high_educ"),
                       c("x_pop_growth_1975_2015","x_prop_autos_dom","x_prop_dom_urban","x_state","x_street_pop","x_wghtd_mean_household_income_per_capita","x_density_pop_02km","x_land_use_mix"),
                       c("x_pop_growth_1975_2015","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_density_pop_02km","x_prop_razao_dep","x_pop_2010"),
                       c("x_pop_growth_1975_2015","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_prop_razao_dep","x_pop_2010","x_prop_dom_urban"),
                       c("x_pop_growth_1975_2015","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_density_pop_02km","x_pop_2010","x_prop_high_educ"),
                       c("x_pop_growth_1975_2015","x_prop_autos_dom","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_prop_high_educ","x_prop_dom_urban"),
                       c("x_pop_growth_1975_2015","x_prop_autos_dom","x_density_pop_02km","x_prop_razao_dep","x_pop_2010","x_state"),
                       c("x_pop_growth_1975_2015","x_prop_autos_dom","x_density_pop_02km","x_pop_2010","x_state","x_prop_high_educ"),
                       c("x_pop_growth_1975_2015","x_prop_autos_dom","x_prop_dom_urban","x_state","x_pop_2010"),
                       c("x_pop_growth_1975_2015","x_prop_high_educ","x_prop_dom_urban","x_state"),
                       c("x_pop_growth_1975_2015","x_prop_razao_dep","x_prop_high_educ"),
                       c("x_pop_growth_1975_2015","x_prop_razao_dep","x_prop_dom_urban","x_state"),
                       c("x_pop_growth_1975_2015","x_state"),
                       c("x_pop_growth_1975_2015","x_street_pop","x_density_pop_02km","x_pop_2010","x_wghtd_mean_household_income_per_capita"),
                       c("x_pop_growth_1975_2015","x_street_pop","x_pop_2010","x_wghtd_mean_household_income_per_capita","x_prop_dom_urban"),
                       c("x_pop_growth_1975_2015","x_street_pop","x_density_pop_02km","x_pop_2010","x_state"),
                       c("x_pop_growth_1975_2015","x_street_pop","x_pop_2010","x_state","x_prop_dom_urban"),
                       c("x_pop_growth_1975_2015","x_wghtd_mean_household_income_per_capita","x_pop_2010"),
                       c("x_pop_growth_1975_2015","y_energy_per_capita","x_prop_dom_urban","x_prop_razao_dep","x_street_pop","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_closeness_centrality_avg","x_density_pop_02km","x_land_use_mix","x_mean_slope"),
                       c("x_pop_growth_1975_2015","y_energy_per_capita","x_street_pop","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_closeness_centrality_avg","x_density_pop_02km","x_land_use_mix","x_mean_slope","x_prop_dom_urban","x_prop_high_educ"),
                       c("x_pop_growth_1975_2015","y_energy_per_capita","x_prop_dom_urban","x_state","x_street_pop","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_closeness_centrality_avg","x_density_pop_02km","x_land_use_mix"),
                       c("x_pop_growth_1975_2015","y_energy_per_capita","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_mean_slope","x_prop_dom_urban","x_prop_razao_dep","x_pop_2010"),
                       c("x_pop_growth_1975_2015","y_energy_per_capita","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_mean_slope","x_prop_dom_urban","x_pop_2010","x_prop_high_educ"),
                       c("x_pop_growth_1975_2015","y_energy_per_capita","x_prop_dom_urban","x_state","f_compact_contig_inter_dens","x_pop_2010"),
                       c("x_pop_growth_1975_2015","x_density_pop_02km","x_pop_2010","x_prop_dom_urban"),
                       c("x_prop_autos_dom","x_prop_dom_urban","x_state","x_pop_2010","x_prop_high_educ","x_density_pop_02km"),
                       c("x_prop_autos_dom","x_prop_dom_urban","x_state","x_pop_2010","x_prop_razao_dep","x_density_pop_02km"),
                       c("x_prop_autos_dom","x_prop_dom_urban","x_wghtd_mean_household_income_per_capita","x_prop_high_educ","x_pop_2010","x_density_pop_02km"),
                       c("x_prop_autos_dom","x_prop_dom_urban","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_prop_razao_dep","x_density_pop_02km"),
                       c("x_prop_autos_dom","x_prop_dom_urban","x_wghtd_mean_household_income_per_capita","x_prop_high_educ","x_street_pop","x_land_use_mix","x_density_pop_02km"),
                       c("x_prop_autos_dom","x_prop_dom_urban","x_wghtd_mean_household_income_per_capita","x_street_pop","x_land_use_mix","x_density_pop_02km","x_prop_razao_dep"),
                       c("x_prop_autos_dom","x_prop_high_educ","x_prop_razao_dep","x_prop_dom_urban","x_state"),
                       c("x_prop_autos_dom","x_prop_high_educ","x_prop_dom_urban","x_pop_2010","x_wghtd_mean_household_income_per_capita","x_prop_razao_dep"),
                       c("x_prop_autos_dom","x_prop_high_educ","x_state","x_pop_2010","x_density_pop_02km","x_prop_razao_dep"),
                       c("x_prop_autos_dom","x_prop_high_educ","x_pop_2010","x_wghtd_mean_household_income_per_capita","x_density_pop_02km","x_prop_razao_dep"),
                       c("x_prop_autos_dom","x_prop_high_educ","x_wghtd_mean_household_income_per_capita","x_street_pop","x_land_use_mix","x_density_pop_02km","x_prop_razao_dep"),
                       c("x_prop_autos_dom","x_state","x_pop_2010","x_prop_dom_urban","x_wghtd_mean_household_income_per_capita","x_prop_high_educ"),
                       c("x_prop_autos_dom","x_state","x_pop_2010","x_prop_dom_urban","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita"),
                       c("x_prop_autos_dom","x_state","x_pop_2010","x_prop_high_educ","x_density_pop_02km","x_wghtd_mean_household_income_per_capita"),
                       c("x_prop_autos_dom","x_state","x_pop_2010","x_density_pop_02km","x_prop_razao_dep","x_wghtd_mean_household_income_per_capita"),
                       c("x_prop_autos_dom","x_state","x_prop_high_educ","x_density_pop_02km","x_wghtd_mean_household_income_per_capita","x_street_pop","x_land_use_mix"),
                       c("x_prop_autos_dom","x_state","x_density_pop_02km","x_wghtd_mean_household_income_per_capita","x_street_pop","x_land_use_mix","x_prop_razao_dep"),
                       c("x_prop_dom_urban","x_prop_razao_dep","x_prop_high_educ"),
                       c("x_prop_dom_urban","x_street_pop","x_density_pop_02km","x_pop_2010","x_wghtd_mean_household_income_per_capita"),
                       c("x_prop_dom_urban","x_street_pop","x_density_pop_02km","x_pop_2010","x_state"),
                       c("x_prop_dom_urban","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_state"),
                       c("x_prop_high_educ","x_street_pop","x_density_pop_02km","x_pop_2010","x_wghtd_mean_household_income_per_capita"),
                       c("x_prop_high_educ","x_street_pop","x_density_pop_02km","x_pop_2010","x_state"),
                       c("x_prop_high_educ","x_street_pop","x_pop_2010","x_wghtd_mean_household_income_per_capita","x_prop_dom_urban"),
                       c("x_prop_high_educ","x_street_pop","x_state","x_prop_dom_urban"),
                       c("x_prop_high_educ","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_state"),
                       c("x_prop_high_educ","x_wghtd_mean_household_income_per_capita","x_state","x_prop_dom_urban"),
                       c("x_prop_high_educ","y_energy_per_capita","x_prop_dom_urban","x_prop_razao_dep","x_street_pop","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_closeness_centrality_avg","x_density_pop_02km","x_land_use_mix","x_mean_slope"),
                       c("x_prop_high_educ","y_energy_per_capita","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_mean_slope","x_prop_dom_urban","x_prop_razao_dep","x_pop_2010"),
                       c("x_prop_high_educ","y_energy_per_capita","x_wghtd_mean_household_income_per_capita","x_mean_slope","x_prop_dom_urban","x_prop_razao_dep","x_pop_2010","x_pop_growth_1975_2015"),
                       c("x_prop_high_educ","y_energy_per_capita","x_state","x_prop_dom_urban","x_prop_razao_dep"),
                       c("x_prop_high_educ","x_density_pop_02km","x_pop_2010","x_prop_dom_urban"),
                       c("x_prop_high_educ","x_density_pop_02km","x_state","x_prop_dom_urban"),
                       c("x_prop_razao_dep","x_state","x_prop_high_educ"),
                       c("x_prop_razao_dep","x_street_pop","x_density_pop_02km","x_pop_2010","x_wghtd_mean_household_income_per_capita"),
                       c("x_prop_razao_dep","x_street_pop","x_density_pop_02km","x_pop_2010","x_state"),
                       c("x_prop_razao_dep","x_street_pop","x_pop_2010","x_wghtd_mean_household_income_per_capita","x_prop_dom_urban"),
                       c("x_prop_razao_dep","x_street_pop","x_state","x_prop_dom_urban"),
                       c("x_prop_razao_dep","x_street_pop","x_prop_high_educ"),
                       c("x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_pop_2010","x_state"),
                       c("x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_state","x_prop_dom_urban"),
                       c("x_prop_razao_dep","x_wghtd_mean_household_income_per_capita","x_prop_high_educ"),
                       c("x_prop_razao_dep","x_density_pop_02km","x_pop_2010","x_prop_dom_urban"),
                       c("x_prop_razao_dep","x_density_pop_02km","x_state","x_prop_dom_urban"),
                       c("x_prop_razao_dep","x_density_pop_02km","x_prop_high_educ"),
                       c("x_state","x_street_pop","x_density_pop_02km","x_pop_2010","x_wghtd_mean_household_income_per_capita"),
                       c("x_state","x_street_pop","x_pop_2010","x_wghtd_mean_household_income_per_capita","x_prop_dom_urban"),
                       c("x_state","y_energy_per_capita","x_prop_dom_urban","x_prop_razao_dep","x_street_pop","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_closeness_centrality_avg","x_density_pop_02km","x_land_use_mix","x_mean_slope"),
                       c("x_state","y_energy_per_capita","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_mean_slope","x_prop_dom_urban","x_prop_razao_dep","x_pop_2010"),
                       c("x_state","y_energy_per_capita","x_wghtd_mean_household_income_per_capita","x_mean_slope","x_prop_dom_urban","x_prop_razao_dep","x_pop_2010","x_pop_growth_1975_2015"),
                       c("x_state","y_energy_per_capita","x_street_pop","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_closeness_centrality_avg","x_density_pop_02km","x_land_use_mix","x_mean_slope","x_prop_dom_urban","x_prop_high_educ"),
                       c("x_state","y_energy_per_capita","x_wghtd_mean_household_income_per_capita","f_compact_contig_inter_dens","x_mean_slope","x_prop_dom_urban","x_pop_2010","x_prop_high_educ"),
                       c("x_state","y_energy_per_capita","x_wghtd_mean_household_income_per_capita","x_mean_slope","x_prop_dom_urban","x_pop_2010","x_prop_high_educ","x_pop_growth_1975_2015"),
                       c("x_state","x_density_pop_02km","x_pop_2010","x_prop_dom_urban"),
                       c("x_wghtd_mean_household_income_per_capita","x_density_pop_02km","x_pop_2010","x_prop_dom_urban"),
                       c("x_wghtd_mean_household_income_per_capita","x_density_pop_02km","x_state","x_pop_2010"))
  data.frame( implication=unlist(lapply(implications,tos)),
              pvalue=unlist( lapply( implications, tst ) ) )

}
test <- testImplications( covariance.matrix = cov( df_fuel ),
                  sample.size = nrow(df_fuel))




setDT(test)[pvalue > 0.1] %>% nrow() / nrow(test)
#>>> cor(df_fuel$x_mean_slope , df_fuel$x_prop_razao_dep )
#>>> cor(df_fuel$x_mean_slope , df_fuel$x_prop_autos_dom )
#>>> cor(df_fuel$x_mean_slope , df_fuel$x_prop_high_educ )




# vars mais repetidas
implication <- setDT(test)[pvalue < 0.1]$implication


aaa <- test[implication %like% 'street_pop']


all_vars <- str_split(implication, pattern = ' ')
all_vars <- unlist(all_vars)
table(all_vars) %>% sort()



df_fuel$x_wghtd_mean_household_income_per_capita

ggplot(data=df_fuel, aes(y=y_energy_per_capita, x=x_wghtd_mean_household_income_per_capita)) +
  geom_point() +
  geom_smooth() +
  # scale_x_log10() +
  # scale_y_log10() +
  theme_classic()


ggplot(data=df_fuel, aes(x=y_energy_per_capita)) +
  geom_density()


# dag model ----------------------------------

d <- dagitty('dag {
bb="0,0,1,1"
f_compact_contig_inter_dens [exposure,pos="0.418,0.040"]
x_circuity_avg [exposure,pos="0.425,0.232"]
x_closeness_centrality_avg [exposure,pos="0.421,0.141"]
x_density_pop_02km [exposure,pos="0.431,0.350"]
x_land_use_mix [exposure,pos="0.481,0.802"]
x_mean_age_auto [pos="0.486,0.926"]
x_mean_slope [pos="0.287,0.262"]
x_pop_2010 [pos="0.092,0.276"]
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
x_density_pop_02km -> x_land_use_mix
x_density_pop_02km -> x_prop_autos_dom
x_density_pop_02km -> x_street_pop
x_density_pop_02km -> y_energy_per_capita
x_land_use_mix -> x_prop_autos_dom
x_land_use_mix -> y_energy_per_capita
x_mean_age_auto -> y_energy_per_capita
x_mean_slope -> x_circuity_avg
x_mean_slope -> y_energy_per_capita
x_pop_2010 -> f_compact_contig_inter_dens
x_pop_2010 -> x_closeness_centrality_avg
x_pop_2010 -> x_density_pop_02km
x_pop_2010 -> x_land_use_mix
x_pop_2010 -> x_prop_dom_urban
x_pop_2010 -> x_street_pop
x_pop_2010 -> x_wghtd_mean_household_income_per_capita
x_pop_growth_1975_2015 -> f_compact_contig_inter_dens
x_pop_growth_1975_2015 -> x_pop_2010
x_pop_growth_1975_2015 -> x_prop_dom_urban
x_prop_autos_dom -> y_energy_per_capita
x_prop_dom_urban -> x_density_pop_02km
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
                                type = 'minimal',
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

# generate all model specifications
model_spec_all       <- generate_model()
model_spec_landuse   <- generate_model(exposure_vars = 'x_land_use_mix')
model_spec_density   <- generate_model(exposure_vars = 'x_density_pop_02km')
model_spec_fcompact  <- generate_model(exposure_vars = 'f_compact_contig_inter_dens')
model_spec_circuity  <- generate_model(exposure_vars = 'x_circuity_avg')
model_spec_closeness <- generate_model(exposure_vars = 'x_closeness_centrality_avg')
# model_residents <- generate_model(exposure_vars = 'x_residents_per_household')
# model_rooms <- generate_model(exposure_vars = 'x_rooms_per_household')

all_models_specs <- list(model_spec_all,
                         model_spec_landuse,
                         model_spec_density,
                         model_spec_fcompact,
                         model_spec_circuity,
                         model_spec_closeness)


# run all models
model_output_all       <- lm( formula(model_spec_all)      , data=df_fuel )
model_output_landuse   <- lm( formula(model_spec_landuse)  , data=df_fuel )
model_output_density   <- lm( formula(model_spec_density)  , data=df_fuel )
model_output_fcompact  <- lm( formula(model_spec_fcompact) , data=df_fuel )
model_output_circuity  <- lm( formula(model_spec_circuity) , data=df_fuel )
model_output_closeness <- lm( formula(model_spec_closeness), data=df_fuel )
# model_rooms <- lm( model_rooms, data=df_fuel)

all_models_output <- list(model_output_all,
                          model_output_landuse,
                          model_output_density,
                          model_output_fcompact,
                          model_output_circuity,
                          model_output_closeness)

# get AIC values
aic_list <- lapply(all_models_output, FUN = function(i){round(AIC(i),1)})

stargazer(
  all_models_output,
  out = "./output/regression_table_energy_dag_lm_sem-fronteira.html",
  column.labels= c('all', 'landuse', 'density', 'fcompact', 'circuity', 'closenes'),
  type = 'text',
  p.auto=F,
  ci=F,
  t.auto=F,
  add.lines = list(c('AIC', paste0(aic_list)))
  )


urbanformdf

### check  heteroscedasticity-------------

performance::check_heteroscedasticity(model_all)


# There is heteroscedasticity if p-values are smaller than 0.05
lmtest::bptest(model_all) # Breusch-Pagan test
car::ncvTest(model_all)  # NCV Test

# top and bottom left: if cloud of points are random, then no heteroscedasticity
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(model_all)

# in case there is heteroscedasticity, use ols with robust standard errors
  library(estimatr)
  library(car)

formula(model_spec_all[3])


mrobust <- lm(y_energy_per_capita ~f_compact_contig_inter_dens+x_circuity_avg+x_closeness_centrality_avg+x_density_pop_02km+x_land_use_mix+x_mean_slope+ x_pop_2010+ x_prop_dom_urban +x_state, data = df_fuel)
mrobust <- estimatr::lm_robust(y_energy_per_capita ~f_compact_contig_inter_dens+x_circuity_avg+x_closeness_centrality_avg+x_density_pop_02km+x_land_use_mix+x_mean_slope+ x_pop_2010+ x_prop_dom_urban, fixed_effects= ~x_state, data = df_fuel, se_type = "stata")
summary(mrobust)

  mrobust2 <- feols(formula(model_spec_all[3]), data = df_fuel, se = "hetero")
  summary(mrobust2)

 a <-  stats::influence.measures(mrobust)


### check  multicollinearity-------------
library(mctest)
library(qgraph)

#VIF
vif_test <- mctest::imcdiag(model_all, method ='VIF')
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




# compare models performance ----------------------
library(performance)

tem_model <- lapply(X=all_models_output, FUN= lm, data=df_fuel)
performance::check_model(model_all)
performance::compare_performance(all_models_output, rank = T)  %>% plot()




sjPlot::plot_model(all_models_output[[1]], show.data = T)

library(forestplot)
all_models_output

forestplot(x = all_models_output[[1]],
            sortvar = TE,
            predict = TRUE,
            print.tau2 = FALSE)


# model to data.frame
model <- all_models_output[[1]]
df <- coef(summary(model)) %>% as.data.frame
head(df)

https://stackoverflow.com/questions/62246541/forest-plot-with-table-ggplot-coding



df$colour <- rep(c("white", "gray95"), nrow(df)/2)
df$labels <- names(df)

p <- ggplot(df, aes(x = rr, y = labels, xmin = rrlow, xmax = rrhigh)) +
  geom_hline(aes(yintercept = labels, colour = colour), size = 7) +
  geom_pointrange(shape = 22, fill = "black") +
  geom_vline(xintercept = 1, linetype = 3) +
  xlab("Variable") +
  ylab("Adjusted Relative Risk with 95% Confidence Interval") +
  theme_classic() +
  scale_colour_identity() +
  scale_y_discrete(limits = rev(df$labels)) +
  scale_x_log10(limits = c(0.25, 4),
                breaks = c(0.25, 0.5, 1, 2, 4),
                labels = c("0.25", "0.5", "1", "2", "4"), expand = c(0,0)) +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank())

names(fplottable) <- c("labels", "eventnum", "arr")
fplottable$labels <- factor(fplottable$labels, rev(levels(df$labels)))
fplottable$colour <- rep(c("white", "gray95"), 18)

data_table <- ggplot(data = fplottable, aes(y = labels)) +
  geom_hline(aes(yintercept = labels, colour = colour), size = 7) +
  geom_text(aes(x = 0, label = labels), hjust = 0) +
  geom_text(aes(x = 5, label = eventnum)) +
  geom_text(aes(x = 7, label = arr), hjust = 1) +
  scale_colour_identity() +
  theme_void() +
  theme(plot.margin = margin(5, 0, 35, 0))

grid.arrange(data_table,p, ncol = 2)



# teste elevation --------
summary(df_raw$x_prop_slope_above_10)x_ci

cor(df_raw$x_mean_slope, df_raw$x_wghtd_mean_household_income_per_capita)
cor(df_raw$x_sd_slope, df_raw$x_wghtd_mean_household_income_per_capita)
cor(df_raw$x_prop_slope_above_10, df_raw$x_wghtd_mean_household_income_per_capita)

# spec
mean_slope <- y_energy_per_capita ~x_mean_slope+f_compact_contig_inter_dens+c+x_closeness_centrality_avg+x_density_pop_02km_2015+x_land_use_mix+x_mean_age_auto+ x_pop_2010+ x_pop_growth_1975_2015+ x_prop_dom_urban+ x_prop_high_educ+ x_prop_razao_dep+ x_state+ x_wghtd_mean_household_income_per_capita
sd_slope <- y_energy_per_capita ~x_sd_slope+f_compact_contig_inter_dens+x_circuity_avg+x_closeness_centrality_avg+x_density_pop_02km_2015+x_land_use_mix+x_mean_age_auto+ x_pop_2010+ x_pop_growth_1975_2015+ x_prop_dom_urban+ x_prop_high_educ+ x_prop_razao_dep+ x_state+ x_wghtd_mean_household_income_per_capita
prop_slope_above_10 <- y_energy_per_capita ~x_prop_slope_above_10+f_compact_contig_inter_dens+x_circuity_avg+x_closeness_centrality_avg+x_density_pop_02km_2015+x_land_use_mix+x_mean_age_auto+ x_pop_2010+ x_pop_growth_1975_2015+ x_prop_dom_urban+ x_prop_high_educ+ x_prop_razao_dep+ x_state+ x_wghtd_mean_household_income_per_capita

# model
mean_slope <- lm( formula(mean_slope)      , data=df_fuel )
sd_slope <- lm( formula(sd_slope)      , data=df_fuel )
prop_slope_above_10 <- lm( formula(prop_slope_above_10)      , data=df_fuel )

# comparison
performance::compare_performance(mean_slope, sd_slope, prop_slope_above_10)

stargazer(mean_slope, sd_slope, prop_slope_above_10, type = 'text')









# spec
m_f <- y_energy_per_capita ~x_mean_slope+f_compact_contig_inter_dens+x_circuity_avg+x_closeness_centrality_avg+x_density_pop_02km_2015+x_land_use_mix+x_mean_age_auto+ x_pop_2010+ x_pop_growth_1975_2015+ x_prop_dom_urban+ x_prop_high_educ+ x_prop_razao_dep+ x_state+ x_wghtd_mean_household_income_per_capita
m_v <- y_energy_per_capita ~x_mean_slope+x_intersection_density_km+x_contiguity+x_compacity+x_circuity_avg+x_closeness_centrality_avg+x_density_pop_02km_2015+x_land_use_mix+x_mean_age_auto+ x_pop_2010+ x_pop_growth_1975_2015+ x_prop_dom_urban+ x_prop_high_educ+ x_prop_razao_dep+ x_state+ x_wghtd_mean_household_income_per_capita

# model
m_f <- lm( formula(m_f)      , data=df_fuel )
m_v <- lm( formula(m_v)      , data=df_fuel )


# comparison
performance::compare_performance(m_f, m_v)

stargazer(m_f, m_v, type = 'text')



###############   compacitu positive 66666 ----------------

cor(df_fuel$y_energy_per_capita, df_fuel$x_compacity)
aaa <- y_energy_per_capita ~f_compact_contig_inter_dens+x_circuity_avg+x_closeness_centrality_avg+x_density_pop_02km_2015+x_land_use_mix+x_mean_age_auto+ x_mean_slope+ x_pop_2010+ x_pop_growth_1975_2015+ x_prop_dom_urban+ x_prop_high_educ+ x_prop_razao_dep+ x_state+ x_wghtd_mean_household_income_per_capita

#' x_mean_slope
#' x_pop_2010
aaa <- y_energy_per_capita ~f_compact_contig_inter_dens+x_circuity_avg+x_closeness_centrality_avg+x_density_pop_02km_2015+x_land_use_mix + x_mean_age_auto  #+ x_pop_2010 + x_pop_growth_1975_2015+ x_prop_dom_urban+ x_prop_high_educ+ x_prop_razao_dep+ x_state+ x_wghtd_mean_household_income_per_capita

lm( formula(aaa), data=df_fuel ) %>% summary

