# load packages and data
source("R/setup.R")

library(patchwork)

metrics_df <- read_rds("../../data/urbanformbr/pca_regression_df/pca_regression_df_ready_to_use.rds")
mix_df <- read_csv("../../data/urbanformbr/pca_regression_df/landuse_mix_metrics.csv")

metrics_df <- metrics_df %>% left_join(mix_df, by = c("i_code_urban_concentration"="code_muni"))
metrics_df <- metrics_df %>%
  mutate(dissimilarity = 1 - dissimilarity)

(metrics_df %>%
  ggplot(aes(x=dissimilarity, y=y_fuel_consumption_per_capita_2010, size = x_pop_2015)) +
  geom_point() + geom_smooth(method = "lm") + theme(legend.position = "none") +
    scale_y_log10()) +
(metrics_df %>%
  ggplot(aes(x=dissimilarity, y=y_wghtd_mean_commute_time, size = x_pop_2015)) +
  geom_point() + geom_smooth(method = "lm") + theme(legend.position = "none")) +
(metrics_df %>%
   ggplot(aes(x=x_pop_2015, y=dissimilarity, size = x_pop_2015, color=y_wghtd_mean_commute_time)) +
   geom_point() + geom_smooth(method = "lm") + theme(legend.position = "none") +
   scale_x_log10()) +
(metrics_df %>%
   ggplot(aes(x=x_pop_2015, y=y_fuel_consumption_per_capita_2010*x_pop_2015, size = x_pop_2015)) +
   geom_point() + geom_smooth(method = "lm") + theme(legend.position = "none") +
   scale_x_log10() + scale_y_log10()) +
(metrics_df %>%
   ggplot(aes(x=x_pop_2015, y=y_wghtd_mean_commute_time, size = x_pop_2015, color=dissimilarity)) +
   geom_point() + geom_smooth(method = "lm") + theme(legend.position = "none") +
   scale_x_log10() + scale_color_distiller(palette = "Spectral"))



metrics_df %>%
    ggplot(aes(y=x_pop_2015, x=x_dissimilarity, size = x_pop_2015)) +
    geom_point() + geom_smooth(method = "lm") + theme(legend.position = "none") +
    scale_y_log10()

metrics_df %>%
  select(x_dissimilarity, x_pop_2015) %>%
  mutate(x_pop_2015 = x_pop_2015) %>%
  cor()

diss_lm <- lm(x_dissimilarity ~ x_pop_2015, data = metrics_df)
diss_resid <- resid(diss_lm)
plot(metrics_df$dissimilarity, diss_resid)

diss_lm <- lm(x_dissimilarity ~ y_fuel_consumption_per_capita_2010, data = metrics_df)
diss_resid <- resid(diss_lm)
plot(metrics_df$dissimilarity, diss_resid)


diss_lm <- lm(x_dissimilarity ~ y_wghtd_mean_commute_time * x_pop_2015, data = metrics_df)
diss_resid <- resid(diss_lm)
plot(metrics_df$dissimilarity, diss_resid)


(metrics_df %>%
  ggplot(aes(x=theil_h, y=y_fuel_consumption_per_capita_2010)) +
  geom_point() + geom_smooth(method = "lm")) +
(metrics_df %>%
  ggplot(aes(x=theil_h, y=y_wghtd_mean_commute_time)) +
  geom_point() + geom_smooth(method = "lm"))

(metrics_df %>%
  ggplot(aes(x=entropy, y=y_fuel_consumption_per_capita_2010)) +
  geom_point() + geom_smooth(method = "lm") ) +
(metrics_df %>%
  ggplot(aes(x=entropy, y=y_wghtd_mean_commute_time)) +
  geom_point() + geom_smooth(method = "lm"))



diversity_df %>%
  ggplot(aes(dissimilarity, theil_h)) +
  geom_point()
