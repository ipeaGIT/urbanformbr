library(ggplot2)
library(gridExtra)
library(data.table)
library(ggridges)
library(patchwork)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

source("R/colours.R")
options(scipen = 99)


# read data ----------------------------------------------------

df_metrics <- fread("../../data/urbanformbr/consolidated_data/urbanformbr_metrics_full.csv")

df_metrics <- df_metrics[, .(i_code_urban_concentration, x_pop_2010,
               x_contiguity_1990, x_contiguity_2000, x_contiguity_2014,
               x_compacity_1990, x_compacity_2000, x_compacity_2014,
               x_density_pop_02km_1990, x_density_pop_02km_2000, x_density_pop_02km_2014)
           ]

df_metrics[, size := fifelse(x_pop_2010 <= 1000000, "médio", "grande")]

df_metrics_tidy <- df_metrics %>%
  pivot_longer(cols = x_contiguity_1990:x_density_pop_02km_2014, names_to = "metric", values_to = "value") %>%
  mutate(metric = str_remove(metric, "_pop_02km")) %>%
  separate(metric, into = c("x", "metric", "year")) %>%
  mutate(year = factor(year, levels = c(2014, 2000, 1990), ordered=T),
         size = factor(size, levels = c("médio", "grande"),
                       labels = c("cidades médias", "cidades grandes")))



df_comp <- subset(df_metrics_tidy, metric == "compacity")
df_frag <- subset(df_metrics_tidy, metric == "contiguity")
df_dens <- subset(df_metrics_tidy, metric == "density")

### plots  ----------------------------------------------------

fig_comp <-
  ggplot(df_comp, aes(x=value, y=year, fill = year)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles=2, show.legend = FALSE) +
  scale_fill_brewer(palette = 'Oranges', direction = -1) +
  geom_point(x=40, y=factor(2014), color='red') +
  scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1.2)) +
  labs(y='', x='Compacidade') +
  theme_ridges() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "none") +
  facet_wrap(~size)

fig_frag <-
  ggplot(df_frag, aes(x = value , y =year, fill=year)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles=2, show.legend = FALSE) +
  scale_fill_brewer(palette = 'Purples', direction = -1) +
  scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1.2)) +
  labs(y='', x='Contiguidade') +
  theme_ridges() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "none") +
  facet_wrap(~size)

fig_dens <-
  ggplot(df_dens, aes(x = value , y =year, fill=year)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles=2, show.legend = FALSE) +
  scale_fill_brewer(palette = 'Greens', direction = -1) +
  # scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1.2)) +
  labs(y='', x='Densidade Experienciada (raio de 2 km)') +
  theme_ridges() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "none") +
  facet_wrap(~size)

### save figures  ----------------------------------------------------

fig_final <- fig_comp / fig_frag / fig_dens + plot_annotation(tag_levels = 'A')
fig_final

ggsave(fig_final, file='./figures/figura_11_landscape_metrics_over_time.pdf', dpi = 300,
       width = 16, height = 24, units = 'cm')




# testes ------------------------------------------------------------------

urban_growth_df %>%
  filter(period_start==1990, period_end==2014) %>%
  left_join(df_metrics, by = "code_urban_concentration" ) %>%
  ggplot(aes(x=pop_geo_growth, y=contiguity_2014 )) +
    geom_point() +
  facet_wrap(~growth_type)
  View()

df_metrics %>%
  filter(year != 2000) %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  mutate(direction = contiguity)
  ggplot(aes(x=contiguity, y=compacity, color=year)) +
  geom_point() +
  geom_path(aes(group=code_urban_concentration))












