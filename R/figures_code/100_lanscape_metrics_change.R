library(ggplot2)
library(gridExtra)
library(data.table)
library(ggridges)
library(patchwork)
library(readr)


source("R/colours.R")
options(scipen = 99)


# read data ----------------------------------------------------

df_comp <- fread('../../data/urbanformbr/fragmentation_compacity/compacity_metrics.csv')
df_frag <- fread('../../data/urbanformbr/fragmentation_compacity/fragmentation_metrics.csv')

# reorder years
df_comp[, year := factor(year, levels = c(2014, 2000 , 1990, 1975), ordered=T)]
df_frag[, year := factor(year, levels = c(2014, 2000 , 1990, 1975), ordered=T)]


### plots  ----------------------------------------------------

fig_comp <- ggplot(df_comp, aes(x = avg_cell_closeness , y = as.factor(year), fill=as.factor(year))) +
                stat_density_ridges(quantile_lines = TRUE, quantiles=5, show.legend = FALSE) +
                scale_fill_brewer(palette = 'Oranges', direction = -1) +
  geom_point(x=40, y=factor(2014), color='red') +
  labs(y='', x='Compacidade') +
                theme_ridges()

fig_frag <- ggplot(df_frag, aes(x = proportion_largest_patch , y = as.factor(year), fill=as.factor(year))) +
                stat_density_ridges(quantile_lines = TRUE, quantiles=2, show.legend = FALSE) +
                scale_fill_brewer(palette = 'Purples', direction = -1) +
                labs(y='', x='Fragmentação') +
                theme_ridges()




### save figures  ----------------------------------------------------

fig_final <- fig_comp / fig_frag + plot_annotation(tag_levels = 'A')


ggsave(fig_final, file='./figures/lanscape_metrics_over_time.png', dpi = 300,
       width = 14, height = 20, units = 'cm')

