library(ggplot2)
library(gridExtra)
library(data.table)
library(magrittr)
library(ggridges)
library(patchwork)
library(readr)


# source("R/fun_support/colours.R")
options(scipen = 99)


# read data ----------------------------------------------------

df_raw <- read_rds('../../data/urbanformbr/urban_growth/urban_growth.rds')
head(df_raw)

# calculate totals by UCA
# df2 <- setDT(df)[, .(code_muni, name_uca_case, period_start, pop_start, built_start )]
setDT(df_raw)
df <- df_raw[, .(
            # pop = sum(pop),
            # built = sum(built),
            pop_start = sum(pop_start),
            pop_end = sum(pop_end),
            built_start = sum(built_start),
            built_end = sum(built_end)
            ),
       by=.(code_muni, name_uca_case, period_start, period_end)]




# add periods
df[, period := paste0(period_start,'-', period_end)]
df[, period_window := period_end - period_start]

# geometric growth rate
df[, growth_pop := 100 * ((pop_end / pop_start )^(1/period_window) - 1)]
df[, growth_built := 100 * ((built_end / built_start )^(1/period_window) - 1)]
head(df)



# reorder periods
df[, period := factor(period, levels = c('1975-1990', '1990-2000' , '2000-2014', '1975-2014'), ordered=T)]
table(df$period)

# parga
subset(df, growth_built==0) %>% View()


### Scatter plot  ----------------------------------------------------
temp_df <- subset(df, period == '1975-2014')

maxv <- max(df$growth_pop, df$growth_built)

fig_scatter <- ggplot() +
                  geom_point(data = df ,aes(x=growth_built,y=growth_pop),
                             size=.5, alpha=.5) +
                  coord_fixed(xlim=c(0,maxv), ylim=c(0,maxv)) +
                  facet_grid(.~period )+
                  geom_abline() +
                  theme(aspect.ratio=1) +
                  theme_minimal()


  subset(df, growth_pop>16)
  subset(df, name_uca_case =='porto_seguro')

pop
1970:  9,505   (IBGE 33,108)
1990: 28,515   (IBGE 34,661)




### save figures  ----------------------------------------------------




ggsave(fig_scatter, file='./figures/growth_scatterplot.png', dpi = 300,
       width = 16, height = 6, units = 'cm')





### path plot  ----------------------------------------------------
temp_df <- subset(df, period != '1975-2014')
head(temp_df)


temp_df <- temp_df[, .( growth_pop  = weighted.mean(x=growth_pop, w=pop_start ),
                  growth_built  =  weighted.mean(x=growth_built, w=built_start )
                  ),
             by=.(period)]

### Path
temp_df <- melt.data.table(temp_df, id.vars = 'period')

temp_path <- ggplot(data=head(temp_df), aes(x=period, y=value, color=variable, group=variable)) +
              geom_point() +
              geom_path() +
              theme_ridges()


ggsave(temp_path, file='./figures/growth_path.png', dpi = 300,
       width = 20, height = 12, units = 'cm')


### Ridge

aaa <-  melt.data.table(df, id.vars = 'period', measure.vars = c('growth_pop','growth_built'  ))

temp_ridge <- ggplot(aaa, aes(x = value , y = variable     , fill=variable     )) +
              stat_density_ridges(quantile_lines = TRUE, quantiles=2, show.legend = FALSE) +
              scale_fill_brewer(palette = 'Purples', direction = -1) +
              labs(y='', x='Tax crescimento') +
              theme_ridges()


ggsave(temp_ridge, file='./figures/growth_ridge.png', dpi = 300,
       width = 12, height = 12, units = 'cm')

