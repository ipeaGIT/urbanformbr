library(ggplot2)
library(gridExtra)
library(data.table)

# get the fuel regression model ----------------------------------------------------

pca_regression_df_ready_to_use <- readr::read_rds(
  "../../git_luiz/urbanformbr/"model_all.rds")

# get regression model output as data.frame  ----------------------------------------------------

model_all$coefficients <- model_all$coefficients[c(1:13)]
modeldf <- broom::tidy(model_all, conf.int = TRUE, conf.level = 0.95)

setDT(modeldf)
modeldf[, term := factor(x = term, levels=term)]
modeldf[, interval := paste0('(', round(conf.low,1), ', ', round(conf.high,1), ')') ]

modeldf <- modeldf %>% mutate(significance=if_else(p.value<=0.1,"*"," "),
                              significance=if_else(p.value<=0.05,"**",significance),
                              significance=if_else(p.value<=0.01,"***",significance))

#### subseting for urban form variables

urbanformdf <- modeldf %>% filter(term %in% c("x_land_use_mix","x_density_pop_02km_2015",
                                "x_closeness_centrality_avg","f_compact_contig_inter_dens",
                                "x_circuity_avg"))

urbanformdf$color <- c("white","gray95","white","gray95","white")

urbanformdf_2 <- urbanformdf[order(-urbanformdf$estimate),]

### plot  ----------------------------------------------------

modeldf$color <- c("white","gray95","white","gray95","white","gray95","white","gray95",
                   "white","gray95","white","gray95","white")
modeldf_2 <- modeldf[order(-modeldf$estimate),]

modeldf <- modeldf[order(modeldf$estimate),]

p <- ggplot(modeldf, aes(x = estimate , y = reorder(term, estimate), xmin = conf.low, xmax = conf.high)) +
  geom_hline(aes(yintercept = reorder(term, estimate), color = color), size = 7) +
  geom_pointrange(shape = 21, fill = "blue") +
  geom_vline(xintercept = 0, linetype = 4) +
  xlab("Elasticity") +
  ylab("Variables") +
  theme_classic() +
  scale_colour_identity() +
  scale_y_discrete(limits = rev(modeldf_2$term)) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())

data_table <- ggplot(data = modeldf, aes(y = reorder(term, estimate)) ) +
  geom_hline(aes(yintercept = term, color = color), size = 7) +
  geom_text(aes(x = 0, label = reorder(term, estimate)), hjust = 0) +
  geom_text(aes(x = 5, label = reorder(round(estimate,2), estimate) )) +
  geom_text(aes(x = 7, label = reorder(interval, estimate) ), hjust = '4') +
  scale_colour_identity() +
  theme_void() + 
  theme(plot.margin = margin(5, 0, 35, 5))

temp_figure <- gridExtra::grid.arrange(data_table,p,layout_matrix = rbind(c(1,4)),ncol = 2)

ggsave(temp_figure, file='forestplot.png', dpi = 300,
       width = 16, height = 10, units = 'cm')
