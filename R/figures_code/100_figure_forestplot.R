library(ggplot2)
library(gridExtra)
library(data.table)
library(broom)

options(scipen = 99)


# get the fuel regression model ----------------------------------------------------

model <- readr::read_rds('./output/model_energy_dag_lm_sem-fronteira_cross-validation.rds')



# get regression model output as data.frame  ----------------------------------------------------

modeldf <- broom::tidy(model, conf.int = TRUE, conf.level = 0.95)

setDT(modeldf)
modeldf[, term := factor(x = term, levels=term)]
modeldf[, interval := paste0('(', round(conf.low,2), ', ', round(conf.high,2), ')') ]


modeldf[, significance := fcase(p.value>0.1,"",
                                p.value<=0.1,"*",
                                p.value<=0.05,"**",
                                p.value<=0.01,"***") ]


modeldf[, coef_stars := paste(round(estimate,3),significance)]


#### subseting for urban form variables

urbanformdf <- subset(modeldf, term %in% c("x_land_use_mix","x_density_pop_02km_2015",
                                "x_closeness_centrality_avg","f_compact_contig_inter_dens",
                                "x_circuity_avg"))

# sort values
urbanformdf <- urbanformdf[order(estimate),]
urbanformdf$color <- c("white","gray95","white","gray95","white")



### plot  ----------------------------------------------------

p <- ggplot(data=urbanformdf, aes(x = estimate , y = reorder(term, estimate), xmin = conf.low, xmax = conf.high)) +
        geom_hline(aes(yintercept = reorder(term, estimate), color = color), size = 7) +
        geom_pointrange(shape = 21, fill = "black") +
        geom_vline(xintercept = 0, linetype = 4) +
        xlab("Elasticity") +
        ylab("Variables") +
        theme_classic() +
        scale_colour_identity() +
        # scale_y_discrete(limits = rev(urbanformdf$term)) +
        theme(axis.text.y = element_blank(),
              axis.title.y = element_blank())

data_table <- ggplot(data = urbanformdf, aes(y = reorder(term, estimate)) ) +
                geom_hline(aes(yintercept = term, color = color), size = 7) +
                geom_text(aes(x = 0, label = reorder(term, estimate)), hjust = 0) +
                geom_text(aes(x = 5, label = reorder(coef_stars, estimate) )) +
                geom_text(aes(x = 7, label = reorder(interval, estimate) ), hjust = '4') +
                scale_colour_identity() +
                theme_void() +
                theme(plot.margin = margin(5, 0, 35, 5)) +
                expand_limits(x=c(0, 8))

temp_figure <- gridExtra::grid.arrange(data_table, p, layout_matrix = rbind(c(1,4)),ncol = 2)



ggsave(temp_figure, file='./figures/forestplot.png', dpi = 300,
       width = 25, height = 12, units = 'cm')

