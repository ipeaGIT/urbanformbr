library(ggplot2)
library(gridExtra)
library(data.table)
library(broom)

options(scipen = 99)


# get the fuel regression model ----------------------------------------------------

model <- readr::read_rds('./output/regression_output/model_spec_all.rds')
model <- model[[1]]


# get regression model output as data.frame  ----------------------------------------------------

modeldf <- broom::tidy(model, conf.int = TRUE, conf.level = 0.90)

setDT(modeldf)
modeldf[, interval := paste0('(', round(conf.low,2), ', ', round(conf.high,2), ')') ]


modeldf[, significance := fcase(p.value>0.1,"",
                                p.value<=0.1,"*",
                                p.value<=0.05,"**",
                                p.value<=0.01,"***") ]


modeldf[, coef_stars := paste(round(estimate,3),significance)]


#### subseting for urban form variables
vars_urban_form <- c('f_compact_contig_inter_dens', 'x_circuity_avg', 'x_density_pop_02km_2014', 'x_land_use_mix', 'x_normalized_closeness_centrality_avg', 'I(x_pop_2010 * f_compact_contig_inter_dens)')
urbanformdf <- subset(modeldf, term %in% vars_urban_form)

# sort values
urbanformdf <- urbanformdf[order(estimate),]
urbanformdf$color <- c("white","gray95","white","gray95","white","gray95")


# factor labels
labels_urban_form <- c('f_Compact/Contig/Inter.Dens', 'Circuity Avg', 'Density (exp. 2Km)', 'Land use mix', 'Closeness central.', 'Pop_x_f_Compact/Contig/Inter.Dens')
urbanformdf[, term := factor(x = term, levels=vars_urban_form, labels = labels_urban_form)]
table(urbanformdf$term)




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
                geom_text(aes(x = 5, label = reorder(coef_stars, estimate))) +
                geom_text(aes(x = 7, label = reorder(interval, estimate) ), hjust = '4') +
                scale_colour_identity() +
                theme_void() +
                theme(plot.margin = margin(5, 0, 30, 0))


temp_figure <- gridExtra::grid.arrange(data_table, p, ncol = 2, widths = c(1.5,1))
# temp_figure <- gridExtra::grid.arrange(data_table, p, layout_matrix = rbind(c(1,4)),ncol = 2)
temp_figure

ggsave(temp_figure, file='./figures/forestplot.png', dpi = 300,
       width = 25, height = 12, units = 'cm')







#### regression table geral ----------------------------
# https://cran.r-project.org/web/packages/fixest/vignettes/exporting_tables.html

# export regression table to excel

model_table <- fixest::etable(model, digits = 3, tex=F)

xlsx::write.xlsx(model_table, file='./output/regression_output/regression_table_1.xlsx', sheetName="Sheet1",row.names = TRUE)


