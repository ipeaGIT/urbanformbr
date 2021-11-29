library(ggplot2)
library(gridExtra)
library(data.table)

# regression ----------------------------------------------------
data('mtcars')

m <- lm(hp ~mpg + cyl + carb , data=mtcars)

# get regression model output as data.frame  ----------------------------------------------------
df <- broom::tidy(m, conf.int = TRUE, conf.level = 0.95)


setDT(df)
df[, term := factor(x = term, levels=term)]
df[, interval := paste0('(', round(conf.low,1), ', ', round(conf.high,1), ')') ]

### plot  ----------------------------------------------------

df$color <- rep(c("white", "gray95"), nrow(df)/2)



p <- ggplot(df, aes(x = estimate , y = reorder(term, estimate), xmin = conf.low, xmax = conf.high)) +
      geom_hline(aes(yintercept = reorder(term, estimate), color = color), size = 7) +
      geom_pointrange(shape = 22, fill = "black") +
      geom_vline(xintercept = 1, linetype = 3) +
      xlab("Elasticity") +
      ylab("Variables") +
      theme_classic() +
      scale_colour_identity() +
      # scale_y_discrete(limits = rev(df$term)) +
      theme(axis.text.y = element_blank(),
            axis.title.y = element_blank())




data_table <- ggplot(data = df, aes(y = reorder(term, estimate)) ) +
              geom_hline(aes(yintercept = term, color = color), size = 7) +
              geom_text(aes(x = 0, label = reorder(term, estimate)), hjust = 0) +
              geom_text(aes(x = 5, label = reorder(round(estimate,2), estimate) )) +
              geom_text(aes(x = 7, label = reorder(interval, estimate) ), hjust = 1) +
              scale_colour_identity() +
              theme_void() +
              theme(plot.margin = margin(5, 0, 35, 5)) # !!!!!!!!


temp_figure <- gridExtra::grid.arrange(data_table, p, ncol = 2)

ggsave(temp_figure, file='forestplot.png', dpi = 300,
       width = 16, height = 10, units = 'cm')
