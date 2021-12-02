library(data.table)
library(ggplot2)
library(PupillometryR)


df <- fread('C:/Users/B00337220050/Downloads/pop_growth.csv', encoding = 'UTF-8')
head(df)

count(df, year_start, year_end, porte) %>%
  pivot_wider(names_from = porte, values_from = n) %>%
  janitor::clean_names() %>%
  mutate(count = grande + medio)

# select entire period
df2 <- subset(df, period == '1975 - 2014')

# remove outliers
df3 <- subset(df2,
              share_growth > quantile(df2$share_growth,0.02) &
                share_growth < quantile(df2$share_growth,0.95))

summary(df2$share_growth)
summary(df3$share_growth)

# figure
ggplot(data = df3, aes(x=status , y=share_growth ,fill= status,color=status)) +
  geom_flat_violin(alpha=.5, position = position_nudge(x = .05, y = 0)) +
   geom_jitter( alpha=.2,  position = position_nudge(x = -.1, y = 0)) +
  geom_boxplot( fill=NA,
                alpha = 0.3,
                width = .1,
                colour = "black",
                outlier.shape = NA,
                position = position_dodge(width = 0.9)) +
  facet_grid(.~porte) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(file='forma.png', width = 15, height = 10, units = 'cm')
