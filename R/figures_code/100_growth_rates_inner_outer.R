#' Script para preparar plots para o relatório

source('R/fun_support/setup.R')
source("R/fun_support/colours.R")

mapviewOptions(platform = "leaflet")

# prepare data ------------------------------------------------------------


# taxas de crescimento por cidade, período e tipo de crescimento
metrics_df <- read_csv("data/urbanformbr_metrics_full.csv") %>%
  select(starts_with("i_"), "x_pop_2010")

urban_growth_df <-
  #read_rds("../../data/urbanformbr/urban_growth/urban_growth.rds") %>%
  read_rds("data/urban_growth.rds") %>%
  ungroup() %>%
  filter(period_start == 1990, period_end == 2014) %>%
  select(code_urban_concentration:period_end, growth_type, pop_geo_growth, built_geo_growth) %>%
  pivot_wider(names_from = growth_type,
              values_from = c(pop_geo_growth, built_geo_growth),
              values_fill = 0) %>%
  mutate(pop_growth_inner = pop_geo_growth_upward + pop_geo_growth_infill,
         pop_growth_outer = pop_geo_growth_extension + pop_geo_growth_leapfrog,
         pop_growth_total = pop_growth_inner + pop_growth_outer,
         built_growth_inner = built_geo_growth_upward + built_geo_growth_infill,
         built_growth_outer = built_geo_growth_extension + built_geo_growth_leapfrog,
         built_growth_total = built_growth_inner + built_growth_outer) %>%
  select(-contains("geo"))

metrics_df <- left_join(metrics_df, urban_growth_df,
                        by = c("i_code_urban_concentration" = "code_urban_concentration",
                               "i_name_uca_case" = "name_uca_case")) %>%
  mutate(size = if_else(x_pop_2010 >= 1000000, "Grande", "Média"))


# Plot figure -------------------------------------------------------------


# Population Growth -------------------------------------------------------

p1 <- metrics_df %>%
  arrange(x_pop_2010) %>%
  ggplot(aes(x = pop_growth_inner, y = pop_growth_outer)) +
  geom_hline(yintercept = 0, size = 0.1) +
  geom_vline(xintercept = 0, size = 0.1) +
  geom_point(aes(color = size), alpha = 1, size = 1) +
  geom_abline() +
  expand_limits(x=c(-0.02, 0.12), y=c(-0.02, 0.12)) +
  scale_x_percent() +
  scale_y_percent() +
  scale_colour_aop(palette = "blue", reverse = TRUE) +
  coord_equal() +
  labs(x = "Adensamento\nPreenchimento", y = "Extensão\nLeapfrog",
       color = "Tamanho da cidade",
       subtitle = "a)\tCresc. populacional") +
  theme_light() +
  theme(legend.position = "bottom")
p1

# Built Area Growth -------------------------------------------------------

p2 <- metrics_df %>%
  arrange(x_pop_2010) %>%
  ggplot(aes(x = built_growth_inner, y = built_growth_outer)) +
  geom_hline(yintercept = 0, size = 0.1) +
  geom_vline(xintercept = 0, size = 0.1) +
  geom_point(aes(color = size), alpha = 1, size = 1) +
  geom_abline() +
  expand_limits(x=c(-0.02, 0.12), y=c(-0.02, 0.12)) +
  scale_x_percent() +
  scale_y_percent() +
  scale_colour_aop(palette = "blue", reverse = TRUE) +
  coord_equal() +
  labs(x = "Adensamento\nPreenchimento", y = "Extensão\nLeapfrog",
       color = "Tamanho da cidade",
       subtitle = "b)\tCresc. da área construída") +
  theme_light() +
  theme(legend.position = "bottom")
p2

# Population vs Built Area ------------------------------------------------

p3 <- metrics_df %>%
  arrange(x_pop_2010) %>%
  ggplot(aes(x = pop_growth_total, y = built_growth_total)) +
  geom_hline(yintercept = 0, size = 0.1) +
  geom_vline(xintercept = 0, size = 0.1) +
  geom_point(aes(color = size), alpha = 1, size = 1) +
  geom_abline() +
  expand_limits(x=c(-0.02, 0.12), y=c(-0.02, 0.12)) +
  scale_x_percent() +
  scale_y_percent() +
  scale_size_continuous(range = c(0.5, 5)) +
  scale_colour_aop(palette = "blue", reverse = TRUE) +
  coord_equal() +
  labs(x = "População", y = "Área Construída",
       color = "Tamanho da cidade",
       subtitle = "c)\tCresc. populacional vs\n\tárea construída") +
  theme_light() +
  theme(legend.position = "bottom")
p3

p1 + p2 + p3 +
  plot_layout(guides = "collect") &
  theme(legend.position='bottom')

ggsave(filename = "figures/scatterplot_urban_growth.png", width = 16, height = 7,
       units = "cm", scale = 1.5, dpi = 300)


# Crescimento Populacional vs Crescimento Físico --------------------------

rate_df <- metrics_df %>%
  mutate(rate_pop_built = pop_growth_total / built_growth_total) %>%
  # mutate(rate_class = case_when(rate_pop_built > 2 ~ "pop",
  #                               rate_pop_built >= 1 ~ "slight pop",
  #                               rate_pop_built > 0.5 ~ "slight built",
  #                               TRUE ~ "built"))
  mutate(rate_class = case_when(rate_pop_built > 1.25 ~ "pop",
                                rate_pop_built >= 1 ~ "slight pop",
                                rate_pop_built > 0.8 ~ "slight built",
                                TRUE ~ "built"))

city_codes <- c(#2913606, # Ilhéus
  4209300, # Lages
  4314902, # Porto Alegre
  1400100, # Boa Vista
  4321600, # Tramandaí - Osório
  2800308, # Aracaju
  3550308, # São Paulo
  5300108  # Brasília
)

cities_df <- rate_df %>%
  filter(i_code_urban_concentration %in% city_codes)

p1 <- metrics_df %>%
  arrange(x_pop_2010) %>%
  ggplot(aes(x = pop_growth_total, y = built_growth_total)) +
  geom_hline(yintercept = 0, size = 0.1) +
  geom_vline(xintercept = 0, size = 0.1) +
  geom_point(aes(color = size), alpha = 1, size = 1) +
  # geom_smooth(method = "lm") +
  ggrepel::geom_text_repel(data = cities_df, aes(label = i_name_urban_concentration), size = 3) +
  geom_abline() +
  expand_limits(x=c(-0.02, 0.12), y=c(-0.02, 0.12)) +
  scale_x_percent() +
  scale_y_percent() +
  scale_size_continuous(range = c(0.5, 5)) +
  scale_colour_aop(palette = "blue", reverse = TRUE) +
  coord_equal() +
  labs(x = "Crescimento Populacional", y = "Crescimento da\nÁrea Construída",
       color = "Tamanho da cidade",
       subtitle = "a) Taxas de crescimento\nurbano") +
  theme_light() +
  theme(legend.position = "bottom")





p2 <- rate_df %>%
  ggplot(aes(x=x_pop_2010, y=rate_pop_built)) +
  # geom_hline(yintercept = 1, size = 5, alpha = 0.1) +
  annotate(geom = "rect", alpha = 0.15,
           xmin = min(metrics_df$x_pop_2010),
           xmax = max(metrics_df$x_pop_2010),
           ymin = 1/1.25, ymax = 1.25) +
  annotate(geom = "text", label = "+ 25%", vjust = -0.1, hjust = 0.5,
           x = max(metrics_df$x_pop_2010), y = 1.25) +
  annotate(geom = "text", label = "- 25%", vjust = 1.1, hjust = 0.5,
           x = max(metrics_df$x_pop_2010), y = 1/1.25) +
  geom_point(aes(color = size), alpha = 1, size = 1) +
  ggrepel::geom_text_repel(data = cities_df, aes(label = i_name_urban_concentration), size = 3) +
    geom_hline(yintercept = 1.25, size = 0.25) +
    geom_hline(yintercept = 1) +
    geom_hline(yintercept = 1/1.25, size = 0.25) +
    scale_x_log10(breaks = c(100000, 250000, 500000, 1000000, 2500000, 5000000, 10000000, 20000000),
                labels = c("0.1", "0.25", "0.5", "1.0", "2.5", "5.0", "10", "20"),
                limits = c(100000, 20000000)) +
  scale_y_log10(breaks = c(0.5, 1, 2, 4, 8),
                labels = c("1/2", "1", "2", "4", "8")) +
  scale_colour_aop(palette = "blue", reverse = TRUE) +
  labs(x = "População (em milhões)", y = "Razão",
       color = "Tamanho da cidade",
       subtitle = "b) Razão entre taxa de crescimento populacional\ne da área construída") +
  theme_light() +
  theme(legend.position = "bottom")


p3 <- p1 + p2 +
  plot_layout(guides = "collect") &
  theme(legend.position='bottom')

p3
ggsave(p3, filename = "figures/scatterplot_urban_growth.png", width = 16, height = 7,
       units = "cm", scale = 1.5, dpi = 300)




rate_df %>%
  count(rate_class)
rate_df %>%
  count(size, rate_class) %>%
  group_by(size) %>%
  mutate(p = n / sum(n))

