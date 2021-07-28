# description -------------------------------------------------------------

# this script compares population from censo and ghsl in 2015 to see if ghsl pop
#..data can be used to estimate pop geom growth (1975-2015) at pca & regression
# analysis

# ghsl pop estimates were calculated at R/GHSL/03_2_pop_estimate_built_up_area

# setup -------------------------------------------------------------------

source('R/setup.R')


# read data ---------------------------------------------------------------

df_prep <- readr::read_rds("../../data/urbanformbr/pca_regression_df/pca_regression_df.rds") %>%
  dplyr::select(-c(code_muni_uca))


  # * censo -----------------------------------------------------------------
  df_pop_censo <- readr::read_rds("../../data/urbanformbr/pca_regression_df/1970-2015_pop.rds")

  # filter only 184 from our df
  df_pop_censo <- subset(df_pop_censo, code_urban_concentration %in% df_prep$code_urban_concentration)

  #a <- df_pop_censo %>%
  #  dplyr::filter(ano==2015) %>%
  #  select(-ano) %>%
  #  rename(pop_2015_censo = pop)

  df_pop_censo <- df_pop_censo %>%
    dplyr::filter(ano==2015) %>%
    tidyr::pivot_wider(
      names_from = c("ano"),
      values_from = c("pop"),
      names_prefix = "pop_censo_"
    )

  # * ghsl ------------------------------------------------------------------
  df_pop_ghsl <- readr::read_rds('../../data/urbanformbr/ghsl/results/uca_pop_100000_built_up_area_population_results.rds')

  df_pop_ghsl <- df_pop_ghsl %>%
    sf::st_drop_geometry() %>%
    dplyr::select(code_urban_concentration, pop2015) %>%
    rename(pop_ghsl_2015 = pop2015) %>%
    dplyr::filter(code_urban_concentration %in% df_prep$code_urban_concentration)

# merge data --------------------------------------------------------------

df_final <- dplyr::left_join(
  df_pop_censo,df_pop_ghsl,
  by = c("code_urban_concentration" = "code_urban_concentration")
)

#df_final <- df_final %>%
#  mutate(
#    pop_censo_2015_log10 = log10(pop_censo_2015),
#    pop_ghsl_2015_log10 = log10(pop_ghsl_2015)
#    )

df_final_long <- df_final %>% tidyr::pivot_longer(
  cols = pop_censo_2015:pop_ghsl_2015,
  names_to = "base",
  values_to = "pop_2015"
)

# plot & analyse data ---------------------------------------------------------------

cor(df_final$pop_censo_2015,df_final$pop_ghsl_2015)

df_final %>%
    ggplot(aes(pop_censo_2015,pop_ghsl_2015)) +
    geom_point() +
    scale_x_log10() +
    scale_y_log10() +
    stat_smooth(method = lm)


ggplot(df_final_long) +
  geom_boxplot(aes(x = base, y = pop_2015, fill = base)) +
  scale_y_log10()

t.test(pop_2015 ~ base, data = df_final_long)
t.test(log10(pop_2015) ~ base, data = df_final_long)

model <- lm(pop_censo_2015 ~ pop_ghsl_2015, data = df_final)
model_log10 <- lm(log10(pop_censo_2015) ~ log10(pop_ghsl_2015), data = df_final)

summary(model)
summary(model_log10)

# Results: results suggest that at 95% confidence level, there is no significant
# difference between censo's and ghsl's population data
# given the p-value is greater than 0.05, we can accept the null hypothesis that the
# two means (censo's and ghsl's pop) are equal

# Conclusion: results suggest that there is no statistical significant difference in
# using ghsl's population data to estimate population geometric growth


