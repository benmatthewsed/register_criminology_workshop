library(tidyverse)


oldest_cohort <- 1960

last_obs <- 2015

oldest_age <- last_obs - oldest_cohort



lexis <- 
  tibble(
    age = seq(0, oldest_age),
    year = list(seq(oldest_cohort, last_obs))
  ) %>% 
  unnest() %>% 
  mutate(birth_year = year - age,
         country = "Sweden")

lexis <- 
  tibble(
    age = seq(0, oldest_age),
    year = list(seq(oldest_cohort, last_obs))
  ) %>% 
  unnest() %>% 
  mutate(birth_year = year - age,
         country = "Sweden")

lexii <- 
  tibble(
    age = seq(0, oldest_age),
    year = list(seq(oldest_cohort, last_obs))
  ) %>% 
  unnest() %>% 
  mutate(birth_year = year - age,
         country = "Scotland")

lexii <- bind_rows(lexii, lexis)

lexii <- 
lexii %>% 
  mutate(
in_bounds = case_when(
  birth_year >= 1960 & birth_year <= 1984 & age <= 30 & age > 15 & country == "Sweden" ~ 1,
  birth_year >= 1960 & birth_year <= 1984 & age > 30 & year < 2016 & country == "Sweden" ~ 0,
  year >= 1989 & age <= oldest_age & age > 15 & year < 2016 & country == "Scotland" ~ 1,
  TRUE ~ NA_real_
))

lexii %>% 
  filter(country == "Scotland", in_bounds == 1)

year_plot <- 
lexii %>% 
  ggplot(aes(x = year, y = age, group = birth_year)) +
  geom_line(alpha = 0) +
  geom_line(
    data = 
      filter(lexii, in_bounds == 1),
    colour = "#c66d00") +
  coord_equal() +
  labs(y = "Age", x = "Year of conviction") +
  geom_vline(xintercept = c(2015)) +
  geom_hline(yintercept = 16,
             linetype = "dashed",
             colour = "grey50") +
  facet_wrap(~ country, ncol = 2)

ggsave(
  here::here("figures", "comparative_lexis_year.png"),
  year_plot,
  type = "cairo-png",
  height = 4,
  width = 8
)

cohort_annotation <- 
  tribble(
    ~birth_year, ~age, ~country, ~text,
    2000, 40, "Scotland", "Inconsistent\nfollow-up",
    2000, 40, "Sweden", "Consistent\nfollow-up"
  )

cohort_plot <- 
lexii %>% 
  ggplot(aes(x = birth_year, y = age, group = birth_year)) +
  geom_line(alpha = 0) +
  geom_line(
    data = 
      filter(lexii, in_bounds == 1),
    colour = "#c66d00") +
  coord_equal() +
  labs(y = "Age", x = "Year of birth") +
  geom_hline(yintercept = 16,
             linetype = "dashed",
             colour = "grey50") +
  geom_abline(slope = -1, intercept = 2015) +
  facet_wrap(~ country, ncol = 2) +
  geom_text(data = cohort_annotation,
            aes(label = text))

ggsave(
  here::here("figures", "comparative_lexis_cohort.png"),
  cohort_plot,
  type = "cairo-png",
  height = 4,
  width = 8
)

lexis <- 
  lexis %>%
  mutate(
    observed = case_when(
      age %% 2 == 0 & 
        year >= 2008 & 
        year <= 2015 &
        age < 17 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  mutate(
    birth_year = year - age
  ) %>%
  group_by(birth_year) %>%
  mutate(observed_count = cumsum(observed)) %>%
  ungroup()


lines <- seq(-1950, -2020 , -2)

lines <- 
  as.tibble(lines) %>% 
  rename(intercept = value) %>% 
  mutate(slope = 1)



# highlight cohort and cross-section --------------------------------------

lexii %>% 
  ggplot(aes(x = year, y = age, group = birth_year)) +
  geom_line(alpha = 0) +
  geom_line(
    data = 
      filter(lexii, in_bounds == 1),
    colour = "#c66d00") +
  geom_line(
    data = 
      filter(lexii, birth_year == 1973),
    colour = "blue") +
  geom_line(
    data = 
      filter(lexii, year == 2015),
    colour = "green") +
  coord_equal() +
  labs(y = "Age", x = "Year of conviction") +
  geom_vline(xintercept = c(2015)) +
  geom_hline(yintercept = 16,
             linetype = "dashed",
             colour = "grey50") +
  facet_wrap(~ country, ncol = 2)
