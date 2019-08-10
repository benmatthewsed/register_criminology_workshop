
library(tidyverse)
library(here)


# read in data ------------------------------------------------------------


soi_prev <- read_csv(here("data", "scot_age_year_sex_prevalence_counts_all_convictions.csv"))

mype <- read_csv(here("data", "scot_mid_year_population_estimates_sex_1989_2016.csv"))



mype <- 
  mype %>% 
  rename(disp_age = age,
         disp_year = year) %>% 
  filter(disp_age <= 60,
         disp_year >= 2002)

soi_prev <- 
  soi_prev %>% 
  filter(disp_year <= 2015,
         disp_year >= 2002)

soi_prev <- left_join(soi_prev, mype)


soi_prev <- 
  soi_prev %>% 
  mutate(prev_rate = n / mype * 10000)

soi_bands <- 
soi_prev %>% 
  mutate(age_group = case_when(
    disp_age <= 17 ~ "15-17",
    disp_age >= 18 & disp_age <= 20 ~ "18-20",
    disp_age >= 21 & disp_age <= 24 ~ "21-24",
    disp_age >= 25 & disp_age <= 29 ~ "25-29",
    disp_age >= 30 & disp_age <= 39 ~ "30-39",
    disp_age >= 40 & disp_age <= 49 ~ "40-49",
    disp_age >= 50 & disp_age <= 59 ~ "50-59"
    )) %>% 
  filter(!is.na(age_group)) %>% 
  group_by(sex, disp_year, age_group) %>% 
  summarise(n = sum(n),
            mype = sum(mype),
            mid = mean(disp_age)) %>% 
  ungroup() %>% 
  mutate(prev_rate = n / mype * 10000)

soi_prev %>% 
  ggplot(aes(x = disp_age, y = prev_rate, colour = as.factor(disp_year))) +
  geom_line() +
  geom_point(
    data = soi_bands,
    aes(x = mid)
  ) +
  facet_wrap(~sex)



# interpolating -----------------------------------------------------------



interp <- function(data){
  spline(data$mid, data$prev_rate, n = 100000) %>% 
    as_tibble() %>% 
    filter(round(x, 0) %in% seq(16, 60)) %>% 
    mutate(x = round(x, 0)) %>% 
    group_by(x) %>% 
    summarise(y = mean(y)) %>% 
    rename(age = x,
           approx_spline = y)
  
}

lin_interp <- function(data){
  approx(data$mid, data$prev_rate, n = 100000) %>% 
    as_tibble() %>% 
    filter(round(x, 0) %in% seq(16, 60)) %>% 
    mutate(x = round(x, 0)) %>% 
    group_by(x) %>% 
    summarise(y = mean(y)) %>% 
    rename(age = x,
           approx_lin = y)
  
}         

interp <- 
soi_bands %>% 
  group_by(sex, disp_year) %>% 
  nest() %>% 
  mutate(spline = map(data, interp),
        lin = map(data, lin_interp)) %>% 
  select(-data) %>% 
  unnest() %>% 
  select(-age1)

soi_prev %>% 
  ggplot(aes(x = disp_age, y = prev_rate, colour = as.factor(disp_year))) +
  geom_line() +
  geom_point(
    data = soi_bands,
    aes(x = mid)
  ) +
  facet_wrap(~sex)


interp %>% 
  ggplot(aes(x = age, y = approx_spline, colour = as.factor(disp_year))) +
  geom_line() +
  facet_grid( ~ sex) +
  scale_fill_viridis_c()


interp %>% 
  count(age) %>% 
  View()

soi_subset <- 
soi_prev %>% 
  filter(disp_age <= 54)

interp <- 
interp %>% 
  gather(interp, approx_count, approx_spline:approx_lin)

soi_subset <- 
soi_subset %>% 
  select(-n, -mype, approx_count = prev_rate) %>% 
  mutate(interp = "true") %>% 
  select(sex, disp_year, age = disp_age, interp, approx_count)

bind_rows(
  soi_subset, interp
) %>% 
  ggplot(aes(x = age, y = approx_count)) +
  geom_line(aes(colour = interp)) +
  facet_grid(disp_year ~ sex, scales = "free_y")


bind_rows(
  soi_subset, interp
) %>% 
  ggplot(aes(x = disp_year, y = age, fill = approx_count)) +
  geom_tile() +
  facet_grid(interp ~ sex) +
  coord_equal()


bind_rows(
  soi_subset, interp
)

interp %>% 
  group_by(interp) %>% 
  mutate(diff = approx_count - soi_subset$approx_count) %>% 
  ggplot(aes(x = disp_year, y = age, fill = diff)) +
  geom_tile() +
  facet_grid(interp ~ sex) +
  coord_equal()

# saving interpolated

interp %>% 
  filter(str_detect(interp, "spline")) %>% 
  write_csv(
    .,
    here::here("data", "scotland_spline_interpolated.csv")
  )
