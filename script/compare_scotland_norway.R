# compare scotland norway

library(tidyverse)

# read in data

norway_spline <- read_csv(here::here("data", "norway_spline_interpolated.csv"))

norway_spline <- 
norway_spline %>% 
  mutate(sex = if_else(sex == "Males", "Male", "Female"))

soi_prev <- read_csv(here::here("data", "scot_age_year_sex_prevalence_counts_all_convictions.csv"))

mype <- read_csv(here::here("data", "scot_mid_year_population_estimates_sex_1989_2016.csv"))

soi_prev <- 
soi_prev %>% 
  rename(year = disp_year, age = disp_age)

soi_prev <- left_join(soi_prev, mype)

soi_prev <- 
  soi_prev %>% 
  mutate(prev_rate = n / mype * 10000)


soi_prev <- 
soi_prev %>% 
  filter(year %in% unique(norway_spline$year),
         age %in% unique(norway_spline$age))

norway_spline <- 
norway_spline %>% 
  filter(year %in% unique(soi_prev$year))

soi_prev <- 
soi_prev %>% 
  select(year, sex, age, prev = prev_rate) %>% 
  mutate(method = "Scotland-real")

norway_spline <- 
  norway_spline %>% 
  select(year, sex, age, prev = approx_spline) %>% 
  mutate(method = "Norway-spline")

combined <- 
bind_rows(
  soi_prev, norway_spline
)

combined <- 
combined %>% 
  filter(year < 2016)

combined %>% 
  ggplot(aes(x = year, y = age, fill = prev)) +
  geom_tile() +
  metR::geom_contour_tanaka(aes(z = prev)) +
  facet_grid(method ~ sex) +
  scale_fill_viridis_c() +
  coord_equal()

combined <- 
combined %>% 
  group_by(sex, method) %>% 
  mutate(std = prev / max(prev)) %>% 
  ungroup()

combined %>% 
  ggplot(aes(x = year, y = age, fill = std)) +
  geom_tile() +
  metR::geom_contour_tanaka(aes(z = std)) +
  facet_grid(method ~ sex) +
  scale_fill_viridis_c() +
  coord_equal()


combined %>% 
  select(-prev) %>% 
  spread(method, std) %>% 
  mutate(diff =  `Scotland-real` - `Norway-spline`) %>% 
  ggplot(aes(x = year, y = age, fill = diff)) +
  geom_tile() +
  metR::geom_contour_tanaka(aes(z = diff)) +
  facet_grid( ~ sex) +
  scale_fill_gradient2() +
  coord_equal()

combined %>% 
  select(-std) %>% 
  spread(method, prev) %>% 
  mutate(diff =  `Scotland-real` - `Norway-spline`) %>% 
  ggplot(aes(x = year, y = age, fill = diff)) +
  geom_tile() +
  metR::geom_contour_tanaka(aes(z = diff)) +
  facet_grid( ~ sex) +
  scale_fill_gradient2() +
  coord_equal()


# now interpolated


scotland_spline <- read_csv(here::here("data", "scotland_spline_interpolated.csv"))

scotland_spline <- 
scotland_spline %>% 
  select(1, year = disp_year, 3, approx_spline = approx_count) %>% 
  mutate(country = "Scotland")

norway_spline <- read_csv(here::here("data", "norway_spline_interpolated.csv"))

norway_spline <- 
  norway_spline %>% 
  mutate(sex = if_else(sex == "Males", "Male", "Female"),
         country = "Norway")

combined_spline <- 
bind_rows(
  scotland_spline, norway_spline
)


combined_spline %>% 
  ggplot(aes(x = year, y = age, fill = approx_spline)) +
  geom_tile() +
  metR::geom_contour_tanaka(aes(z = approx_spline)) +
  facet_grid(country ~ sex) +
  scale_fill_viridis_c() +
  coord_equal()

combined_spline %>% 
  spread(country, approx_spline) %>% 
  mutate(diff =  `Scotland` - `Norway`) %>% 
  ggplot(aes(x = year, y = age, fill = diff)) +
  geom_tile() +
  metR::geom_contour_tanaka(aes(z = diff)) +
  facet_grid( ~ sex) +
  scale_fill_gradient2() +
  coord_equal()
