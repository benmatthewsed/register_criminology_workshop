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


# plot lines --------------------------------------------------------------

line_plot <- 
combined_spline %>% 
  filter(year < 2016) %>% 
  mutate(year = as.integer(year)) %>% 
  ggplot(aes(x = age, y = approx_spline, colour = year, group = year)) +
  geom_line() +
  facet_grid(sex ~ country, scales = "free_y") +
  scale_colour_viridis_c(option = "plasma",
                         breaks = c(2005, 2010, 2015)) +
  labs(x = "Age",
       y = "Conviction rate per 10,000 (spline approximation)",
       colour = "Year",
       caption = "WARNING: Data are estimated from aggregated age-bands via spline interpolation and may not be reliable!
       Scottish data are all convictions from SOI.
       Norwegian data are 'All types of sanctions' downloaded from https://www.ssb.no/en/statbank/table/10623"
)
  
ggsave(
  here::here("figures", "norway_scotland_line_plot.png"),
  line_plot,
  type = "cairo-png",
  height = 6, width = 8
)


# plot lexis --------------------------------------------------------------


female_rate_plot <- 
combined_spline %>% 
  filter(sex == "Female") %>% 
  ggplot(aes(x = year, y = age, fill = approx_spline)) +
  geom_tile() +
  metR::geom_contour_tanaka(aes(z = approx_spline)) +
  facet_grid(sex ~ country) +
  scale_fill_viridis_c(option = "plasma") +
  coord_equal() +
  geom_vline(xintercept = seq(2005, 2015, by = 5),
             linetype = "dashed",
             colour = "grey90",
             alpha = 0.3) +
  geom_hline(yintercept = seq(15, 55, by = 5),
             linetype = "dashed",
             colour = "grey90",
             alpha = 0.3) +
  geom_abline(slope = 1,
              intercept = seq(-1950, - 2050, by = -5),
              linetype = "dashed",
              colour = "grey90",
              alpha = 0.3) +
  labs(x = "Year",
       y = "Age",
       fill = "Conviction rate\n(spline\ninterpolation)")

male_rate_plot <- 
combined_spline %>% 
  filter(sex == "Male") %>% 
  ggplot(aes(x = year, y = age, fill = approx_spline)) +
  geom_tile() +
  metR::geom_contour_tanaka(aes(z = approx_spline)) +
  facet_grid(sex ~ country) +
  scale_fill_viridis_c(option = "plasma") +
  coord_equal() +
  geom_vline(xintercept = seq(2005, 2015, by = 5),
             linetype = "dashed",
             colour = "grey90",
             alpha = 0.3) +
  geom_hline(yintercept = seq(15, 55, by = 5),
             linetype = "dashed",
             colour = "grey90",
             alpha = 0.3) +
  geom_abline(slope = 1,
              intercept = seq(-1950, - 2050, by = -5),
              linetype = "dashed",
              colour = "grey90",
              alpha = 0.3) +
  labs(x = "Year",
       y = "Age",
       fill = "Conviction rate\n(spline\ninterpolation)")

rates_plot <- 
cowplot::plot_grid(
  female_rate_plot, male_rate_plot
)

ggsave(
  here::here("figures", "scotland_norway_rates_plot.png"),
  rates_plot,
  type = "cairo-png",
  height = 6, width = 10
)

female_comp <- 
combined_spline %>% 
  filter(year < 2016, sex == "Female") %>% 
  spread(country, approx_spline) %>% 
  mutate(diff =  `Scotland` - `Norway`) %>% 
  ggplot(aes(x = year, y = age, fill = diff)) +
  geom_tile() +
  metR::geom_contour_tanaka(aes(z = diff)) +
  facet_grid( ~ sex) +
  scale_fill_gradient2() +
  geom_vline(xintercept = seq(2005, 2015, by = 5),
             linetype = "dashed",
             colour = "grey70",
             alpha = 0.3) +
  geom_hline(yintercept = seq(15, 55, by = 5),
             linetype = "dashed",
             colour = "grey70",
             alpha = 0.3) +
  geom_abline(slope = 1,
              intercept = seq(-1950, - 2050, by = -5),
              linetype = "dashed",
              colour = "grey70",
              alpha = 0.3) +
  coord_equal() +
  labs(x = "Year",
       y = "Age",
       fill = "Difference")

male_comp <- 
combined_spline %>% 
  filter(year < 2016, sex == "Male") %>% 
  spread(country, approx_spline) %>% 
  mutate(diff =  `Scotland` - `Norway`) %>% 
  ggplot(aes(x = year, y = age, fill = diff)) +
  geom_tile() +
  metR::geom_contour_tanaka(aes(z = diff)) +
  facet_grid( ~ sex) +
  scale_fill_gradient2() +
  geom_vline(xintercept = seq(2005, 2015, by = 5),
             linetype = "dashed",
             colour = "grey70",
             alpha = 0.3) +
  geom_hline(yintercept = seq(15, 55, by = 5),
             linetype = "dashed",
             colour = "grey70",
             alpha = 0.3) +
  geom_abline(slope = 1,
              intercept = seq(-1950, - 2050, by = -5),
              linetype = "dashed",
              colour = "grey70",
              alpha = 0.3) +
  coord_equal() +
  labs(x = "Year",
       y = "Age",
       fill = "Difference",
       caption = "Blue = higher in Scotland, red = higher in Norway")

comp_plot <- cowplot::plot_grid(female_comp, male_comp, align = "h")

ggsave(
  here::here("figures", "scotland_norway_comparison.png"),
  comp_plot,
  type = "cairo-png",
  width = 8,
  height = 6
)
