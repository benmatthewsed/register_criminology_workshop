library(tidyverse)
library(readxl)

# downloaded from https://www.ssb.no/en/statbank/table/10623

norway <- read_xlsx(here::here("data", "norway_sanction_statistics.xlsx"),
          range = "A4:T276")


norway_pop_grp <- read_csv(here::here("data", "norway_pop_grp.csv"))

norway <- 
norway %>% 
  gather(year, count, `2002`:`2017`) %>% 
  rename(sanction = 1,
         age = 3,
         sex = 4) %>% 
  select(-2) %>% 
  fill(sanction, age) %>% 
  mutate(age = str_sub(age, 1L, 5L),
         age = if_else(str_detect(age, "60"), "60+", age))

norway %>% 
  count(sanction)

norway <- 
  left_join(norway, norway_pop_grp %>% 
              mutate(year = as.character(year)))

norway <- 
norway %>% 
  mutate(prev_rate = count / n * 10000)

tmp <- 
norway %>% 
  filter(!is.na(sex),
         str_detect(sanction, "All ty"),
         !str_detect(age, "60")) %>% 
  mutate(age_start = as.numeric(str_sub(age, 1, 2)),
         age_end = as.numeric(str_sub(age, 4, 5)),
         diff = age_end - age_start + 1,
         ave_rate = prev_rate / diff,
         long_rate = map2(ave_rate, diff, rep),
         long_age = map2(age_start, age_end, seq),
         mid = map_dbl(long_age, mean),
         year = as.numeric(year))



# making up some data -----------------------------------------------------

 
  interp <- function(data){
    spline(data$mid, data$ave_rate, n = 100000) %>% 
      as_tibble() %>% 
      filter(round(x, 3) %in% seq(16, 60)) %>% 
      mutate(x = round(x, 0)) %>% 
      group_by(x) %>% 
      summarise(y = mean(y)) %>% 
      rename(age = x,
             approx_spline = y)
    
  }

  lin_interp <- function(data){
    approx(data$mid, data$ave_rate, n = 100000) %>% 
      as_tibble() %>% 
      filter(round(x, 3) %in% seq(16, 60)) %>% 
      mutate(x = round(x, 0)) %>% 
      group_by(x) %>% 
      summarise(y = mean(y)) %>% 
      rename(age = x,
             approx_lin = y)
    
  }


# plotting interpolated data ----------------------------------------------


  
tmp %>% 
  group_by(year, sex) %>% 
  nest() %>% 
  mutate(spline = map(data, interp),
         lin = map(data, lin_interp)) %>% 
  unnest(spline) %>% 
  ggplot(aes(x = year, y = age, fill = approx_spline)) +
  geom_tile() +
  metR::geom_contour_tanaka(aes(z = approx_spline)) +
  facet_grid( ~ sex) +
  scale_fill_viridis_c() +
  coord_equal()

tmp %>% 
  group_by(year, sex) %>% 
  nest() %>% 
  mutate(spline = map(data, interp),
         lin = map(data, lin_interp)) %>% 
  unnest(spline, lin) %>% 
  select(-age1) %>% 
  gather(interp, approx_count, approx_spline:approx_lin) %>% 
  ggplot(aes(x = year, y = age, fill = approx_count)) +
  geom_tile() +
  metR::geom_contour_tanaka(aes(z = approx_count)) +
  facet_grid(interp ~ sex) +
  scale_fill_viridis_c() +
  coord_equal()


tmp %>% 
  group_by(year, sex) %>% 
  nest() %>% 
  mutate(spline = map(data, interp),
         lin = map(data, lin_interp)) %>% 
  unnest(spline, lin) %>% 
  select(-age1) %>% 
  gather(interp, approx_count, approx_spline:approx_lin) %>% 
  ggplot(aes(x = age, y = approx_count, colour = as.factor(year))) +
  geom_line(aes(linetype = interp)) +
  facet_grid( ~ sex) +
  scale_colour_viridis_d()



# saving spline -----------------------------------------------------------

norway_spline <- 
tmp %>% 
  group_by(year, sex) %>% 
  nest() %>% 
  mutate(spline = map(data, interp),
         lin = map(data, lin_interp)) %>% 
  unnest(spline)

write_csv(
  norway_spline,
  here::here("data", "norway_spline_interpolated.csv")
)
