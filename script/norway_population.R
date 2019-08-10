library(vroom)

tmp1 <- read_delim(here::here("data", "Personer1.csv"), delim = ";", skip = 1)
tmp2 <- read_delim(here::here("data", "Personer1 (1).csv"), delim = ";", skip = 1)
tmp3 <- read_delim(here::here("data", "Personer1 (2).csv"), delim = ";", skip = 1)

norway_pop <- 
bind_rows(
  tmp1, tmp2, tmp3
) %>% 
  gather(year, n, `Persons 2002`:`Persons 2017`) %>% 
  group_by(sex, age, year) %>% 
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  mutate(age = as.numeric(str_sub(age, 1, 2)),
         year = as.numeric(str_sub(year, 9, str_length(year))))

write_csv(
  norway_pop,
  here::here("data", "norway_pop.csv")
)

norway_pop_grp <- 
norway_pop %>% 
  mutate(age_group = case_when(
    age <= 17 ~ "15-17",
    age >= 18 & age <= 20 ~ "18-20",
    age >= 21 & age <= 24 ~ "21-24",
    age >= 25 & age <= 29 ~ "25-29",
    age >= 30 & age <= 39 ~ "30-39",
    age >= 40 & age <= 49 ~ "40-49",
    age >= 50 & age <= 59 ~ "50-59"
  )) %>% 
  group_by(sex, age_group, year) %>% 
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  rename(age = age_group)


write_csv(
  norway_pop_grp,
  here::here("data", "norway_pop_grp.csv")
)
