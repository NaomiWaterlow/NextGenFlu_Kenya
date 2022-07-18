demography.ken <- read.csv(here::here("Econ/Data", "demography_ken.csv"))

#Kenya demography ----
demography.ken <- demography.ken %>% 
  mutate(Age.edit = Age -1,
         X2019 = X2018) %>% 
  select(-Age)

demography.pivot.gp2 <- demography.ken %>% 
  #select(-Age) %>% 
  mutate(Model_age_band = case_when(
    Age.edit == 0 ~ "Below_1",
    between(Age.edit, 1, 5) ~ "One_to_five",
    between(Age.edit, 6,14) ~ "Six_to_14",
    between(Age.edit, 15,19) ~ "Fifteen_to_19",
    between(Age.edit, 20, 49) ~ "Twenty_to_49",
    TRUE ~ "Fifty_and_above"
  )) %>% 
  pivot_longer(-c(Age.edit, Model_age_band), names_to = "Year", values_to = "numbers.per.age") %>% 
  mutate(Year = parse_number(Year)) %>% 
  group_by(Model_age_band, Year) %>% 
  mutate(numbers.per.age.gp = sum(numbers.per.age),
         age.wt = numbers.per.age/numbers.per.age.gp) %>% 
  ungroup()
