library(tidyverse)

demography.ken <- read.csv(here::here("Econ/Data", "demography_ken.csv"))
names(demography.ken)


#Kenya demography ----
demography.pivot <- demography.ken %>% 
  mutate(age_group = case_when(
    Age == 1 ~ "Below_1",
    between(Age, 2, 5) ~ "One_to_five",
    between(Age, 6,14) ~ "Six_to_14",
    between(Age, 15,19) ~ "Fifteen_to_19",
    between(Age, 20, 49) ~ "Twenty_to_49",
    TRUE ~ "Fifty_and_above"
  )) %>% 
  pivot_longer(-c(Age, age_group), names_to = "Year", values_to = "numbers.per.age") %>% 
  mutate(Year = parse_number(Year))


# demography.pivot.gp <- demography.pivot %>% 
#   group_by(age_group, Year) %>% 
#   dplyr::summarise(numbers.per.age.gp = sum(numbers.per.age)) %>% 
#   ungroup()
  

demography.pivot.gp2 <- demography.pivot %>% 
  group_by(age_group, Year) %>% 
  mutate(numbers.per.age.gp = sum(numbers.per.age),
         age.wt = numbers.per.age/numbers.per.age.gp)

#Model number of deaths ----
filename <- "Naomi"
read.date <- as.Date("2022-06-15")
samples.combined.df <- read.csv(file = here::here(paste0("Econ/Model_outcomes_output", "/", filename, "/", read.date),
                                                  paste0(filename, "_samples_all_years_scenarios.csv")
))

deaths <- samples.combined.df %>% 
  select(Scenario, Year, Sample, contains("deaths")) %>% 
  select(-total.deaths.all.ages)

names(deaths) <- c("Scenario", "Year", "Sample", 
                   "Below_1","One_to_five",
                   "Six_to_14", "Fifteen_to_19", 
                   "Twenty_to_49", "Fifty_and_above")

deaths.pivot<- deaths %>% 
  pivot_longer(-c(Scenario, Year, Sample), names_to = "age_group", values_to = "deaths.per.age.gp")

# ddf1 <- deaths.pivot %>%  
#   left_join(demography.pivot.gp, by = c("age_group", "Year")) %>% 
#   mutate(flu_mort = deaths.per.age.gp/numbers.per.age.gp) %>% 
#   left_join(demography.pivot, by = c("Year", "age_group")) %>% 
#   mutate(wtd.flu_mort = flu_mort*numbers.per.age/numbers.per.age.gp)

#combine and calculate wtd flu mortality ----
ddf2 <- deaths.pivot %>% 
  left_join(demography.pivot.gp2, by = c("age_group", "Year")) %>% 
  mutate(wtd.deaths = deaths.per.age.gp * age.wt,
         wtd.flu_mort = deaths.per.age.gp*age.wt/numbers.per.age.gp)


df.2010.2020.with.LE %>% 
  mutate(Age2 = case_when(
    between(Age, 1, 85) ~ paste0("Age_", Age),
    Age >= 86 ~ "Age_86.and.over"
  )) %>% 
  group_by(Age2) %>% 
  mutate(Prob_death2 = round(mean(Prob_death), 9),
         diff = Prob_death-Prob_death2) %>% #View()
  filter(Age <= 86) %>% #View()
  select(Age, Age2, WHO_age_band, Prob_death2)

ddf2 %>% 
  filter(Year == 2010, Scenario == 1, Sample == 10036) -> print

