#Updated 5/7/2022
library(here)
library(tidyverse)

source(here::here("Econ/Scripts", "LE_calculator.R"))
Ken_LT  <- fread(here::here(
  "Econ/Data", "WPP2019_Life_Table_Medium.csv"))[
    Location == "Kenya" & MidPeriod %in% c(2013, 2018) & Sex == "Total"]  # only need combined sexes for 2025

#Undiscounted LE----
ken.2010.2015 <- flu_dLE(LT = Ken_LT, r = 0, selectTime = "2010-2015") %>% 
  mutate(Period = "2010-2015")
ken.2015.2020 <- flu_dLE(LT = Ken_LT, r = 0, selectTime = "2015-2020") %>% 
  mutate(Period = "2015-2020")

undisc <- rbind(ken.2010.2015, ken.2015.2020) %>%
  select(Age, dLEx, Period) 
names(undisc) <- c("Age", "LE_undisc", "Period")


#Discounted LE----
disc_ken.2010.2015 <- flu_dLE(LT = Ken_LT, r = 0.03, selectTime = "2010-2015") %>% 
  mutate(Period = "2010-2015")
disc_ken.2015.2020 <- flu_dLE(LT = Ken_LT, r = 0.03, selectTime = "2015-2020") %>% 
  mutate(Period = "2015-2020")

disc <- rbind(disc_ken.2010.2015, disc_ken.2015.2020) %>%
  select(Age, dLEx, Period) 
names(disc) <- c("Age", "LE_disc", "Period")


#Combined LE, disc_LE for both periods ----
df.2010.2020.with.LE <- undisc %>% 
  left_join(disc, by = c("Age", "Period"))

#df.2010.2020.with.LE <- read.csv(here::here("Econ/Data", "Kenya_LE.csv"))
#Kenya demography ----
source(here::here("Econ/Scripts", "Kenya_demography.mod.R"))


#Model number of deaths ----

deaths <- samples.combined.df %>% 
  select(Scenario, Year, Sample, contains("deaths")) %>% 
  select(-total.deaths.all.ages)

names(deaths) <- c("Scenario", "Year", "Sample", 
                   "Below_1","One_to_five",
                   "Six_to_14", "Fifteen_to_19", 
                   "Twenty_to_49", "Fifty_and_above")

deaths.pivot<- deaths %>% 
  pivot_longer(-c(Scenario, Year, Sample), names_to = "Model_age_band", values_to = "deaths.per.age.gp")

# ddf1 <- deaths.pivot %>%  
#   left_join(demography.pivot.gp, by = c("age_group", "Year")) %>% 
#   mutate(flu_mort = deaths.per.age.gp/numbers.per.age.gp) %>% 
#   left_join(demography.pivot, by = c("Year", "age_group")) %>% 
#   mutate(wtd.flu_mort = flu_mort*numbers.per.age/numbers.per.age.gp)

#combine and calculate wtd flu mortality ----
ddf2 <- deaths.pivot %>% 
  left_join(demography.pivot.gp2, by = c("Model_age_band", "Year")) %>% 
  mutate(wtd.deaths = deaths.per.age.gp * age.wt,
         #wtd.flu_mort = deaths.per.age.gp*age.wt/numbers.per.age.gp
         Period = case_when(
           between(Year, 2010, 2014) ~ "2010-2015",
           TRUE ~ "2015-2020"
         ))

df.2010.2020.with.LE %>% 
  mutate(Age2 = case_when(
    between(Age, 0, 84) ~ paste0("Age_", Age),
    Age >= 85 ~ "Age_85.and.over"
  )) %>% 
  group_by(Period, Age2) %>% 
  mutate(LE_undisc2 = mean(LE_undisc),
         LE_disc2 = mean(LE_disc),
         diff.undisc = LE_undisc - LE_undisc2,
         diff.disc = LE_disc - LE_disc2) %>% #View()
  ungroup() %>% 
  filter(Age <= 85) %>% #View()
  select(Period,Age, Age2, LE_undisc2, LE_disc2) -> LE.data

names(ddf2)
names(LE.data) 
#Period, Age=Age.edit

range(ddf2$Age.edit)
range(LE.data$Age)

ddf2 %>% 
  left_join(LE.data, by = c("Period" = "Period", "Age.edit" = "Age")) %>% #View()
  mutate(YLL00 = wtd.deaths*LE_undisc2,
         YLL03 = wtd.deaths*LE_disc2) -> data.with.yll


data.with.yll %>% 
  group_by(Scenario, Year, Sample, Model_age_band) %>% 
  dplyr::summarise(total.deaths = sum(wtd.deaths),
                   total.YLL00 = sum(YLL00),
                   total.YLL03 = sum(YLL03)) %>% 
  pivot_longer(-c(Scenario, Year, Sample, Model_age_band), 
               names_to = "variable", 
               values_to = "estimate") %>% 
  pivot_wider(names_from = c("Model_age_band", "variable"),
              values_from = "estimate") -> pivot.data.with.yll

# #to check that total deaths add up properly
# pivot.data.with.yll %>% 
#   select(Scenario, Year, Sample,
#          #Deaths
#          Below_1_total.deaths, One_to_five_total.deaths, Six_to_14_total.deaths, 
#          Fifteen_to_19_total.deaths, Twenty_to_49_total.deaths, Fifty_and_above_total.deaths) %>% 
#   mutate(total.deaths = Below_1_total.deaths+ One_to_five_total.deaths + Six_to_14_total.deaths +
#            Fifteen_to_19_total.deaths + Twenty_to_49_total.deaths + Fifty_and_above_total.deaths) -> deaths2



pivot.data.with.yll <- pivot.data.with.yll %>%
  select(Scenario, Year, Sample,
         #Deaths
         Below_1_total.deaths, One_to_five_total.deaths, Six_to_14_total.deaths,
         Fifteen_to_19_total.deaths, Twenty_to_49_total.deaths, Fifty_and_above_total.deaths,
         #YLL00
         Below_1_total.YLL00, One_to_five_total.YLL00, Six_to_14_total.YLL00,
         Fifteen_to_19_total.YLL00, Twenty_to_49_total.YLL00, Fifty_and_above_total.YLL00,
         #YLL03
         Below_1_total.YLL03, One_to_five_total.YLL03, Six_to_14_total.YLL03,
         Fifteen_to_19_total.YLL03, Twenty_to_49_total.YLL03, Fifty_and_above_total.YLL03
  ) %>%
  mutate(total.deaths2 = Below_1_total.deaths+ One_to_five_total.deaths + Six_to_14_total.deaths +
           Fifteen_to_19_total.deaths + Twenty_to_49_total.deaths + Fifty_and_above_total.deaths,
         total.YLL00.all.ages = Below_1_total.YLL00 + One_to_five_total.YLL00 + Six_to_14_total.YLL00 +
           Fifteen_to_19_total.YLL00 + Twenty_to_49_total.YLL00 + Fifty_and_above_total.YLL00,
         total.YLL03.all.ages = Below_1_total.YLL03 + One_to_five_total.YLL03 + Six_to_14_total.YLL03 +
           Fifteen_to_19_total.YLL03 + Twenty_to_49_total.YLL03 + Fifty_and_above_total.YLL03)




# 
# data.with.yll %>% 
#   group_by(Scenario, Year, Sample, Model_age_band) %>% 
#   dplyr::summarise(total.deaths = sum(wtd.deaths),
#                    total.YLL00 = sum(YLL00),
#                    total.YLL03 = sum(YLL03)) %>% 
#   pivot_longer(-c(Scenario, Year, Sample, Model_age_band), 
#                names_to = "variable", 
#                values_to = "estimate") %>% 
#   filter(Year < 2020 & variable == "total.deaths") %>% 
#   ggplot(aes(x = factor(Year), y = estimate))+
#   geom_boxplot(aes(color = factor(Scenario)))+
#   facet_grid(variable~.)+
#   ylim(0, 5000)
#   