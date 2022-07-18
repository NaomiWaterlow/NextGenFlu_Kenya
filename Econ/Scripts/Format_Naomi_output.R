#Updated 5/7/2022
#Run script only if new epi model output received which needs to be formatted
#for econ scripts
library(data.table)
library(dplyr)
library(tidyverse)
library(here)

# Load and transform data ----
#load(here::here("Econ/Data/Final", "2022-06-20_overall_storage_default.Rdata"))
load(here::here("Econ/Data/Final/SA-pop-add", "overall_storage_POP_ADD_WANING.Rdata"))
#load(here::here("Econ/Data/SA-year-round", "2022-06-09_overall_storage_year_round.Rdata"))
#load(here::here("Econ/Data/SA-pop-add", "2022-06-13_overall_storage_POP_ADD_WANING.Rdata"))
#load(here::here("Econ/Data/SA-fixed-reduction", "2022-06-14_overall_storage_FIXED_REDUCTION.Rdata"))

names(overall_storage)
setDT(overall_storage)

overall_storage[, c("Year") := tstrsplit(week, "-", fixed=TRUE, keep = 1L)]
names(overall_storage)

#Number of samples
(nsample <- length(unique(overall_storage$sample)))

overall_storage$sample <- as.factor(overall_storage$sample)

str(overall_storage)
unique(overall_storage$Year)

unique(overall_storage$Vacc_scenario)
table(overall_storage$Vacc_scenario, overall_storage$scenario_nice)
#refer excel table for matching scenario numbers and names

# Format data for analyses ----
overall_storage %>%  
  group_by(Vacc_scenario, Year, sample, age_group_nice) %>% 
  summarise(total = sum(total_infections)) %>% 
  pivot_wider(names_from = age_group_nice, values_from = total) %>% #View()
  ungroup() -> df

#write.csv(df, file = here("Econ/Data", paste0(today, "_", "Naomi_grouped_totals.csv")))
write.csv(df, file = here("Econ/Data/Final/SA-pop-add", paste0("SA-pop-add_", "Naomi_grouped_totals.csv")))
