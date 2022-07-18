#Updated 6/7/22
#Script to load all essential files and libraries

library(tidyverse)
library(lubridate)
library(fluEvidenceSynthesis)
library(Rmisc)
library(truncnorm)
library(gridExtra)
library(ggpubr)
library(data.table)
library(here)


#Grouped epi model data from Naomi
epi.model.output <- read.csv(here::here(paste0("Econ/Data/Final/", filename), paste0(filename, "_Naomi_grouped_totals.csv")))
#epi.model.output <- read.csv(here::here("Econ/Data", "2022-05-19_POP_ADD_grouped_totals.csv"))


#Data on number of vaccine doses per scenario and year
load(here::here(paste0("Econ/Data/Final/", filename), paste0(filename, "_Vaccine_model_output.Rdata")))
total_vaccines <- total_vaccines %>% 
  mutate(Year = year(Date),
         Vaccines = (Vaccinated1 + Vaccinated2 + Vaccinated3 + Vaccinated4 + Vaccinated5 +
                       Vaccinated6 + Vaccinated7 + Vaccinated8 + Vaccinated9 + Vaccinated10 +
                       Vaccinated11 + Vaccinated12 + Vaccinated13 + Vaccinated14 + Vaccinated15 +
                       Vaccinated16 + Vaccinated17 + Vaccinated18)*1.15 # incorporating 15% wastage
  )

vaccine.doses <- total_vaccines %>% 
  group_by(Year, Scenario) %>% 
  dplyr::summarise(Total.Vaccines = sum(Vaccines)) ; rm(total_vaccines)

#No. of samples 
(nsample <- length(unique(epi.model.output$sample)))

#Set the base year for discouting 
base.year <- 2010

#Public health values for Kenya (for econ analyses) (what is the source of these data)?
inputs   <- read.csv(here::here("Econ/Data",
                                "ken-public-health-values.csv"))

