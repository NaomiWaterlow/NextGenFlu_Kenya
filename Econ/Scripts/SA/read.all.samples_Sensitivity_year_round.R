library(here)
library(tidyverse)
#rm(list = ls())

#source(here::here("Econ/Scripts", "00_load_data.R"))

all.samples <- read.csv(file = here::here(paste0("Econ/Model_outcomes_output", "/", filename, "/", read.date),
                                          paste0(filename, "_all_samples_all_years_scenarios.csv")
))

# 1 (NO_V), 8 (CU_V), 55 (IB_V), 57 (IE_V), 126 (U_V)


all.samples <- all.samples %>% 
  mutate(Scenario2 = case_when(
    Scenario == 1 ~ "NO_V",
    Scenario == 8 ~ "CU_V",
    Scenario == 55~ "IB_V",
    Scenario == 57~ "IE_V",
    TRUE ~ "U_V"
  ),
  Scenario3 = case_when(
    Scenario == 1 ~ "No vaccination",
    Scenario == 8 ~ "Current seasonal",
    Scenario == 55~ "Incremental (breadth)",
    Scenario == 57~ "Incremental (efficacy)",
    TRUE ~ "Universal"
  ))

all.samples$Scenario2 <- factor(all.samples$Scenario2, 
                                levels = c("NO_V", "CU_V", "IB_V", "IE_V", "U_V"))
all.samples$Scenario3 <- factor(all.samples$Scenario3, 
                                levels = c("No vaccination",
                                           "Current seasonal",
                                           "Incremental (breadth)", 
                                           "Incremental (efficacy)",
                                           "Universal"))


#read in deflator data
source(here::here("Econ/Scripts", "gdp_deflator.R"))
defl.data.ken$Year <- as.numeric(defl.data.ken$Year)

#calculate inflator when inflating to 2019 values
defl.data.ken <- defl.data.ken %>% 
  mutate(inflator = WB.Deflator[which(.$Year == 2019)]/WB.Deflator)

all.samples <- all.samples %>% 
  left_join(defl.data.ken[c("Year", "inflator")])

all.samples2 <- all.samples %>% 
  mutate(across(matches(paste0(c("costs", "wages"), collapse = "|")),
                .fns = function(x) (x*inflator)#,
                #.names = "inmb_{col}")
  ))

base.year <- 2010

#create df with costs and outcomes data for all  years combined, per sample
ay.outcomes.costs <- all.samples2 %>%
  mutate(r = 0.03,
         n = Year - base.year,
         disc.total.costs = total.costs/((1+r)^n),
         disc.incremental.total.costs = incremental.total.costs/((1+r)^n),
         disc.total.costs.minus.vaccine.price = total.costs.minus.vaccine.price/((1+r)^n)) %>% #View()
  select(-c(X, Scenario, Scenario2, r, n)) %>%
  pivot_longer(-c(Scenario3, Sample, Year), names_to = "Measure", values_to = "value") %>% #View()
  group_by(Scenario3, Sample, Measure) %>% #View()
  dplyr::summarise(total = sum(value)) %>% #View()
  pivot_wider(names_from = "Measure", values_from = "total") %>%
  ungroup()


names(ay.outcomes.costs) <- paste0("ay_", names(ay.outcomes.costs))

ay.outcomes.costs <- ay.outcomes.costs %>% 
  mutate(icer.total.per.daly.00 = ay_incremental.total.costs/ay_dalys.00.averted,
         icer.total.per.daly.03 = ay_incremental.total.costs/ay_dalys.03.averted,
         icer.disc.total.per.daly.00 = ay_disc.incremental.total.costs/ay_dalys.00.averted,
         icer.disc.total.per.daly.03 = ay_disc.incremental.total.costs/ay_dalys.03.averted
  )
