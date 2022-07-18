library(here)

#source(here::here("Econ/Scripts", "00_load_data.R"))

all.samples <- read.csv(file = here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", read.date),
                                          paste0(filename, "_all_samples_all_years_scenarios.csv")
))

# 1 - NO_V, 4 - CU_V, 28 - IM_V, 53 - IE_V, 75 - IB_V, 122 - U_V - current


all.samples <- all.samples %>% 
  mutate(Scenario2 = case_when(
    Scenario == 1 ~ "NO_V",
    Scenario == 4 ~ "CU_V",
    Scenario == 28 ~ "IM_V",
    Scenario == 53 ~ "IE_V",
    Scenario == 75 ~ "IB_V",
    TRUE ~ "U_V"
  ),
  Scenario3 = case_when(
    Scenario == 1 ~ "No vaccination",
    Scenario == 4 ~ "Current seasonal",
    Scenario == 28 ~ "Improved (minimal)",
    Scenario == 53 ~ "Improved (efficacy)",
    Scenario == 75 ~ "Improved (breadth)",
    TRUE ~ "Universal"
  ))

all.samples$Scenario2 <- factor(all.samples$Scenario2, 
                                levels = c("NO_V", "CU_V", "IM_V", "IB_V", "IE_V", "U_V"))
all.samples$Scenario3 <- factor(all.samples$Scenario3, 
                                levels = c("No vaccination",
                                           "Current seasonal",
                                           "Improved (minimal)",
                                           "Improved (breadth)", 
                                           "Improved (efficacy)",
                                           "Universal"))

base.year <- 2010
all.samples <- all.samples %>% 
  mutate(r = 0.03,
         n = Year - base.year,
         disc.total.costs = total.costs/((1+r)^n),
         disc.incremental.total.costs = incremental.total.costs/((1+r)^n),
         disc.total.costs.minus.vaccine.price = total.costs.minus.vaccine.price/((1+r)^n)) %>% 
  select(-c(r, n))

#create df with costs and outcomes data for all  years combined, per sample
ay.outcomes.costs <- all.samples %>%
  select(-c(X, Scenario, Scenario2)) %>%
  pivot_longer(-c(Scenario3, Sample, Year), names_to = "Measure", values_to = "value") %>% #View()
  group_by(Scenario3, Sample, Measure) %>% #View()
  dplyr::summarise(total = sum(value)) %>% #View()
  pivot_wider(names_from = "Measure", values_from = "total") %>%
  ungroup()

names(ay.outcomes.costs) <- paste0("ay_", names(ay.outcomes.costs))

ay.outcomes.costs <- ay.outcomes.costs %>% 
  mutate(#icer.total.per.daly.00 = ay_incremental.total.costs/ay_dalys.00.averted,
    #icer.total.per.disc.daly.03 = ay_incremental.total.costs/ay_disc_dalys.03.averted,
    icer.disc.total.per.daly.00 = ay_disc.incremental.total.costs/ay_dalys.00.averted,
    icer.disc.total.per.disc.daly.03 = ay_disc.incremental.total.costs/ay_disc_dalys.03.averted
  )

write.csv(ay.outcomes.costs, file = here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
                                         paste0(filename, "_ay.outcomes.costs.csv")
))
