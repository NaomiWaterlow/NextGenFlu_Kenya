#Updated 5/7/2022
library(here)

#Run only if running this script directly without running 01_Public-health-outcomes_mine_short.T
source(here::here("Econ/Scripts", "00_load_data.R"))

dir.create(here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date)),
           recursive = TRUE)

samples.combined.df <- read.csv(file = here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", read.date),
                                                  paste0(filename, "_samples_all_years_scenarios.csv")
))

names(samples.combined.df)

source(here::here("Econ/Scripts", "yll_calculator.R"))

base.year <- 2010

samples.combined.df2 <- samples.combined.df %>% 
  left_join(pivot.data.with.yll, by = c("Scenario", "Year", "Sample")) %>% 
  mutate(total.DALY00.all.ages = total.YLD00.all.ages + total.YLL00.all.ages,
         #total.DALY03.all.ages = total.YLD00.all.ages + total.YLL03.all.ages,
         disc_total.YLD00.all.ages = total.YLD00.all.ages/(1.03^(Year-base.year)),
         #disc_total.YLL00.all.ages = total.YLL00.all.ages/(1.03^(Year-base.year)),
         disc_total.YLL03.all.ages = total.YLL03.all.ages/(1.03^(Year-base.year)),
         #disc_total.DALY00.all.ages = disc_total.YLD00.all.ages + disc_total.YLL00.all.ages,
         disc_total.DALY03.all.ages = disc_total.YLD00.all.ages + disc_total.YLL03.all.ages)


#read in deflator data
source(here::here("Econ/Scripts", "gdp_deflator.R"))
defl.data.ken$Year <- as.numeric(defl.data.ken$Year)

#calculate inflator when inflating to 2019 values
defl.data.ken <- defl.data.ken %>% 
  mutate(inflator = WB.Deflator[which(.$Year == 2019)]/WB.Deflator)

samples.combined.df2 <- samples.combined.df2 %>% 
  left_join(defl.data.ken[c("Year", "inflator")])

samples.combined.df2 <- samples.combined.df2 %>% 
  mutate(across(matches(paste0(c("costs", "wages"), collapse = "|")),
                .fns = function(x) (x*inflator)#,
                #.names = "inmb_{col}")
  ))

samples.combined.df2 <- samples.combined.df2 %>% 
  mutate(
    vaccine.costs.all.ages = vaccine.admin.costs.all.ages + vaccine.doses.price.all.ages,
    direct.medical.costs = OPC.costs.all.ages + hosp.costs.all.ages + vaccine.costs.all.ages,
    direct.medical.costs.minus.vaccine.costs = OPC.costs.all.ages + hosp.costs.all.ages,
    direct.medical.costs.minus.vaccine.price.only = OPC.costs.all.ages + hosp.costs.all.ages +
      vaccine.admin.costs.all.ages,
    healthcare.related.costs = OPC.transport.costs.all.ages + hosp.transport.costs.all.ages + 
      vaccine.transport.costs.all.ages + OTC.meds.costs.all.ages,
    healthcare.related.costs.minus.vaccine = OPC.transport.costs.all.ages + 
      hosp.transport.costs.all.ages + OTC.meds.costs.all.ages,
    indirect.costs = OPC.lost.wages.all.ages + hosp.lost.wages.all.ages + OPC.child.care.costs + 
      Hosp.child.care.costs,
    total.costs = direct.medical.costs + healthcare.related.costs + indirect.costs,
    total.costs.minus.vaccine.price = total.costs - vaccine.doses.price.all.ages,
    total.costs.minus.vacc.costs.vacc.transport = total.costs - (vaccine.costs.all.ages + vaccine.transport.costs.all.ages)
  )

all.samples <- data.frame()

base.df <- samples.combined.df2 %>% 
  filter(#Year == year & 
    Scenario == 1) %>% 
  select(-X)

names(base.df) <- paste0("base_", names(base.df))


for (i in 1:length(unique(epi.model.output$Vacc_scenario))) {
  
  scenario = unique(samples.combined.df2$Scenario)[i]
  
  play.df <- samples.combined.df2 %>% 
    filter(#Year == year & 
      Scenario == scenario)
  
  player <- play.df %>% 
    full_join(base.df, by = c("Sample" = "base_Sample", "Year" = "base_Year"), keep = TRUE) %>% 
    mutate(
      #Costs
      incremental.total.costs = total.costs - base_total.costs,
      incremental.direct.medical.costs = direct.medical.costs - base_direct.medical.costs,
      #      r = 0.03,
      #      n = Year - base.year,
      #      disc.incremental.total.costs = incremental.total.costs/((1+r)^n)) %>% 
      #Discounting
      #disc.total.costs = total.costs/((1+r)^n),
      
      #cases etc. averted
      cases.averted = base_cases.all.ages - cases.all.ages,
      disc_cases.averted = cases.averted/(1.03^(Year-base.year)),
      hospitalisations.averted = base_Hosp.all.ages - Hosp.all.ages,
      deaths.averted = base_total.deaths.all.ages - total.deaths.all.ages,
      
      #yldaverted
      yld.00.averted = base_total.YLD00.all.ages - total.YLD00.all.ages,
      disc_yld.00.averted = base_disc_total.YLD00.all.ages - disc_total.YLD00.all.ages,
      
      #yllaverted
      yll.00.averted = base_total.YLL00.all.ages - total.YLL00.all.ages,
      disc_yll.03.averted = base_disc_total.YLL03.all.ages - disc_total.YLL03.all.ages,
      
      #dalys averted
      dalys.00.averted = base_total.DALY00.all.ages - total.DALY00.all.ages,
      disc_dalys.03.averted = base_disc_total.DALY03.all.ages - disc_total.DALY03.all.ages
      
      #disc_yll.00.averted = base_disc_total.YLL00.all.ages - disc_total.YLL00.all.ages,
      #yll.03.averted = base_total.YLL03.all.ages - total.YLL03.all.ages,
      #dalys.03.averted = base_total.DALY03.all.ages - total.DALY03.all.ages,
      #disc_dalys.00.averted = base_disc_total.DALY00.all.ages - disc_total.DALY00.all.ages,
    ) %>% 
    select(-c(X, contains("base_")))
  
  #samples
  all.samples <- rbind(all.samples, player); rm(player)
  
}

all.samples <- all.samples %>% 
  select(-c(inflator))

write.csv(all.samples, file = here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
                                         paste0(filename, "_all_samples_all_years_scenarios.csv")
))

