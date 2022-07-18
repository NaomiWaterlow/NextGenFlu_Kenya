library(tidyverse)

#comparing campaign mode with year-round
all.samples.campaign <- read.csv(file = here::here(paste0("Econ/Model_outcomes_output", "/", 
                                                          "Naomi", "/", "2022-06-09"),
                                                   paste0("Naomi", "_all_samples_all_years_scenarios.csv")
))

all.samples.campaign <- all.samples.campaign %>% 
  mutate(Scenario2 = case_when(
    Scenario == 1 ~ "NO_V",
    Scenario == 4 ~ "CU_V",
    Scenario == 51~ "IB_V",
    Scenario == 53~ "IE_V",
    TRUE ~ "U_V"
  ),
  Scenario3 = case_when(
    Scenario == 1 ~ "No vaccination",
    Scenario == 4 ~ "Current seasonal",
    Scenario == 51~ "Incremental (breadth)",
    Scenario == 53~ "Incremental (efficacy)",
    TRUE ~ "Universal"
  ))

all.samples.campaign <- all.samples.campaign %>% 
  mutate(Mode = "Campaign")

 
all.samples.year.round <- read.csv(file = here::here(paste0("Econ/Model_outcomes_output", "/", 
                                                            "Sensitivity_year_round", "/", "2022-06-10"),
                                                     paste0("Sensitivity_year_round", "_all_samples_all_years_scenarios.csv")
))

all.samples.year.round <- all.samples.year.round %>% 
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

all.samples.year.round <- all.samples.year.round %>% 
  mutate(Mode = "Year-round")

dim(all.samples.campaign)
dim(all.samples.year.round)


combined <- rbind(all.samples.campaign, all.samples.year.round)

combined %>% 
  select(Scenario3, Year, Sample, Mode, cases.all.ages, total.costs, Vaccine.doses,
         total.DALY03.all.ages, dalys.03.averted) %>% 
  pivot_longer(-c(Scenario3, Year, Sample, Mode), names_to = "param",
               values_to = "value") -> to.plot

param.names <- unique(to.plot$param)
to.plot$Scenario3 <- factor(to.plot$Scenario3, 
                                levels = c("No vaccination",
                                           "Current seasonal",
                                           "Incremental (breadth)", 
                                           "Incremental (efficacy)",
                                           "Universal"))


pdf(file = here::here(paste0("Econ/Model_outcomes_output", "/", "Testing"),
                      "boxplot.campaign.vs.year-round.pdf"), 
    width = 10, height = 8)
for (i in 1:3){
  a<- to.plot %>% 
    filter(param == param.names[i]) %>% 
    ggplot(aes(factor(Year), value))+
    geom_boxplot(aes(fill = factor(Mode)), outlier.size = 0.6,
                 lwd = 0.5, fatten = 1)+
    facet_wrap(Scenario3~., scales = "free")+
    #coord_cartesian(ylim = c(0,5e5))+
    ggtitle(paste(param.names[i]))+
    labs(fill = "Vaccination mode")+
    theme(legend.position = "bottom", axis.text.x = element_text(angle =  90))
  print(a)
}

for (i in 4:5){
  a<- to.plot %>% 
    filter(param == param.names[i]) %>% 
    ggplot(aes(factor(Year), value))+
    geom_boxplot(aes(fill = factor(Mode)), outlier.size = 0.6,
                 lwd = 0.5, fatten = 1)+
    facet_wrap(Scenario3~., scales = "free")+
    coord_cartesian(ylim = c(0,5e5))+
    ggtitle(paste(param.names[i]))+
    labs(fill = "Vaccination mode")+
    theme(legend.position = "bottom", axis.text.x = element_text(angle =  90))
  print(a)
}
dev.off()

base.year <- 2010
#all years together----
ay.outcomes.costs <- combined %>%
  mutate(r = 0.03,
         n = Year - base.year,
         disc.total.costs = total.costs/((1+r)^n),
         disc.incremental.total.costs = incremental.total.costs/((1+r)^n),
         disc.total.costs.minus.vaccine.price = total.costs.minus.vaccine.price/((1+r)^n)) %>% #View()
  select(-c(X, Scenario, Scenario2)) %>%
  pivot_longer(-c(Scenario3, Sample, Year, Mode), names_to = "Measure", values_to = "value") %>% #View()
  group_by(Mode, Scenario3, Sample, Measure) %>% #View()
  dplyr::summarise(total = sum(value)) %>% #View()
  ungroup() %>% 
  pivot_wider(names_from = "Measure", values_from = "total") #%>% View()

names(ay.outcomes.costs) <- paste0("ay_", names(ay.outcomes.costs))


ay.outcomes.costs <- ay.outcomes.costs %>% 
  mutate(icer.total.per.daly.00 = ay_incremental.total.costs/ay_dalys.00.averted,
         icer.total.per.daly.03 = ay_incremental.total.costs/ay_dalys.03.averted,
         icer.disc.total.per.daly.00 = ay_disc.incremental.total.costs/ay_dalys.00.averted,
         icer.disc.total.per.daly.03 = ay_disc.incremental.total.costs/ay_dalys.03.averted
  )



ay.outcomes.costs %>% 
  select(ay_Scenario3, ay_Sample, ay_Mode, ay_cases.all.ages, ay_total.costs, ay_Vaccine.doses,
         ay_total.DALY03.all.ages, ay_dalys.03.averted, icer.disc.total.per.daly.03) %>% 
  pivot_longer(-c(ay_Scenario3, ay_Sample, ay_Mode), names_to = "param",
               values_to = "value") -> ay.to.plot

ay.to.plot$ay_Scenario3 <- factor(ay.to.plot$ay_Scenario3, 
                            levels = c("No vaccination",
                                       "Current seasonal",
                                       "Incremental (breadth)", 
                                       "Incremental (efficacy)",
                                       "Universal"))

ay.param.names <- unique(ay.to.plot$param)


pdf(file = here::here(paste0("Econ/Model_outcomes_output", "/", "Testing"),
                      "ay.boxplot.campaign.vs.year-round.pdf"), 
    width = 10, height = 8)
for (i in 1:3){
  a<- ay.to.plot %>% 
    filter(param == ay.param.names[i]) %>% 
    ggplot(aes(factor(ay_Scenario3), value))+
    geom_boxplot(aes(fill = factor(ay_Mode)), outlier.size = 0.6,
                 lwd = 0.5, fatten = 1)+
    ggtitle(paste(param.names[i]))+
    labs(fill = "Vaccination mode")+
    theme(legend.position = "bottom", axis.text.x = element_text(angle =  90))
  print(a)
}

for (i in 4:5){
  a<- ay.to.plot %>% 
    filter(param == ay.param.names[i]) %>% 
    ggplot(aes(factor(ay_Scenario3), value))+
    geom_boxplot(aes(fill = factor(ay_Mode)), outlier.size = 0.6,
                 lwd = 0.5, fatten = 1)+
    coord_cartesian(ylim = c(0,1e6))+
    ggtitle(paste(ay.param.names[i]))+
    labs(fill = "Vaccination mode")+
    theme(legend.position = "bottom", axis.text.x = element_text(angle =  90))
  print(a)
}
  
for (i in 6){
    a<- ay.to.plot %>% 
      filter(param == ay.param.names[i]) %>% 
      ggplot(aes(factor(ay_Scenario3), value))+
      geom_boxplot(aes(fill = factor(ay_Mode)), outlier.size = 0.6,
                   lwd = 0.5, fatten = 1)+
      coord_cartesian(ylim = c(0,5000))+
      ggtitle(paste(ay.param.names[i]))+
      labs(fill = "Vaccination mode")+
      theme(legend.position = "bottom", axis.text.x = element_text(angle =  90))
    print(a)
}
dev.off()
# load(here::here("Econ/Data/SA-year-round", "2022-06-09_Naomi_Vaccine_model_output_year_round.Rdata"))
# year.round.vaccines <- total_vaccines
# 
# rm(total_vaccines)
# load(here::here("Econ/Data", "2022-05-30_Naomi_Vaccine_model_output.Rdata"))
# 
# campaign.vaccines <- total_vaccines
# rm(total_vaccines)
# 
# library(tidyverse)
# library(lubridate)
# year.round.vaccines <- year.round.vaccines %>% 
#   mutate(Year = year(Date),
#          Vaccines = (Vaccinated1 + Vaccinated2 + Vaccinated3 + Vaccinated4 + Vaccinated5 +
#                        Vaccinated6 + Vaccinated7 + Vaccinated8 + Vaccinated9 + Vaccinated10 +
#                        Vaccinated11 + Vaccinated12 + Vaccinated13 + Vaccinated14 + Vaccinated15 +
#                        Vaccinated16 + Vaccinated17 + Vaccinated18)/0.85 # incorporating 15% wastage
#   )
# 
# campaign.vaccines <- campaign.vaccines %>% 
#   mutate(Year = year(Date),
#          Vaccines = (Vaccinated1 + Vaccinated2 + Vaccinated3 + Vaccinated4 + Vaccinated5 +
#                        Vaccinated6 + Vaccinated7 + Vaccinated8 + Vaccinated9 + Vaccinated10 +
#                        Vaccinated11 + Vaccinated12 + Vaccinated13 + Vaccinated14 + Vaccinated15 +
#                        Vaccinated16 + Vaccinated17 + Vaccinated18)/0.85 # incorporating 15% wastage
#   )
# 
# sum(year.round.vaccines$Vaccines)
# sum(campaign.vaccines$Vaccines)
# 
# vaccine.doses.campaign <- campaign.vaccines %>% 
#   group_by(Year, Scenario) %>% 
#   dplyr::summarise(Total.Vaccines = sum(Vaccines))
# 
# vaccine.doses.year.round <- year.round.vaccines %>% 
#   group_by(Year, Scenario) %>% 
#   dplyr::summarise(Total.Vaccines = sum(Vaccines))
# 
# 
# vaccine.doses.campaign %>% 
#   group_by(Scenario) %>% 
#   dplyr::summarise(total.vacc = sum(Total.Vaccines))
# vaccine.doses.year.round %>% 
#   group_by(Scenario) %>% 
#   dplyr::summarise(total.vacc = sum(Total.Vaccines))


