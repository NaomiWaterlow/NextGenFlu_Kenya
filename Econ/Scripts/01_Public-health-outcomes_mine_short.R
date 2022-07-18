#Updated 5/7/2022

###########################README
# this file calculates the public health outcomes for the 5 different vaccination scenarios 
# including the base scenario. This file calls in the following R script

#02_a_outputs1outcomes.R, which in turn calls in other script files. 

library(here)


colname1 <- c ("Sample", "cases <1", "cases 1-5", "cases 6-14", "cases 15-19", "cases 20-49", "cases >50", "cases all ages"
               , "upper resp cases <1", "upper resp cases 1-5", "upper resp cases 6-14", "upper resp cases 15-19", "upper resp cases 20-49", "upper resp cases >50", "upper resp cases all ages"
               ,"lower resp cases <1", "lower resp cases 1-5", "lower resp cases 6-14", "lower resp cases 15-19", "lower resp cases 20-49", "lower resp cases >50", "lower resp cases all ages"
               ,"total deaths <1", "total deaths 1-5", "total deaths 6-14", "total deaths 15-19", "total deaths 20-49", "total deaths >50", "total deaths all ages"
               ,"URT OPC visit <1", "URT OPC visit 1-5", "URT OPC visit 6-14", "URT OPC visit 15-19", "URT OPC visit 20-49", "URT OPC visit >50", "URT OPC visit all ages"
               , "Hosp <1", "Hosp 1-5", "Hosp 6-14", "Hosp 15-19", "Hosp 20-49", "Hosp >50", "Hosp all ages"
               ,"LRT non hosp <1", "LRT non hosp 1-5", "LRT non hosp 6-14", "LRT non hosp 15-19", "LRT non hosp 20-49", "LRT non hosp >50", "LRT non hosp all ages"
               , "total YLD00 <1", "total YLD00 1-5", "total YLD00 6-14", "total YLD00 15-19", "total YLD00 20-49", "total YLD00 >50", "total YLD00 all ages"
               #, "total YLL00 <1", "total YLL00 1-5", "total YLL00 6-14", "total YLL00 15-19", "total YLL00 20-49", "total YLL00 >50", "total YLL00 all ages"
               #, "total DALY00 <1", "total DALY00 1-5", "total DALY00 6-14", "total DALY00 15-19", "total DALY00 20-49", "total DALY00 >50", "total DALY00 all ages"
               #, "total YLD10 <1", "total YLD10 1-5", "total YLD10 6-14", "total YLD10 15-19", "total YLD10 20-49", "total YLD10 >50", "total YLD10 all ages"
               #, "total YLL10 <1", "total YLL10 1-5", "total YLL10 6-14", "total YLL10 15-19", "total YLL10 20-49", "total YLL10 >50", "total YLL10 all ages"
               #, "total DALY10 <1", "total DALY10 1-5", "total DALY10 6-14", "total DALY10 15-19", "total DALY10 20-49", "total DALY10 >50", "total DALY10 all ages"
               #, "total YLD03 <1", "total YLD03 1-5", "total YLD03 6-14", "total YLD03 15-19", "total YLD03 20-49", "total YLD03 >50", "total YLD03 all ages"
               #, "total YLL03 <1", "total YLL03 1-5", "total YLL03 6-14", "total YLL03 15-19", "total YLL03 20-49", "total YLL03 >50", "total YLL03 all ages"
               #, "total DALY03 <1", "total DALY03 1-5", "total DALY03 6-14", "total DALY03 15-19", "total DALY03 20-49", "total DALY03 >50", "total DALY03 all ages"
               #, "total YLD13 <1", "total YLD13 1-5", "total YLD13 6-14", "total YLD13 15-19", "total YLD13 20-49", "total YLD13 >50", "total YLD13 all ages"
               #, "total YLL13 <1", "total YLL13 1-5", "total YLL13 6-14", "total YLL13 15-19", "total YLL13 20-49", "total YLL13 >50", "total YLL13 all ages"
               #, "total DALY13 <1", "total DALY13 1-5", "total DALY13 6-14", "total DALY13 15-19", "total DALY13 20-49", "total DALY13 >50", "total DALY13 all ages"
               , "OPC costs all ages", "hosp costs all ages", "vaccine admin costs all ages", "vaccine doses price all ages"
               , "OPC transport costs all ages", "hosp transport costs all ages", "vaccine transport costs all ages", "OTC meds costs all ages"          
               , "OPC lost wages all ages", "hosp lost wages all ages", "OPC child care costs", "Hosp child care costs"
)

samples.combined.df <- data.frame()


#Use this code to output a dynamically named variable
# eval(parse(text = paste0("combined", scenario)))
# #From https://stackoverflow.com/questions/5542945/opposite-of-rs-deparsesubstitutevar


#Loop to generate outcome data for each year and scenario
for (j in 1:length(unique(epi.model.output$Year))){
  #Define which year to conduct analyses for
  year <- unique(epi.model.output$Year)[j]
  
  #Extract all data for that year
  data.year <- epi.model.output %>% filter(Year == year)
  
  for (i in 1:length(unique(epi.model.output$Vacc_scenario))) {
    #Define which scenario within a particular year to run analyses for
    scenario <- unique(epi.model.output$Vacc_scenario)[i]
    
    #extract infection data for that scenario
    casesagegrp <- data.year %>% 
      filter(Vacc_scenario == scenario) %>% 
      select(-c(X, Vacc_scenario, Year))
    
    samples.data <- casesagegrp %>% select(sample)
    
    
    #total vaccine doses used in that year and scenario (output from Naomi's models)
    vaccinedoses <- vaccine.doses %>% 
      filter(Year == year & 
               Scenario == scenario) %>% 
      .$Total.Vaccines
    
    source(here::here("Econ/Scripts", "02_a_outputs1outcomes_mine.R"), print.eval=TRUE)
    
    
    #all samples for each outcome
    samples.output.combined <- cbind(samples.data,
                                     # Per age group outcomes
                                     casesagegrp, urtcases, lrtcases, totaldeaths,
                                     OPCvisit, LRThosp, LRTnonhosp, 
                                     totalYLD00, #totalYLL00, totalDALY00,
                                     #totalYLD10, totalYLL10, totalDALY10,
                                     #totalYLD03, #totalYLL03, totalDALY03,
                                     #totalYLD13, totalYLL13, totalDALY13,
                                     
                                     # Costs overall i.e not divided by age group
                                     #   Direct medical costs
                                     as.data.frame(OPCcostscases[,7]), as.data.frame(hospcostscases[,7]), 
                                     as.data.frame(vaccinecosts.admin), as.data.frame(vaccinecosts.price),
                                     
                                     #   Health care related costs
                                     as.data.frame(OPCtransportcosts[,7]), as.data.frame(hosptransportcosts[,7]),
                                     as.data.frame(vaccinetransportcosts), as.data.frame(OTCmedscosts[,7]),
                                     
                                     # Indirect costs
                                     as.data.frame(OPClostwagescosts[,7]), as.data.frame(hosplostwagescosts[,7]),
                                     as.data.frame(OPCchildcarecosts[,7]), as.data.frame(hospchildcarecosts[,7])
                                     
                                     
    )
    
    colnames(samples.output.combined) <- colname1
    
    samples.output.combined <- samples.output.combined %>% 
      mutate(Scenario = scenario, 
             Year = year,
             Vaccine.doses = vaccinedoses)
    
    #print year and scenario as they are completed
    samples.output.combined %>% select(Year, Scenario) %>% unique() %>% print()
    
    samples.combined.df <- rbind(samples.combined.df, samples.output.combined); rm(samples.output.combined)
    
    df.to.write <- samples.combined.df %>% 
      mutate(Year = year)
  }
}


#write all samples to file
write.csv(samples.combined.df, file = here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
                                                 paste0(filename, "_samples_all_years_scenarios.csv")
))
