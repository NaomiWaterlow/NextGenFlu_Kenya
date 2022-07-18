
vaccines_summary <- data.table(scenario  = c(1:145))

for(i in 1:length(vaccine_scenarios)){
  
  vaccines_summary[i,"waning"] <- 1/(vaccine_scenarios[[i]]$waning_rate)/365.25
  vaccines_summary[i,"efficacy1"] <- vaccine_scenarios[[i]]$efficacy_H3[1,1]
  vaccines_summary[i,"efficacy2"] <- vaccine_scenarios[[i]]$efficacy_H3[1,7]
  vaccines_summary[i,"dates"] <- length(vaccine_scenarios[[i]]$dates)
  vaccines_summary[i,"coverage"] <- vaccine_scenarios[[i]]$coverage[1]
  
}

save(vaccines_summary, file=paste0("vacc_scenario_summary_",name_run,".RData"))


overall_storage <- data.table(
  "Date" = rep(overall_store$Date, posterior_sample_size),
  "Vacc_scenario" = rep(overall_store$Vacc_scenario, posterior_sample_size),
  "virus_type" = rep(overall_store$virus_type, posterior_sample_size),
  "week" = rep(overall_store$week, posterior_sample_size),
  "age_grp" = rep(overall_store$age_grp, posterior_sample_size),
  "age_group_nice" = rep(overall_store$age_group_nice, posterior_sample_size),
  "scenario_nice" = rep(overall_store$scenario_nice, posterior_sample_size),
  "background_infections" = rep(overall_store$infections, posterior_sample_size),
  "sample" = rep(unique(epidemic_summary$sample), each = nrow(overall_store))
)

overall_storage[epidemic_summary_weekly, epidemic_infections := V1, 
              on=c(week ="week", Vacc_scenario="scenario", 
                   age_grp = "variable", virus_type = "virus", 
                   sample = "sample")]

overall_storage[is.na(epidemic_infections), epidemic_infections := 0]
overall_storage[,total_infections := background_infections + epidemic_infections]

#save(overall_storage, file="overall_storage.RDS")
save(overall_storage, file=paste0("overall_storage_",name_run,".Rdata"))

