
#######  Vaccine scenarios #####
vaccines_summary <- data.table(scenario = c(1:145))

for(i in 1:length(vaccine_scenarios)){
  
  vaccines_summary[i,"waning"] <- 1/(vaccine_scenarios[[i]]$waning_rate)/365.25
  vaccines_summary[i,"efficacy1"] <- vaccine_scenarios[[i]]$efficacy_H3[1,1]
  vaccines_summary[i,"efficacy2"] <- vaccine_scenarios[[i]]$efficacy_H3[1,7]
  vaccines_summary[i,"dates"] <- length(vaccine_scenarios[[i]]$dates)
  vaccines_summary[i,"coverage"] <- vaccine_scenarios[[i]]$coverage[1]
  
}

###### Timings plot #######
hosp_sum_week_m[overall_store, Date_time := Date, on= c(Date ="week")]
dates_for_foi <- data.table(dates_for_foi)
dates_for_foi[overall_store, Date_start := Date, on= c(X1 ="week")]
dates_for_foi[overall_store, Date_end := Date, on= c(X2 ="week")]
colnames(dates_for_foi)[3] <- "variable"
dates_for_foi[,Date_follow := shift(Date_start, -1L, "lag"), by = "variable"]
temp <- as.data.frame(hosp_sum_week_m[variable != "FluA"])

TIMINGS <- ggplot(temp, aes(x = as.Date(Date_time, origin = "1970-01-01"))) + 
  geom_point(aes(y = value),size=0.5)+ geom_line(aes(y = value)) +
  theme_linedraw()   + 
  facet_grid(variable~., scales = "free_y") +
  labs(x = "Date", y = "Weekly reported cases") + 
  geom_rect(data = dates_for_foi, aes(x = as.Date(Date_start, origin = "1970-01-01"),
           xmin = as.Date(Date_start, origin = "1970-01-01"),
           xmax = as.Date(Date_end, origin = "1970-01-01")),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.3) +
  geom_rect(data = dates_for_foi, aes(x = as.Date(Date_start, origin = "1970-01-01"),
    xmin = as.Date(Date_end, origin = "1970-01-01"),
    xmax = as.Date(Date_follow, origin = "1970-01-01")),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.3,
    fill = "purple",
    colour = "purple") +
  geom_vline(data = subset(dates_for_foi, variable == "FluB"),
             aes(xintercept = as.Date(as.Date(c("2011-08-12")))), colour = "purple")



##### Combine and summarise the output #####

#make long
epidemic_summary <- melt.data.table(storage_data, id.vars=c("sample", "week", "epidemic", "scenario"), 
     measure.vars = c("V1", "V2", "V3", "V4", "V5", "V6"))
# add flu type
for(i in 1:length(epidemics_list)){
  epidemic_summary[epidemic == i, virus := epidemics_list[[i]]$flutype]
}

# combine to weekly
epidemic_summary[,week := substring(week,1, nchar(week)-2)]
epidemic_summary_weekly <- epidemic_summary[,sum(value), by = c("week", "scenario", "virus", "variable", "sample")]
# calcualte median and CrI
epidemic_summary2 <- epidemic_summary_weekly[, median(V1), by = c("week","virus", "scenario", "variable")]
epidemic_summary2$lower <- epidemic_summary_weekly[, quantile(V1, probs=0.025), by = c("week","virus", "scenario", "variable")]$V1
epidemic_summary2$upper <- epidemic_summary_weekly[, quantile(V1, probs=0.975), by = c("week","virus" , "scenario", "variable")]$V1
# relabel age groups
overall_store[variable =="rel_sus_1", age_grp := "V1"]
overall_store[variable =="rel_sus_2", age_grp := "V2"]
overall_store[variable =="rel_sus_3", age_grp := "V3"]
overall_store[variable =="rel_sus_4", age_grp := "V4"]
overall_store[variable =="rel_sus_5", age_grp := "V5"]
overall_store[variable =="rel_sus_6", age_grp := "V6"]
# combine the two sources of infections
epidemic_summary2$variable <- as.character(epidemic_summary2$variable)
overall_store[epidemic_summary2, epi_median := i.V1, on = c(week="week", Vacc_scenario="scenario", 
                                                        age_grp ="variable", virus_type = "virus")]
overall_store[epidemic_summary2, epi_lower := i.lower, on = c(week="week", Vacc_scenario="scenario", 
                                                        age_grp ="variable", virus_type = "virus")]
overall_store[epidemic_summary2, epi_upper := i.upper, on = c(week="week", Vacc_scenario="scenario", 
                                                        age_grp ="variable", virus_type = "virus")]
# format
population[,1] <- as.numeric(population[,1])
population[,2] <- as.numeric(population[,2])
population[,3] <- as.numeric(population[,3])
population[,4] <- as.numeric(population[,4])
population[,5] <- as.numeric(population[,5])
population[,6] <- as.numeric(population[,6])
# mean population over the time period
pop1 <- mean(population[,1])
pop2 <- mean(population[,2])
pop3 <- mean(population[,3])
pop4 <- mean(population[,4])
pop5 <- mean(population[,5])
pop6 <- mean(population[,6])
# create total infections
overall_store[,total_infections := infections +epi_median]
overall_store[(is.na(epi_median)), epi_median := 0]
# calculate the number of infections per 10k population
overall_store[age_grp == "V1", pop_infections_10k := (total_infections/pop1)*100000]
overall_store[age_grp == "V2", pop_infections_10k := (total_infections/pop2)*100000]
overall_store[age_grp == "V3", pop_infections_10k := (total_infections/pop3)*100000]
overall_store[age_grp == "V4", pop_infections_10k := (total_infections/pop4)*100000]
overall_store[age_grp == "V5", pop_infections_10k := (total_infections/pop5)*100000]
overall_store[age_grp == "V6", pop_infections_10k := (total_infections/pop6)*100000]
# create nice label for ages
overall_store[age_grp == "V1", age_group_nice := "Age <1"]
overall_store[age_grp == "V2", age_group_nice := "Age 1-5"]
overall_store[age_grp == "V3", age_group_nice := "Age 6-14"]
overall_store[age_grp == "V4", age_group_nice := "Age 15-19"]
overall_store[age_grp == "V5", age_group_nice := "Age 20-49"]
overall_store[age_grp == "V6", age_group_nice := "Age 50+"]
# create nice label for vaccine scenarios
overall_store[Vacc_scenario == target_scenarios[1], scenario_nice := "NO_V"]
overall_store[Vacc_scenario == target_scenarios[2], scenario_nice := "CU_V"]
overall_store[Vacc_scenario == target_scenarios[3], scenario_nice := "IM_V"]
overall_store[Vacc_scenario == target_scenarios[4], scenario_nice := "IE_V"]
overall_store[Vacc_scenario == target_scenarios[5], scenario_nice := "IB_V"]
overall_store[Vacc_scenario == target_scenarios[6], scenario_nice := "U_V"]
# order labels
overall_store$scenario_nice <- factor(overall_store$scenario_nice, levels = c(
  "NO_V", "CU_V", "IM_V", "IE_V", "IB_V", "U_V"
))
# order labels
overall_store$age_group_nice <- factor(overall_store$age_group_nice, levels = c(
  "Age <1", "Age 1-5", "Age 6-14", "Age 15-19", "Age 20-49","Age 50+"
))
# subset only the desired scenarios
temp <- overall_store[Vacc_scenario %in% target_scenarios]
temp$Vacc_scenario <- as.factor(temp$Vacc_scenario)
# plot
ggplot(temp, aes(x = as.Date(Date, origin = "1970-01-01"))) + 
  facet_grid(virus_type~age_group_nice, scales = "free_y") + 
  geom_line(aes(y = pop_infections_10k, colour = scenario_nice)) + 
  theme_linedraw() + 
  scale_colour_manual(values = c("#d73027","#fc8d59", "orange1", "#91cf60", "#1a9850")) +
  labs(x = "Date", y = "Infections per 100'000 population", colour = "Scenario") + 
  theme(axis.title = element_text(size = 12), 
        axis.text = element_text(size = 12), 
        strip.text = element_text(size = 12), 
        axis.text.x = element_text(angle = -75, hjust = -0.3))
  


# add confidence intervals to table
overall_store[is.na(epi_median), epi_median := 0]
overall_store[is.na(epi_lower), epi_lower := 0]
overall_store[is.na(epi_upper), epi_upper := 0]
overall_store[, total_infections := infections + epi_median]
overall_store[, total_infections_lower := infections + epi_lower]
overall_store[, total_infections_upper := infections + epi_upper]

# combine by age group
overall_store2 <- dcast.data.table(overall_store, Date + Vacc_scenario + 
                                     virus_type + week_all + week + scenario_nice ~ 
                                     age_grp,
                                   value.var = c("total_infections", 
                                                 "total_infections_lower", 
                                                 "total_infections_upper"))

overall_store2[,total_infections := total_infections_V1 + total_infections_V2 +total_infections_V3 +
         total_infections_V4 + total_infections_V5 + total_infections_V6]
overall_store2[,total_infections_upper := total_infections_upper_V1 + total_infections_upper_V2 +total_infections_upper_V3 +
         total_infections_upper_V4 + total_infections_upper_V5 + total_infections_upper_V6]
overall_store2[,total_infections_lower := total_infections_lower_V1 + total_infections_lower_V2 +total_infections_lower_V3 +
         total_infections_lower_V4 + total_infections_lower_V5 + total_infections_lower_V6]
# combine by virus type
overall_store2 <- dcast.data.table(overall_store2, Date + Vacc_scenario + week_all + week + 
                   scenario_nice ~ virus_type, value.var = c("total_infections","total_infections_lower", "total_infections_upper" ))

overall_store2[,total_infections := total_infections_AH1N1 + total_infections_AH3N2 + total_infections_B]
overall_store2[,total_infections_lower := total_infections_lower_AH1N1 + total_infections_lower_AH3N2 + total_infections_lower_B]
overall_store2[,total_infections_upper := total_infections_upper_AH1N1 + total_infections_upper_AH3N2 + total_infections_upper_B]
overall_store2 <- overall_store2[,c(1:5,15:17)]
# caclulate the cumulatives
overall_store2[, cumulative_median := cumsum(total_infections), by=list(Vacc_scenario)]
overall_store2[, cumulative_lower := cumsum(total_infections_lower), by=list(Vacc_scenario)]
overall_store2[, cumulative_upper := cumsum(total_infections_upper), by=list(Vacc_scenario)]
# plot
ggplot(overall_store2, aes(x = as.Date(Date, origin = "1970-01-01"))) + 
  geom_line(aes(y = cumulative_median)) + 
   geom_ribbon(aes(ymin = cumulative_lower, ymax = cumulative_upper,
                   fill= scenario_nice), alpha = 0.5)+
  facet_grid(.~scenario_nice) + 
  theme_linedraw() +
  scale_fill_manual(values = c("#d73027","#fc8d59", "orange1", "#91cf60", "#1a9850")) +
  labs(x = "Date", y = "Cumulative Infections", fill = "Scenario") + 
  theme(axis.title = element_text(size = 12), 
        axis.text = element_text(size = 12), 
        strip.text = element_text(size = 12), 
        axis.text.x = element_text(angle = -75, hjust = -0.3))
  

# make sure all the column names are intuitive, for storing and passing on. 
colnames(overall_store)[7] <- "susceptibilty"
save(overall_store, file = here::here(paste0("Epi_model_output",name_run,".Rdata")))

##### NUmber of infections averted per vaccine dose #####

total_vaccines <- na.omit(data.table(total_vaccines))
#TODO, this used a different vaccine format
# total_cases <- overall_store2[, sum(Vaccines_administered), by = "Vacc_scenario"]
# total_cases$upper <- overall_store2[, max(cumulative_upper), by = "Vacc_scenario"]$V1
# total_cases$lower <- overall_store2[, max(cumulative_lower), by = "Vacc_scenario"]$V1
# total_cases$Vaccines <- na.omit(total_vaccines[,sum(X3), by = X2]$V1)
# base_cases <- unlist(total_cases[1,"V1"])
# total_cases[,Averted := 1-(V1-base_cases) ]
# total_cases[,Averted_per_vaccine := Averted/Vaccines]

save(total_vaccines, file = here::here(paste0("Vaccine_model_output_",name_run,".Rdata")))

overall_store[,year := year(as.Date(Date, origin = "1970-01-01"))]
heg <- overall_store[year == 2019]

total_infections_2019 <- heg[, sum(total_infections), by= c("scenario_nice", "age_group_nice")]
total_infections_2019$lower <- heg[, sum(total_infections_lower), by= c("scenario_nice", "age_group_nice")]$V1
total_infections_2019$upper <- heg[, sum(total_infections_upper), by= c("scenario_nice", "age_group_nice")]$V1
total_infections_2019

ggplot(total_infections_2019, aes(x=scenario_nice, y = V1)) + geom_bar(stat="identity") + 
      geom_errorbar(aes(ymin = lower, ymax = upper)) + 
  facet_grid(age_group_nice~.)

total_epi_2019 <- heg[, sum(epi_median), by= c("scenario_nice")]
total_epi_2019$lower <- heg[, sum(epi_lower), by= c("scenario_nice")]$V1
total_epi_2019$upper <- heg[, sum(epi_upper), by= c("scenario_nice")]$V1
total_epi_2019

ggplot(total_epi_2019, aes(x=scenario_nice, y = V1)) + geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin = lower, ymax = upper))


