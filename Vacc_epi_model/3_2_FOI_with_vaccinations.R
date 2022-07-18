# calculate the impact of vaccination on the background FOI

# the vaccination at each point
vaccination_ratio_store2 <- as.data.table(vaccination_ratio_store2)

# calculate teh proportion immune
vaccination_ratio_store2[, prop_immune_1 := prop_v1*prop_Rv1]
vaccination_ratio_store2[, prop_immune_2 := prop_v2*prop_Rv2]
vaccination_ratio_store2[, prop_immune_3 := prop_v3*prop_Rv3]
vaccination_ratio_store2[, prop_immune_4 := prop_v4*prop_Rv4]
vaccination_ratio_store2[, prop_immune_5 := prop_v5*prop_Rv5]
vaccination_ratio_store2[, prop_immune_6 := prop_v6*prop_Rv6]


#vaccination_ratio_store2 <- vaccination_ratio_store2[,19:29]

# frmat population sizes (assuming ratio stays constant over time)
# population[,1] <- as.numeric(population[,1])
# population[,2] <- as.numeric(population[,2])
# population[,3] <- as.numeric(population[,3])
# population[,4] <- as.numeric(population[,4])
# population[,5] <- as.numeric(population[,5])
# population[,6] <- as.numeric(population[,6])
# calculate the relative population in each group
# rel_pop <- c(population[1,1]/ sum(population[1,1:6]), 
#              population[1,2]/ sum(population[1,1:6]), 
#              population[1,3]/ sum(population[1,1:6]), 
#              population[1,4]/ sum(population[1,1:6]), 
#              population[1,5]/ sum(population[1,1:6]), 
#              population[1,6]/ sum(population[1,1:6]))
# # calcuate the relative susceptibility in the 3 age groups of question
vaccination_ratio_store2[, rel_sus_1 := (1-prop_immune_1)]
vaccination_ratio_store2[, rel_sus_2 := (1-prop_immune_2)]
vaccination_ratio_store2[, rel_sus_3 := (1-prop_immune_3)]
vaccination_ratio_store2[, rel_sus_4 := (1-prop_immune_4)]
vaccination_ratio_store2[, rel_sus_5 := (1-prop_immune_5)]
vaccination_ratio_store2[, rel_sus_6 := (1-prop_immune_6)]


# variable is which age group it is
overall_store <- melt.data.table(vaccination_ratio_store2, id.vars = c("Date", "Vacc_scenario", "virus_type" , "week_all" , "week"), 
    measure.vars = c("rel_sus_1", "rel_sus_2", "rel_sus_3", "rel_sus_4", "rel_sus_5", "rel_sus_6"))

# add the relevant lambda estimate to each row of overall store (with some name accounting included)
for(i in 1:nrow(lambda_estimates)){
  
  type <- lambda_estimates[i,"X2"]
  age_grp <- lambda_estimates[i,"X3"]
  if(age_grp == "1"){ age_grp = "rel_sus_1"
  }else if(age_grp == "2"){ age_grp = "rel_sus_2"
  }else if(age_grp == "3"){ age_grp = "rel_sus_3"
  }else if(age_grp == "4"){ age_grp = "rel_sus_4"
  }else if(age_grp == "5"){ age_grp = "rel_sus_5"
  }else if(age_grp == "6"){ age_grp = "rel_sus_6"
  }
  
  if(type == "Flu_B"){ type = "B"
  }else if(type == "H3N2"){ type = "AH3N2"
  }else if(type == "H1N1"){ type = "AH1N1"
  }
  
  lambda_temp <- lambda_estimates[i,"X1"]
  overall_store[virus_type == type & variable == age_grp, lambda_week := lambda_temp]
  
}


  
# multiply together: total infections = lambda * number of weeks * 
#                                 Sum across age groups of 
#                                       (frequency of age group *(1-immunity in age group) / ascertainment of age group) *
overall_store$lambda_week <- as.numeric(overall_store$lambda_week)
overall_store$value <- as.numeric(overall_store$value)
# for each subtype and age group, lambda * proportion susceptle / relevant ascenrtainmentr ates (stored in multipier)
overall_store[virus_type == "AH3N2" & variable == "rel_sus_1",
              infections := (lambda_week*value) /multipliers_H3[1]]
overall_store[virus_type == "AH3N2" & variable == "rel_sus_2",
              infections := (lambda_week*value) /multipliers_H3[2]]
overall_store[virus_type == "AH3N2" & variable == "rel_sus_3",
              infections := (lambda_week*value) /multipliers_H3[3]]
overall_store[virus_type == "AH3N2" & variable == "rel_sus_4",
              infections := (lambda_week*value) /multipliers_H3[3]]
overall_store[virus_type == "AH3N2" & variable == "rel_sus_5",
              infections := (lambda_week*value) /multipliers_H3[3]]
overall_store[virus_type == "AH3N2" & variable == "rel_sus_6",
              infections := (lambda_week*value) /multipliers_H3[3]]

overall_store[virus_type == "AH1N1" & variable == "rel_sus_1",
              infections := (lambda_week*value) /multipliers_H1[1]]
overall_store[virus_type == "AH1N1" & variable == "rel_sus_2",
              infections := (lambda_week*value) /multipliers_H1[2]]
overall_store[virus_type == "AH1N1" & variable == "rel_sus_3",
              infections := (lambda_week*value) /multipliers_H1[3]]
overall_store[virus_type == "AH1N1" & variable == "rel_sus_4",
              infections := (lambda_week*value) /multipliers_H1[3]]
overall_store[virus_type == "AH1N1" & variable == "rel_sus_5",
              infections := (lambda_week*value) /multipliers_H1[3]]
overall_store[virus_type == "AH1N1" & variable == "rel_sus_6",
              infections := (lambda_week*value) /multipliers_H1[3]]

overall_store[virus_type == "B" & variable == "rel_sus_1",
              infections := (lambda_week*value) /multipliers_B[1]]
overall_store[virus_type == "B" & variable == "rel_sus_2",
              infections := (lambda_week*value) /multipliers_B[2]]
overall_store[virus_type == "B" & variable == "rel_sus_3",
              infections := (lambda_week*value) /multipliers_B[3]]
overall_store[virus_type == "B" & variable == "rel_sus_4",
              infections := (lambda_week*value) /multipliers_B[3]]
overall_store[virus_type == "B" & variable == "rel_sus_5",
              infections := (lambda_week*value) /multipliers_B[3]]
overall_store[virus_type == "B" & variable == "rel_sus_6",
              infections := (lambda_week*value) /multipliers_B[3]]

# overall_store[virus_type == "AH3N2", infections_AH3N2 := 
#               lambda_week*(((freq_H3[1]*rel_sus_1)/multipliers_H3[1]) + 
#                              ((freq_H3[2]*rel_sus_2)/multipliers_H3[2]) + 
#                              ((freq_H3[3]*rel_sus_3)/multipliers_H3[3])) ]
#                       
# overall_store[virus_type == "AH1N1", infections_AH1N1 := 
#               lambda_week*(((freq_H1[1]*rel_sus_1)/multipliers_H1[1]) + 
#                              ((freq_H1[2]*rel_sus_2)/multipliers_H1[2]) + 
#                              ((freq_H1[3]*rel_sus_3)/multipliers_H1[3])) ]
# 
# overall_store[virus_type == "B", infections_B := 
#               lambda_week*(((freq_H3[1]*rel_sus_1)/multipliers_B[1]) + 
#                              ((freq_B[2]*rel_sus_2)/multipliers_B[2]) + 
#                              ((freq_B[3]*rel_sus_3)/multipliers_B[3])) ]


FOI_summary <- overall_store[, sum(infections, na.rm = T), by = c("Vacc_scenario","virus_type", "variable")]

ggplot(FOI_summary[variable == "rel_sus_1"], aes(x = Vacc_scenario, y = V1 )) + 
         geom_bar(stat = "identity") + facet_grid(virus_type~.)







