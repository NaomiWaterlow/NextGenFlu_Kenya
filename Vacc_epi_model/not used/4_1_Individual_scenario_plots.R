# individual scenario plots

scenario_to_plot <- 80


plot_subset <- vaccination_ratio_store3[Vacc_scenario %in% c(scenario_to_plot)]
plot_subset$Date <- as.Date(plot_subset$Date, origin = "1970-01-01")

IMMUNITY <- ggplot(plot_subset, aes(x = Date, y =percent_immune, colour= age_group,
                              group = interaction(age_group, virus_type),
                              linetype = virus_type)) +
        geom_line() + 
        facet_grid(Vacc_scenario~.) +
        labs(y= "Percentage immune",
             colour = "Age group", 
             x = "Date", 
             linetype = "Virus") +
        theme_linedraw()# +
        # theme(axis.text = element_text(size =15),
        #       axis.title = element_text(size = 15),
        #       legend.text = element_text(size=15),
        #       legend.title = element_text(size=15)) 



total_cases_time_temp <- total_cases_time[scenario %in% c(3,scenario_to_plot)]
total_cases_time_temp <- as.data.frame(total_cases_time_temp)
# plot
EPIDEMICS_IND <-ggplot(total_cases_time_temp,aes(x = Date, y = total_cases, group =interaction(sample, scenario),
                                                 colour = scenario)) +
  geom_line(alpha = 0.4) + 
  facet_grid(~epidemic, scales = "free_x") +
  theme_linedraw() + 
  labs(x = "Date", y = "Total cases", title = "Epidemics: base_scenario vs current scenario", 
       colour = "Scenario") #+ 
  # theme(axis.title = element_text(size = 12), 
  #       axis.text = element_text(size = 12))


all_combined_sub <- all_combined[Vacc_scenario == scenario_to_plot]

SURVIVAL <- ggplot(all_combined_sub, aes(x = Date, y = cumulative)) +
  geom_line() + theme_linedraw() +
  labs(x = "Date", y = "Cumulative infections") + 
  # theme(axis.title = element_text(size = 12), 
  #       axis.text = element_text(size = 12)) + 
  geom_ribbon(aes(ymin=cum_upper,ymax=cum_lower), alpha = 0.3, colour = NA)


#TODO

FOI_summary[, ]

FOI_summary_sub <- FOI_summary[c(3, scenario_to_plot),]
FOI_summary_sub[, total_FOI := AH3N2 + AH1N1 + FluB]
FOI_summary_sub <- FOI_summary_sub[, c("Vacc_scenario", "epidemic", "total_FOI")]
to_plot <- melt.data.table(FOI_summary_sub, id.vars = "Vacc_scenario")
to_plot$Vacc_scenario <- as.factor(to_plot$Vacc_scenario)
to_plot$nice_names <- "NextGen"
to_plot[Vacc_scenario == 3, nice_names := "Base"]


RATIOS <- ggplot(to_plot, aes(x = nice_names, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position="dodge") +
  theme_linedraw()



grid.arrange(IMMUNITY, EPIDEMICS_IND, SURVIVAL, RATIOS, 
             layout_matrix = rbind(c(1,1,1,3),
                                   c(4,2,2,2)))






