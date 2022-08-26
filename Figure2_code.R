# plots for the paper

##### Timings plot #######
hosp_sum_week_m[overall_storage, Date_time := Date, on= c(Date ="week")]
dates_for_foi <- data.table(dates_for_foi)
dates_for_foi[overall_storage, Date_start := Date, on= c(X1 ="week")]
dates_for_foi[overall_storage, Date_end := Date, on= c(X2 ="week")]
colnames(dates_for_foi)[3] <- "variable"
dates_for_foi[,Date_follow := shift(Date_start, -1L, "lag"), by = "variable"]
temp <- as.data.frame(hosp_sum_week_m[variable != "FluA"])

TIMINGS <- ggplot(temp, aes(x = as.Date(Date_time, origin = "1970-01-01"))) + 
  geom_point(aes(y = value),size=0.2)+ geom_line(aes(y = value)) +
  theme_bw()   + 
  facet_grid(variable~., scales = "free_y") +
  labs(x = "Date", y = "Weekly reported cases", title = "A") + 
  geom_rect(data = dates_for_foi, aes(x = as.Date(Date_start, origin = "1970-01-01"),
                                      xmin = as.Date(Date_start, origin = "1970-01-01"),
                                      xmax = as.Date(Date_end, origin = "1970-01-01")),
            ymin = -Inf,
            ymax = Inf,
            alpha = 0.2) +
  geom_rect(data = dates_for_foi, aes(x = as.Date(Date_start, origin = "1970-01-01"),
                                      xmin = as.Date(Date_end, origin = "1970-01-01"),
                                      xmax = as.Date(Date_follow, origin = "1970-01-01")),
            ymin = -Inf,
            ymax = Inf,
            alpha = 0.2,
            fill = "Sienna",
            colour = "Sienna") +
  geom_vline(data = subset(dates_for_foi, variable == "FluB"),
             aes(xintercept = as.Date(as.Date(c("2011-08-12")))), colour = "Sienna") + 
  theme(axis.title = element_text(size = 12), 
        axis.text = element_text(size = 12), 
        strip.text = element_text(size = 12, colour = "white"),
        strip.background = element_rect(fill = "black"), 
        legend.text = element_text(size = 12),  
        legend.title = element_text(size = 12))



overall_storage <- data.table(overall_storage)

summary_table <- overall_storage[,sum(total_infections), by = c("Date", "Vacc_scenario","week", "scenario_nice", "sample")]
summary_table[,cumulative_sum := cumsum(V1), by = c("Vacc_scenario", "scenario_nice", "sample")]
summary_table2 <- summary_table[,quantile(cumulative_sum, 0.5), by = c("Date", "Vacc_scenario","week", "scenario_nice")]
summary_table2$upper <- summary_table[,quantile(cumulative_sum, 0.975), by = c("Date", "Vacc_scenario","week", "scenario_nice")]$V1
summary_table2$lower <- summary_table[,quantile(cumulative_sum, 0.025), by = c("Date", "Vacc_scenario","week", "scenario_nice")]$V1


summary_table2[Vacc_scenario == target_scenarios[1], scenario_nice2 := "No vaccination"]
summary_table2[Vacc_scenario == target_scenarios[2], scenario_nice2 := "Current seasonal"]
summary_table2[Vacc_scenario == target_scenarios[3], scenario_nice2 := "Improved (minimal)"]
summary_table2[Vacc_scenario == target_scenarios[4], scenario_nice2 := "Improved (efficacy)"]
summary_table2[Vacc_scenario == target_scenarios[5], scenario_nice2 := "Improved (breadth)"]
summary_table2[Vacc_scenario == target_scenarios[6], scenario_nice2 := "Universal"]
summary_table2$scenario_nice2 <- factor(summary_table2$scenario_nice2, levels = c(
  "No vaccination",
  "Current seasonal",
  "Improved (minimal)",
  "Improved (breadth)",
  "Improved (efficacy)",

   "Universal" 
))
# summary_table2$scenario_nice <- factor(summary_table2$scenario_nice, levels = c(
#   "NO_V", "CU_V", "IB_V", "IE_V","U_V"
# ))


# plot
SUMMARY <- ggplot(summary_table2, aes(x = as.Date(Date, origin = "1970-01-01"))) + 
  geom_line(aes(y = V1)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper,
                  fill= scenario_nice2), alpha = 0.5)+
  facet_grid(.~scenario_nice2) + 
  theme_bw() +
  scale_fill_manual(values = c("#d73027","orange1", "#91CF60", "#92C5DE", "#3288BD","purple" )) +
  labs(x = "Date", y = "Cumulative Infections", fill = "Vaccine", title = "") + 
  theme(axis.title = element_text(size = 12), 
        axis.text = element_text(size = 12), 
        strip.text = element_text(size = 12), 
        legend.text =  element_text(size = 10), 
        axis.text.x = element_text(angle = -90, hjust = 1), 
        legend.title = element_text(size = 12), 
        strip.background = element_blank(),
        strip.text.x = element_blank())



gA <- ggplotGrob(TIMINGS)
gB <- ggplotGrob(SUMMARY)
maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
gA$widths[2:5] <- as.list(maxWidth)
gB$widths[2:5] <- as.list(maxWidth)
grid.arrange(gA, gB, ncol=1)

tiff(here::here("Figure2.tiff"), height = 2000, width = 3000, res = 300)
grid.arrange(gA, gB, ncol=1)
dev.off()

tiff(here::here(paste0("Supp_",name_run,".tiff")), height = 1000, width = 3000, res = 300)
SUMMARY
dev.off()


summary_table3 <- summary_table[, sum(V1), by = c("Vacc_scenario", "scenario_nice", "sample")]
summary_base <- summary_table3[Vacc_scenario==1]
summary_table3[summary_base, on=c( "sample"), base_cases :=  i.V1]
summary_table3[,averted := base_cases - V1]
summary_table3[,averted_percent := (averted/base_cases)*100]

summary_table3[, median(V1), by = c("Vacc_scenario")]

total_vaccines[, total := rowSums(.SD), .SDcols = 1:18][]
summary_vaccines <- total_vaccines[,sum(total), by = "Scenario"]
colnames(summary_vaccines) <- c("Vacc_scenario", "vaccines")
summary_table3[summary_vaccines, on="Vacc_scenario", vaccines := i.vaccines]
summary_table3[, averted_by_vaccine := averted/vaccines]


summary_table3[,quantile(averted_percent, probs=c(0.025,0.5,0.975)),
               by ="scenario_nice"]

summary_table3[,quantile(averted_by_vaccine, probs=c(0.025,0.5,0.975), 
                         na.rm=T), by ="scenario_nice"]




