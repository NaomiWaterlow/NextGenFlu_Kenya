#Updated 5/7/2022
library(here)

#Run only if running this script directly without running 01_Public-health-outcomes_mine_short.T
#Load, prepare data ----
#source(here::here("Econ/Scripts", "00_1_read.all.samples.R"))

#My plot codes ----

#Cumulative distributions of costs----
# all.samples %>% 
#   filter(!Scenario == 1) %>% 
#   #select(icer.total.per.daly.03, icer.direct.medical.per.daly.03) %>% 
#   select(contains("icer"), Scenario, Year) %>% 
#   pivot_longer(-c(Scenario,Year), #icer.total.per.daly.03:icer.direct.medical.per.daly.03,  
#                values_to = "ICER_per_DALY_averted", 
#                names_to = "Costs.name") %>% 
#   mutate(Costs = case_when(
#     str_detect(Costs.name, "direct") ~ "Direct medical costs",
#     TRUE ~ "Total costs"
#   ),
#   DALY = str_extract(Costs.name, "\\d+")
#   ) %>% #View()
#   filter(!is.na(DALY)) %>% 
#   filter(DALY == "00") %>% 
#   ggplot(aes(ICER_per_DALY_averted, group=Costs, color=Costs))+ 
#   stat_ecdf(aes(lty=Costs, color=Costs), size=1) +
#   labs(y = "Cumulative distribution", x = " ") +
#   #ggtitle(paste0(year)) +
#   #geom_rect(aes(xmin=1710.5*3, xmax=Inf, ymin=0, ymax=1), fill = "#708090", alpha = 0.019) +
#   # geom_rect(aes(xmin=1710.5, xmax=1710.5*3, ymin=0, ymax=1),fill ="#708090", alpha = 0.017) +
#   # geom_rect(aes(xmin=0, xmax=1710.5, ymin=0, ymax=1), fill = "#708090", alpha = 0.013) +
#   # geom_rect(aes(xmin=-Inf, xmax=0, ymin=0, ymax=1), fill = "#708090", alpha = 0.009) + 
#   scale_color_manual(values = c("#b35806", "#542788")) +
#   facet_grid(Scenario~Year, scales = "free")+
#   #geom_vline(aes(xintercept=0),linetype="dashed")+
#   #geom_vline(aes(xintercept=1710.5),linetype="dashed")+
#   #geom_vline(aes(xintercept=1710.5*3),linetype="dashed")+
#   theme(panel.background = element_blank()
#         , legend.key = element_blank()
#         , legend.position = "bottom"
#         , plot.title = element_text(family = "sans", color="#666666", face=, size=8, hjust=0)
#         , axis.title.y = element_text(family = "sans", color="#666666", size=8)
#         , axis.title.x = element_text(family = "sans", color="#666666", size=8))

#Total incremental costs by cases averted----

# ay.outcomes.costs %>% 
#   select(ay_Scenario3, ay_Sample,
#          ay_disc.incremental.total.costs,
#          ay_cases.averted, ay_Vaccine.doses) %>% 
#   group_by(ay_Scenario3) %>% 
#   dplyr::summarise(median.costs = median(ay_disc.incremental.total.costs)/1e6,
#                    sd.costs = sd(ay_disc.incremental.total.costs)/1e6,
#                    median.cases = median(ay_cases.averted)/1e6,
#                    sd.cases = sd(ay_cases.averted)/1e6,
#                    median.vacc = median(ay_Vaccine.doses)/1e6,
#                    sd.vacc = sd(ay_Vaccine.doses)/1e6) %>% 
#   filter(!ay_Scenario3 == "No vaccination") %>% 
#   mutate(across(where(is.numeric), round, 2)) -> medians.to.plot


ay.outcomes.costs %>% 
  select(ay_Scenario3, ay_Sample,
         ay_disc.incremental.total.costs,
         ay_disc_cases.averted, ay_Vaccine.doses) %>% 
  pivot_longer(-c(ay_Scenario3, ay_Sample), names_to = "Measure", values_to = "value") %>% #View()
  group_by(ay_Scenario3, Measure) %>%  
  dplyr::summarise(median = round(median(value, na.rm = TRUE)/1e6,2),
                   #median = median(value, na.rm = TRUE),
                   lower_ci = round(quantile(value, 0.025, na.rm = TRUE)/1e6,2),
                   upper_ci = round(quantile(value, 0.975, na.rm = TRUE)/1e6,2)) %>%
  ungroup() %>% 
  filter(!ay_Scenario3 == "No vaccination") %>% 
  pivot_longer(-c(ay_Scenario3, Measure), names_to = "stat", values_to = "val") %>% 
  pivot_wider(names_from = c(Measure, stat), values_from = val, names_sep = "_") -> medians.to.plot2


tiff(here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
                "inc.costs_cases.averted.tiff"),
     height = 2000, width = 2000, res = 300)
a<- medians.to.plot2 %>% ggplot(aes(y = ay_disc.incremental.total.costs_median, 
                                    x = ay_disc_cases.averted_median))+
  geom_point(aes(col = ay_Scenario3, shape = ay_Scenario3), size = 3)+
  geom_errorbar(aes(ymin=ay_disc.incremental.total.costs_lower_ci, 
                    ymax = ay_disc.incremental.total.costs_upper_ci,
                    col = ay_Scenario3), width=.1, lwd = 1)+
  geom_errorbar(aes(xmin=ay_disc_cases.averted_lower_ci, 
                    xmax = ay_disc_cases.averted_upper_ci,
                    col = ay_Scenario3), width=.7, lwd = 1)+
  xlab("Reduction in number of cases (millions)")+
  ylab("Discounted incremental total costs (2010 - 2019) \n(millions USD)")+
  labs(col = "Vaccine", shape = "Vaccine")+
  #guides(guide_legend(title="New Legend Title"))+
  #xlim(-5,15)+
  #ylim(-100,1000)+
  # geom_pointrange(aes(y = median, ymin = `lower 95% CI limit`,
  #                     ymax = `upper 95% CI limit`,
  #                     color = factor(Scenario)), size = 1)+
  #geom_hline(yintercept = 1710*3)+
  theme_bw()+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  scale_color_manual(values = c(#"#d73027",
    #"#fc8d59", 
    "orange1", 
    "#91cf60", 
    #"#1a9850"
    "#92c5de",
    #"#4393c3"
    "#3288bd",
    "purple"
  ))+
  theme(#axis.line = element_line(colour = "black"),
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 14), 
    strip.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    panel.border = element_blank(),
    panel.background = element_blank())+
  guides(color = guide_legend(nrow = 2))
#facet_grid(.~Year, scales = "free_x")
print(a)
dev.off()

# ggsave(here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
#                   "inc.costs_cases.averted_23.jpeg"), 
#        height = 1200, width = 1500, units = "px", limitsize = FALSE)



#Total vaccine doses by cases averted----
tiff(here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
                "vacc.doses_cases.averted.tiff"),
     height = 2000, width = 2000, res = 300)
b<- medians.to.plot2 %>% ggplot(aes(y = ay_Vaccine.doses_median, 
                                    x = ay_disc_cases.averted_median))+
  geom_point(aes(col = ay_Scenario3, shape = ay_Scenario3), size = 3)+
  geom_errorbar(aes(ymin=ay_Vaccine.doses_lower_ci, 
                    ymax = ay_Vaccine.doses_upper_ci,
                    col = ay_Scenario3), width=.1, lwd = 1)+
  geom_errorbar(aes(xmin=ay_disc_cases.averted_lower_ci, 
                    xmax = ay_disc_cases.averted_upper_ci,
                    col = ay_Scenario3), width=.7, lwd = 1)+
  xlab("Reduction in number of cases (millions)")+
  ylab("Vaccine doses (millions)")+
  labs(col = "Vaccine", shape = "Vaccine")+
  #guides(guide_legend(title="New Legend Title"))+
  #xlim(-5,15)+
  #ylim(-100,1000)+
  # geom_pointrange(aes(y = median, ymin = `lower 95% CI limit`,
  #                     ymax = `upper 95% CI limit`,
  #                     color = factor(Scenario)), size = 1)+
  #geom_hline(yintercept = 1710*3)+
  theme_bw()+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  scale_color_manual(values = c(#"#d73027",
    #"#fc8d59", 
    "orange1", 
    "#91cf60", 
    #"#1a9850"
    "#92c5de",
    #"#4393c3"
    "#3288bd",
    "purple"
  ))+
  theme(#axis.line = element_line(colour = "black"),
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12), 
    axis.text = element_text(size = 12), 
    strip.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "bottom",
    panel.border = element_blank(),
    panel.background = element_blank())+
  guides(color = guide_legend(nrow = 2))
#facet_grid(.~Year, scales = "free_x")
print(b)
dev.off()

# ggsave(here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
#                   "vacc.doses_cases.averted_2.jpeg"), 
#        height = 1200, width = 1500, units = "px", limitsize = FALSE)



#Total vaccine doses by total incremental costs----
tiff(here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
                "vacc.doses_inc.costs.tiff"),
     height = 2000, width = 2000, res = 300)
c<- medians.to.plot2 %>% ggplot(aes(y = ay_Vaccine.doses_median, 
                                    x = ay_disc.incremental.total.costs_median))+
  geom_point(aes(col = ay_Scenario3, shape = ay_Scenario3), size = 3)+
  geom_errorbar(aes(ymin=ay_Vaccine.doses_lower_ci, 
                    ymax = ay_Vaccine.doses_upper_ci,
                    col = ay_Scenario3), width=.1, lwd = 1)+
  geom_errorbar(aes(xmin=ay_disc.incremental.total.costs_lower_ci, 
                    xmax = ay_disc.incremental.total.costs_upper_ci,
                    col = ay_Scenario3), width=.1, lwd = 1)+
  xlab("Discounted incremental total costs (millions USD)")+
  ylab("Vaccine doses (millions)")+
  labs(col = "Vaccine", shape = "Vaccine")+
  #guides(guide_legend(title="New Legend Title"))+
  #xlim(-5,15)+
  #ylim(-100,1000)+
  # geom_pointrange(aes(y = median, ymin = `lower 95% CI limit`,
  #                     ymax = `upper 95% CI limit`,
  #                     color = factor(Scenario)), size = 1)+
  #geom_hline(yintercept = 1710*3)+
  theme_bw()+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  scale_color_manual(values = c(#"#d73027",
    #"#fc8d59", 
    "orange1", 
    "#91cf60", 
    #"#1a9850"
    "#92c5de",
    #"#4393c3"
    "#3288bd",
    "purple"
  ))+
  theme(#axis.line = element_line(colour = "black"),
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12), 
    axis.text = element_text(size = 12), 
    strip.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "bottom",
    panel.border = element_blank(),
    panel.background = element_blank())+
  guides(color = guide_legend(nrow = 2))
#facet_grid(.~Year, scales = "free_x")
print(c)
dev.off()

# ggsave(here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
#                   "vacc.doses_inc.costs_2.jpeg"), 
#        height = 1200, width = 1500, units = "px", limitsize = FALSE)




#Combine all three ----
d<- arrangeGrob(a + labs(tag = "A")  + theme(legend.position = "none", plot.tag = element_text()),
                b + labs(tag = "B")  + theme(legend.position = "none", plot.tag = element_text()),
                ncol = 2)

e<- arrangeGrob(d, 
                c + labs(tag = "C")  + theme(legend.position = "bottom", plot.tag = element_text()), 
                nrow = 2)

plot(e)

ggsave(here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
                  "combined_3.jpeg"), e,
       height = 1800, width = 2050, units = "px", limitsize = FALSE)


# egg::ggarrange(a,b,c,ncol = 1,
#                labels = c("A","B","C"))
