all.samples %>% 
  mutate(total.costs.minus.vaccine = total.costs - vaccine.costs.all.ages) %>% 
  select(Scenario2, contains("costs")) %>% #View()
  pivot_longer(-Scenario2, names_to = "cost", values_to = "val") %>% 
  group_by(Scenario2, cost) %>% 
  dplyr::summarise(mean = mean(val)) %>% 
  filter(cost %in% c("OPC.costs.all.ages", "hosp.costs.all.ages", "vaccine.costs.all.ages",
                     "direct.medical.costs", 
                     "direct.medical.costs.minus.vaccine", "direct.medical.costs.minus.vaccine.costs.only",
                     "direct.medical.costs.minus.vaccine.cost.plus.admin",
                     "healthcare.related.costs.minus.vaccine", "healthcare.related.costs",
                     "total.costs", "total.costs.minus.vacc.costs.vacc.transport")) %>% 
  pivot_wider(names_from = "cost", values_from = "mean") %>% #View()
  mutate(total2 = OPC.costs.all.ages + hosp.costs.all.ages,
         total3 = direct.medical.costs - vaccine.costs.all.ages) %>% View()

all.samples %>% 
  select(Scenario, Scenario2, Year, Total.Vaccines, vaccine.costs.all.ages)


all.samples %>% 
  select(Scenario2, contains("costs")) %>% #View()
  pivot_longer(-Scenario2, names_to = "cost", values_to = "val") %>% 
  group_by(Scenario2, cost) %>% 
  dplyr::summarise(mean = mean(val)) %>% 
  filter(str_detect(cost, "direct.medical")) %>% 
  # filter(cost %in% c("OPC.costs.all.ages", "hosp.costs.all.ages", "vaccine.costs.all.ages",
  #                    "direct.medical.costs", 
  #                    "direct.medical.costs.minus.vaccine", "direct.medical.costs.minus.vaccine.costs.only",
  #                    "direct.medical.costs.minus.vaccine.cost.plus.admin",
  #                    "healthcare.related.costs.minus.vaccine", "healthcare.related.costs",
  #                    "total.costs", "total.costs.minus.vacc.costs.vacc.transport")) %>% 
  ggplot(aes(x = Scenario2, y = mean, group = cost, col = cost, shape = cost))+
  geom_point()+ geom_line()+
  theme(legend.position = "bottom")

all.samples %>% 
  select(Scenario2, contains("costs")) %>% #View()
  pivot_longer(-Scenario2, names_to = "cost", values_to = "val") %>% 
  group_by(Scenario2, cost) %>% 
  dplyr::summarise(mean = mean(val)) %>% 
  filter(str_detect(cost, "healthcare.related")) %>% 
  # filter(cost %in% c("OPC.costs.all.ages", "hosp.costs.all.ages", "vaccine.costs.all.ages",
  #                    "direct.medical.costs", 
  #                    "direct.medical.costs.minus.vaccine", "direct.medical.costs.minus.vaccine.costs.only",
  #                    "direct.medical.costs.minus.vaccine.cost.plus.admin",
  #                    "healthcare.related.costs.minus.vaccine", "healthcare.related.costs",
  #                    "total.costs", "total.costs.minus.vacc.costs.vacc.transport")) %>% 
  ggplot(aes(x = Scenario2, y = mean, group = cost, col = cost, shape = cost))+
  geom_point()+ geom_line()+
  theme(legend.position = "bottom")

all.samples %>% 
  mutate(total.costs.minus.vaccine = total.costs - vaccine.costs.all.ages) %>% 
  select(Scenario2, contains("costs")) %>% #View()
  pivot_longer(-Scenario2, names_to = "cost", values_to = "val") %>% 
  group_by(Scenario2, cost) %>% 
  dplyr::summarise(mean = mean(val)) %>% 
  filter(str_detect(cost, "total")) %>% 
  # filter(cost %in% c("OPC.costs.all.ages", "hosp.costs.all.ages", "vaccine.costs.all.ages",
  #                    "direct.medical.costs", 
  #                    "direct.medical.costs.minus.vaccine", "direct.medical.costs.minus.vaccine.costs.only",
  #                    "direct.medical.costs.minus.vaccine.cost.plus.admin",
  #                    "healthcare.related.costs.minus.vaccine", "healthcare.related.costs",
  #                    "total.costs", "total.costs.minus.vacc.costs.vacc.transport")) %>% 
  ggplot(aes(x = Scenario2, y = mean, group = cost, col = cost, shape = cost))+
  geom_point()+ geom_line()+
  theme(legend.position = "bottom")

