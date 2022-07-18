#Updated 5/7/2022
#Boxplots of all the parameters

#source(here::here("Econ/Scripts", "00_1_read.all.samples.R"))


#all.samples by year----
names(all.samples)

to.plot <- all.samples %>% 
  select(-X) %>% 
  pivot_longer(-c(Scenario, Scenario2, Scenario3, Year, Sample), names_to = "param",
               values_to = "value") 

param.names <- unique(to.plot$param)
# param.names <- param.names[-which(grepl(paste0(c("10", "13"), collapse = "|"), param.names))]

pdf(file = here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
                      "boxplot.by.year.pdf"), 
    width = 10, height = 7)
for (i in 1:length(param.names)){
  a<- to.plot %>% 
    filter(param == param.names[i]) %>% 
    ggplot(aes(factor(Scenario3), value))+
    geom_boxplot(aes(fill = Scenario3), outlier.size = 0.6,
                 lwd = 0.5, fatten = 1)+
    facet_wrap(Year~., scales = "free", ncol = 4)+
    ggtitle(paste(param.names[i]))+
    labs(fill = "Vaccine")+
    theme(legend.position = "bottom", axis.text.x = element_blank())
  print(a)
}
dev.off()


pdf(file = here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
                      "boxplot.years.stacked.pdf"), 
    width = 10, height = 6)

for (i in 1:length(param.names)){
  a<- to.plot %>% 
    filter(param == param.names[i]) %>% 
    ggplot(aes(factor(Scenario3), value, 
               fill = factor(Year)))+
    geom_boxplot(outlier.size = 0.4, lwd = 0.5, fatten = 1)+
    #facet_wrap(Year~., scales = "free", ncol = 4)+
    ggtitle(paste(param.names[i]))+
    theme_bw()+
    theme(legend.position = "bottom")
  print(a)
}
dev.off()


#ay.outcomes.costs----

names(ay.outcomes.costs)

ay.to.plot <- ay.outcomes.costs %>% 
  #select(-X) %>% 
  pivot_longer(-c(ay_Scenario3, ay_Sample), names_to = "param",
               values_to = "value") 

ay.param.names <- unique(ay.to.plot$param)  
# ay.param.names <- ay.param.names[-which(grepl(paste0(c("10", "13"), collapse = "|"), 
#                                               ay.param.names))]


pdf(file = here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
                      "ay.boxplot.pdf"), 
    width = 10, height = 6)

for (i in 1:length(ay.param.names)){
  a<- ay.to.plot %>% 
    filter(param == ay.param.names[i]) %>% 
    ggplot(aes(factor(ay_Scenario3), value))+
    geom_boxplot(aes(col = ay_Scenario3))+
    #facet_wrap(Year~., scales = "free", ncol = 4)+
    ggtitle(paste(ay.param.names[i]))+
    theme(legend.position = "none")
  print(a)
}
dev.off()

