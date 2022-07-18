library(tidyverse)
output.with.trunc.norm.sampling <- read_csv("Econ/Model_outcomes_output/Final/Naomi/Comparing/J.method_all_samples_all_years_scenarios.csv")
names(output.with.trunc.norm.sampling)[30] <- "total.deaths.all.ages.wrong"

output.with.trunc.norm.sampling <- output.with.trunc.norm.sampling %>% 
  mutate(total.deaths.all.ages = total.deaths..1 + total.deaths.1.5 + total.deaths.6.14 + 
           total.deaths.15.19 + total.deaths.20.49 + total.deaths..50)

names(output.with.trunc.norm.sampling)

output.with.new.sampling <- read_csv("Econ/Model_outcomes_output/Final/Naomi/Comparing/New.method_all_samples_all_years_scenarios.csv")

names(output.with.new.sampling)[120] <- "dalys.03.averted"

output.with.new.sampling.brecht.dalys <- read_csv("Econ/Model_outcomes_output/Final/Naomi/Comparing/New.Brecht.method_all_samples_all_years_scenarios.csv")
names(output.with.new.sampling.brecht.dalys)


#all years
base.year <- 2010
ay.outcomes.costs.old <- output.with.trunc.norm.sampling[,-1] %>%
  mutate(r = 0.03,
         n = Year - base.year,
         disc.total.costs = total.costs/((1+r)^n),
         disc.incremental.total.costs = incremental.total.costs/((1+r)^n),
         dalys.03.averted = dalys.03.averted/((1+r)^n)) %>% 
  select(-c(r, n)) %>% 
  pivot_longer(-c(Scenario, Sample, Year), names_to = "Measure", values_to = "value") %>% #View()
  group_by(Scenario, Sample, Measure) %>% #View()
  dplyr::summarise(total = sum(value)) %>% #View()
  ungroup() %>% 
  mutate(Sampling.method = "J.method")

ay.outcomes.costs.new <- output.with.new.sampling[,-1] %>%
  mutate(r = 0.03,
         n = Year - base.year,
         disc.total.costs = total.costs/((1+r)^n),
         disc.incremental.total.costs = incremental.total.costs/((1+r)^n),
         disc.total.costs.minus.vaccine.price = total.costs.minus.vaccine.price/((1+r)^n)) %>% 
  select(-c(r, n)) %>% 
  pivot_longer(-c(Scenario, Sample, Year), names_to = "Measure", values_to = "value") %>% #View()
  group_by(Scenario, Sample, Measure) %>% #View()
  dplyr::summarise(total = sum(value)) %>% #View()
  ungroup() %>% 
  mutate(Sampling.method = "New.method")

ay.outcomes.costs.new.brecht <- output.with.new.sampling.brecht.dalys[,-1] %>%
  mutate(r = 0.03,
         n = Year - base.year,
         disc.total.costs = total.costs/((1+r)^n),
         disc.incremental.total.costs = incremental.total.costs/((1+r)^n),
         disc.total.costs.minus.vaccine.price = total.costs.minus.vaccine.price/((1+r)^n)) %>% 
  select(-c(r, n)) %>% 
  pivot_longer(-c(Scenario, Sample, Year), names_to = "Measure", values_to = "value") %>% #View()
  group_by(Scenario, Sample, Measure) %>% #View()
  dplyr::summarise(total = sum(value)) %>% #View()
  ungroup() %>% 
  mutate(Sampling.method = "New.Brecht")

ay.df <- rbind(ay.outcomes.costs.new, ay.outcomes.costs.old, ay.outcomes.costs.new.brecht) #%>% 

#averted----
ay.df.avert.no.yll.dalys <- ay.df %>% filter(!Scenario == 1 & str_detect(Measure, "avert")) %>% 
  filter(!str_detect(Measure, "yll")) %>% 
  filter(!str_detect(Measure, "daly"))

ay.df.avert.with.yll.dalys <- ay.df %>% filter(!Scenario == 1 & str_detect(Measure, "avert")) %>% 
  filter(str_detect(Measure, paste0(c("yll", "daly"), collapse = "|")))



pdf(file = here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", "Comparing"),
                      "ay.boxplot.averted.pdf"), 
    width = 10, height = 7)
for (i in 1:length(unique(ay.df.avert.no.yll.dalys$Measure))){
  a<- ay.df.avert.no.yll.dalys %>% 
    filter(Measure == unique(ay.df.avert.no.yll.dalys$Measure)[i]) %>% 
    ggplot(aes(factor(Scenario), total))+
    geom_boxplot(aes(fill = Sampling.method), outlier.size = 0.4,
                 lwd = 0.5, fatten = 1)+
    #facet_wrap(Year~., scales = "free", ncol = 4)+
    ggtitle(paste(unique(ay.df.avert.no.yll.dalys$Measure)[i]))+
    theme(legend.position = "bottom")
  print(a)
}

for (i in 1:length(unique(ay.df.avert.with.yll.dalys$Measure))){
  
  a<- ay.df.avert.with.yll.dalys %>% 
    filter(Measure == unique(ay.df.avert.with.yll.dalys$Measure)[i]) %>% 
    ggplot(aes(factor(Scenario), total))+
    geom_boxplot(aes(fill = Sampling.method), outlier.size = 0.4,
                 lwd = 0.5, fatten = 1)+
    ylim(0, 1e6)+
    #facet_wrap(Year~., scales = "free", ncol = 4)+
    ggtitle(paste(unique(ay.df.avert.with.yll.dalys$Measure)[i]))+
    theme(legend.position = "bottom")
  print(a)
}
dev.off()



#costs ----
ay.costs <- ay.df %>% filter(str_detect(Measure, "cost"))

pdf(file = here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", "Comparing"),
                      "ay.boxplot.costs.pdf"), 
    width = 10, height = 7)
for (i in 1:length(unique(ay.costs$Measure))){
  a<- ay.costs %>% 
    filter(Measure == unique(ay.costs$Measure)[i]) %>% 
    ggplot(aes(factor(Scenario), total))+
    geom_boxplot(aes(fill = Sampling.method), outlier.size = 0.4,
                 lwd = 0.5, fatten = 1)+
    #facet_wrap(Year~., scales = "free", ncol = 4)+
    ggtitle(paste(unique(ay.costs$Measure)[i]))+
    theme(legend.position = "bottom")
  print(a)
}

# for (i in 1:length(unique(ay.df.avert.with.yll.dalys$Measure))){
#   
#   a<- ay.df.avert.with.yll.dalys %>% 
#     filter(Measure == unique(ay.df.avert.with.yll.dalys$Measure)[i]) %>% 
#     ggplot(aes(factor(Scenario), total))+
#     geom_boxplot(aes(fill = Sampling.method), outlier.size = 0.4,
#                  lwd = 0.5, fatten = 1)+
#     ylim(0, 1e6)+
#     #facet_wrap(Year~., scales = "free", ncol = 4)+
#     ggtitle(paste(unique(ay.df.avert.with.yll.dalys$Measure)[i]))+
#     theme(legend.position = "bottom")
#   print(a)
#}
dev.off()

#deaths only----
ay.old.deaths <- output.with.trunc.norm.sampling %>% 
  select(Scenario, Sample, Year, contains("death")) %>% 
  pivot_longer(-c(Scenario, Sample, Year), names_to = "Measure", values_to = "value") %>% #View()
  group_by(Scenario, Sample, Measure) %>% #View()
  dplyr::summarise(total = sum(value)) %>% #View()
  ungroup() %>% 
  mutate(Sampling.method = "J.method")

ay.new.deaths <- output.with.new.sampling %>% 
  select(Scenario, Sample, Year, contains("death")) %>% 
  pivot_longer(-c(Scenario, Sample, Year), names_to = "Measure", values_to = "value") %>% #View()
  filter(!str_detect(Measure, "_")) %>% 
  group_by(Scenario, Sample, Measure) %>% #View()
  dplyr::summarise(total = sum(value)) %>% #View()
  ungroup() %>% 
  mutate(Sampling.method = "New.method")

ay.new.deaths.brecht <- output.with.new.sampling.brecht.dalys %>% 
  select(Scenario, Sample, Year, contains("death")) %>% 
  pivot_longer(-c(Scenario, Sample, Year), names_to = "Measure", values_to = "value") %>% #View()
  filter(!str_detect(Measure, "_")) %>% 
  group_by(Scenario, Sample, Measure) %>% #View()
  dplyr::summarise(total = sum(value)) %>% #View()
  ungroup() %>% 
  mutate(Sampling.method = "New.Brecht")


ay.deaths.df <- rbind(ay.old.deaths, ay.new.deaths, ay.new.deaths.brecht)


pdf(file = here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", "Comparing"),
                      "ay.boxplot.deaths.pdf"), 
    width = 10, height = 7)
for (i in 1:length(unique(ay.deaths.df$Measure))){
  a<- ay.deaths.df %>% 
    filter(Measure == unique(ay.deaths.df$Measure)[i]) %>% 
    ggplot(aes(factor(Scenario), total))+
    geom_boxplot(aes(fill = Sampling.method), outlier.size = 0.4,
                 lwd = 0.5, fatten = 1)+
    #facet_wrap(Year~., scales = "free", ncol = 4)+
    ggtitle(paste(unique(ay.deaths.df$Measure)[i]))+
    theme(legend.position = "bottom")
  print(a)
}

# for (i in 1:length(unique(ay.df.avert.with.yll.dalys$Measure))){
#   
#   a<- ay.df.avert.with.yll.dalys %>% 
#     filter(Measure == unique(ay.df.avert.with.yll.dalys$Measure)[i]) %>% 
#     ggplot(aes(factor(Scenario), total))+
#     geom_boxplot(aes(fill = Sampling.method), outlier.size = 0.4,
#                  lwd = 0.5, fatten = 1)+
#     ylim(0, 1e6)+
#     #facet_wrap(Year~., scales = "free", ncol = 4)+
#     ggtitle(paste(unique(ay.df.avert.with.yll.dalys$Measure)[i]))+
#     theme(legend.position = "bottom")
#   print(a)
#}
dev.off()


#look at total deaths in J.method
ay.old.deaths %>% 
  select(-Sampling.method) %>% 
  pivot_wider(names_from = "Measure", values_from = "total") %>% #View()
  mutate(total.deaths2 = total.deaths..1 + total.deaths.1.5 + total.deaths.6.14+
           total.deaths.15.19 + total.deaths.20.49 + total.deaths..50, 
         diff = total.deaths.all.ages - total.deaths2) %>% 
  select(Sample, total.deaths2, total.deaths.all.ages, diff) %>% View()






to.plot.old <- output.with.trunc.norm.sampling[,-1] %>% 
  pivot_longer(-c(Scenario, Year, Sample), names_to = "param",
               values_to = "val") %>% 
  mutate(Sampling.method = "old")

to.plot.new <- output.with.new.sampling[,-1] %>% 
  pivot_longer(-c(Scenario, Year, Sample), names_to = "param",
               values_to = "val") %>% 
  mutate(Sampling.method = "new")


df <- rbind(to.plot.new, to.plot.old) #%>% 


df.avert <- df %>% filter(!Scenario == 1 & str_detect(param, "avert"))


pdf(file = here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", "Comparing"),
                      "boxplot.by.year.pdf"), 
    width = 10, height = 7)
for (i in 1:length(unique(df.avert$param))){
  a<- df.avert %>% 
    filter(param == unique(df.avert$param)[i]) %>% 
    ggplot(aes(factor(Scenario), val))+
    geom_boxplot(aes(fill = Sampling.method), outlier.size = 0.4,
                 lwd = 0.5, fatten = 1)+
    facet_wrap(Year~., scales = "free", ncol = 4)+
    ggtitle(paste(unique(df.avert$param)[i]))+
    theme(legend.position = "bottom", axis.text.x = element_blank())
  print(a)
}
dev.off()


#dalys----
ay.df.dalys <- ay.df %>%  
  filter(str_detect(Measure, paste0(c("yll", "yld", "daly", "YLL", "YLD", "DALY"), collapse = "|")))

pdf(file = here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", "Comparing"),
                      "boxplot.yld.yll.dalys.pdf"), 
    width = 10, height = 7)
for (i in 1:length(unique(ay.df.dalys$Measure))){
  a<- ay.df.dalys %>% 
    filter(Measure == unique(ay.df.dalys$Measure)[i]) %>% 
    ggplot(aes(factor(Scenario), total))+
    geom_boxplot(aes(fill = Sampling.method), outlier.size = 0.4,
                 lwd = 0.5, fatten = 1)+
    #facet_wrap(Year~., scales = "free", ncol = 4)+
    ggtitle(paste(unique(ay.df.dalys$Measure)[i]))+
    theme(legend.position = "bottom")
  print(a)
}
dev.off()  


#icers ----
# ay.df %>% 
#   pivot_wider(names_from = "Measure", values_from = "total") %>%
#   mutate(#icer.total.per.daly.00 = ay_incremental.total.costs/ay_dalys.00.averted,
#     #icer.total.per.disc.daly.03 = ay_incremental.total.costs/ay_disc_dalys.03.averted,
#     icer.disc.total.per.daly.00 = disc.incremental.total.costs/dalys.00.averted,
#     icer.disc.total.per.disc.daly.03 = disc.incremental.total.costs/disc_dalys.03.averted
#   ) %>% 
#   select(Scenario, Sample, Sampling.method, icer.disc.total.per.daly.00, icer.disc.total.per.disc.daly.03) %>% 
#   pivot_longer(-c(Scenario, Sample, Sampling.method), names_to = "Measure", values_to = "val") -> ay.icers
# 
# pdf(file = here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", "Comparing"),
#                       "boxplot.icers.pdf"), 
#     width = 8, height = 10)
# for (i in 1:length(unique(ay.icers$Measure))){
#   a<- ay.icers %>% 
#     filter(Measure == unique(ay.icers$Measure)[i]) %>% 
#     ggplot(aes(factor(Scenario), val))+
#     geom_boxplot(aes(fill = Sampling.method), outlier.size = 0.4,
#                  lwd = 0.5, fatten = 1)+
#     scale_y_continuous(breaks=c(0, 100, 623, 1913, 5738))+
#     geom_hline(yintercept = c(0, 100, 623, 1913, 5738), 
#                lty = c("solid", "dotted", "dashed", "dotdash", "twodash"))+
#     #facet_wrap(Year~., scales = "free", ncol = 4)+
#     ggtitle(paste(unique(ay.icers$Measure)[i]))+
#     theme(legend.position = "bottom")
#   print(a)
# }
# dev.off()  

#selecting relevant variables
ay.df %>% 
  pivot_wider(names_from = "Measure", values_from = "total") %>%
  mutate(#icer.total.per.daly.00 = ay_incremental.total.costs/ay_dalys.00.averted,
    #icer.total.per.disc.daly.03 = ay_incremental.total.costs/ay_disc_dalys.03.averted,
    icer.disc.total.per.daly.00 = disc.incremental.total.costs/dalys.00.averted,
    icer.disc.total.per.disc.daly.03 = disc.incremental.total.costs/dalys.03.averted#,
    #icer.disc.total.per.brecht.daly.03 = disc.incremental.total.costs/dalys.03.averted
  ) %>% 
  pivot_longer(-c(Scenario, Sample, Sampling.method), names_to = "Measure", values_to = "val") -> ay.all.measures

ay.all.measures %>% 
  filter(Measure %in% c("cases.averted", 
                        "total.deaths.all.ages", 
                        "total.YLD00.all.ages", "total.YLL00.all.ages", "total.DALY00.all.ages", 
                        "dalys.00.averted", "dalys.03.averted")) -> ay.select.deaths.dalys

params.to.plot <- c("cases.averted", 
                    "total.deaths.all.ages", 
                    "total.YLD00.all.ages", "total.YLL00.all.ages", "total.DALY00.all.ages", 
                    "dalys.00.averted", "dalys.03.averted")

  
pdf(file = here::here(paste0("Econ/Model_outcomes_output", "/Final/", "Naomi", "/", "Comparing"),
                      "boxplot.deaths.dalys.pdf"), 
    width = 8, height = 6)
for (i in 1:length(params.to.plot)){
  a<- ay.select.deaths.dalys %>% 
    filter(Measure == params.to.plot[i]) %>% 
    ggplot(aes(factor(Scenario), val))+
    geom_boxplot(aes(fill = Sampling.method), outlier.size = 0.4,
                 lwd = 0.5, fatten = 1)+
    #facet_wrap(Year~., scales = "free", ncol = 4)+
    ggtitle(paste(params.to.plot[i]))+
    theme(legend.position = "bottom")
  print(a)
}
dev.off() 

#costs
ay.all.measures %>% 
  filter(Measure %in% c("direct.medical.costs", "healthcare.related.costs", "indirect.costs",
                        "total.costs", "incremental.total.costs",
                        "icer.disc.total.per.daly.00", "icer.disc.total.per.disc.daly.03",
                        "icer.disc.total.per.brecht.daly.03")) -> ay.select.costs

params.to.plot <- c("direct.medical.costs", "healthcare.related.costs", "indirect.costs",
                    "total.costs", "incremental.total.costs",
                    "icer.disc.total.per.daly.00", "icer.disc.total.per.disc.daly.03")#,
                    #"icer.disc.total.per.brecht.daly.03")


pdf(file = here::here(paste0("Econ/Model_outcomes_output", "/Final/", "Naomi", "/", "Comparing"),
                      "boxplot.costs.pdf"), 
    width = 8, height = 6)
for (i in 1:length(params.to.plot)){
  a<- ay.select.costs %>% 
    filter(Measure == params.to.plot[i]) %>% 
    ggplot(aes(factor(Scenario), val))+
    geom_boxplot(aes(fill = Sampling.method), outlier.size = 0.4,
                 lwd = 0.5, fatten = 1)+
    #facet_wrap(Year~., scales = "free", ncol = 4)+
    ggtitle(paste(params.to.plot[i]))+
    theme(legend.position = "bottom")
  print(a)
}
dev.off() 







#comparing samples values ----
samples.with.trunc.norm.sampling <- read_csv("Econ/Model_outcomes_output/Final/Naomi/Comparing/Samples.J.method.csv")

samples.with.new.sampling <- read_csv("Econ/Model_outcomes_output/Final/Naomi/Comparing/Samples.new.method.csv")

samples.with.new.sampling.brecht.dalys <- read_csv("Econ/Model_outcomes_output/Final/Naomi/Comparing/Samples.new.Brecht.method.csv")

all.samples <- rbind(samples.with.new.sampling, samples.with.new.sampling.brecht.dalys, samples.with.trunc.norm.sampling)
all.samples <- all.samples[,-1]


all.samples %>% 
  ggplot(aes(val, fill = factor(sampling.method), alpha = 0.5))+
  geom_density()+
  #facet_wrap(Scenario~param, nrow = 3, scales = "free")+
  facet_wrap(.~param, scales = "free", nrow =5)+
  theme(legend.position = "bottom")


pdf(file = here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", "Comparing"),
                      "sampled.params.density.plot.pdf"), 
    width = 10, height = 6)

for (i in 1:length(unique(all.samples$param))){
  a<- all.samples %>% 
    filter(param == unique(all.samples$param)[i]) %>% 
    ggplot(aes(val, fill = factor(sampling.method), alpha = 0.5))+
    geom_density()+
    #facet_wrap(Year~., scales = "free", ncol = 4)+
    ggtitle(paste(unique(all.samples$param)[i]))+
    theme(legend.position = "bottom")
  print(a)
}
dev.off()

pdf(file = here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", "Comparing"),
                      "sampled.params.boxplot.pdf"), 
    width = 10, height = 6)

for (i in 1:length(unique(all.samples$param))){
  a<- all.samples %>% 
    filter(param == unique(all.samples$param)[i]) %>% 
    ggplot(aes(factor(param), val))+
    geom_boxplot(aes(col = sampling.method))+
    #facet_wrap(Year~., scales = "free", ncol = 4)+
    ggtitle(paste(unique(all.samples$param)[i]))+
    theme(legend.position = "bottom")
  print(a)
}
dev.off()

