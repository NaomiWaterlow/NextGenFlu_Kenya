
new.brecht.list <- as.data.frame(cbind(anyillness = anyillness, 
                          urt = urt,
                          lrt = lrt,
                          visit1 = visit1, 
                          visit2 = visit2, 
                          visit3 = visit3, 
                          visit4 = visit4,
                          visit5 = visit5,
                          visit6 = visit6,
                          LRThosp1 = LRThosp1,
                          LRThosp2 = LRThosp2,
                          LRThosp3 = LRThosp3,
                          LRThosp4 = LRThosp4,
                          LRThosp5 = LRThosp5,
                          LRThosp6 = LRThosp6,
                          hospdeathsv1 = hospdeathsv1,
                          hospdeathsv2 = hospdeathsv2,
                          hospdeathsv3 = hospdeathsv3,
                          hospdeathsv4 = hospdeathsv4,
                          hospdeathsv5 = hospdeathsv5,
                          hospdeathsv6 = hospdeathsv6,
                          hospdeaths1v1 = hospdeaths1v1,
                          hospdeaths1v2 = hospdeaths1v2,
                          hospdeaths1v3 = hospdeaths1v3,
                          hospdeaths1v4 = hospdeaths1v4,
                          hospdeaths1v5 = hospdeaths1v5,
                          hospdeaths1v6 = hospdeaths1v6,
                          milddaly = milddaly, severehospdaly = severehospdaly, severenonhospdaly = severenonhospdaly,
                          hospcosts1 = hospcosts1, hospcosts2 = hospcosts2,
                          OPCcosts = OPCcosts, Vaccineadmincosts1 = Vaccineadmincosts1, 
                          OPCtransport = OPCtransport, hosptransport = hosptransport, vaccinetransport = vaccinetransport,
                          OTCmedscost = OTCmedscost,
                          OPClostwages = OPClostwages, hosplostwages = hosplostwages,
                          OPCchildcosts = OPCchildcosts, hospchildcosts = hospchildcosts
                          )
)


pivot_longer(new.brecht.list, cols = everything(), names_to = "param", values_to = "val") %>% 
  mutate(sampling.method = "J.method") -> df

write.csv(df, file = here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date), "Samples.new.Brecht.method.csv"))

df <- read.csv("Samples.new.method.csv")
df <- df[,-1]
old.list <- read.csv("Samples.old.method.csv") #does not contain milddaly, severehospdaly and severenonhospdaly
old.list <- old.list[,-1]
  
rbind(df, old.list) %>% 
  ggplot(aes(val, fill = factor(sampling.method), alpha = 0.5))+
  geom_density()+
  #facet_wrap(Scenario~param, nrow = 3, scales = "free")+
  facet_wrap(.~param, scales = "free", nrow =5)+
  theme(legend.position = "bottom")

pdf(file = here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
                      "sampled.params.density.plot.pdf"), 
    width = 10, height = 6)

for (i in 1:length(unique(df$param))){
  a<- rbind(df, old.list) %>% 
    filter(param == unique(df$param)[i]) %>% 
    ggplot(aes(val, fill = factor(sampling.method), alpha = 0.5))+
    geom_density()+
    #facet_wrap(Year~., scales = "free", ncol = 4)+
    ggtitle(paste(unique(df$param)[i]))+
    theme(legend.position = "bottom")
  print(a)
}
dev.off()

pdf(file = here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
                      "sampled.params.boxplot.pdf"), 
    width = 10, height = 6)

for (i in 1:length(unique(df$param))){
  a<- rbind(df, old.list) %>% 
    filter(param == unique(df$param)[i]) %>% 
    ggplot(aes(factor(param), val))+
    geom_boxplot(aes(col = sampling.method))+
    #facet_wrap(Year~., scales = "free", ncol = 4)+
    ggtitle(paste(unique(df$param)[i]))+
    theme(legend.position = "bottom")
  print(a)
}
dev.off()

