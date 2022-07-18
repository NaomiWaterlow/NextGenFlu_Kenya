#Updated 5/7/2022
# Cases with URT going for outpatient visit----
#"URT OPC visit 0-5", 
sd <-(inputs[54,5]-inputs[54,4])/3.92#0.053571
mean<-inputs[54,3]#0.475
a <- estBetaParams(mean, sd^2)
set.seed(21)
visit.0.5 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(visit.0.5) ; sd(visit.0.5)

#"URT OPC visit 6-12", 
sd <-(inputs[55,5]-inputs[55,4])/3.92 #0.053571
mean<-inputs[55,3] #0.475
a <- estBetaParams(mean, sd^2)
set.seed(21)
visit.6.12 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(visit.6.12) ; sd(visit.6.12)

#"URT OPC visit 13-17", 
sd <-(inputs[56,5]-inputs[56,4])/3.92 #0.053571
mean<-inputs[56,3] #0.475
a <- estBetaParams(mean, sd^2)
set.seed(21)
visit.13.17 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(visit.13.17) ; sd(visit.13.17)

#"URT OPC visit 18-24", 
sd <-(inputs[57,5]-inputs[57,4])/3.92 #0.053571
mean<-inputs[57,3] #0.475
a <- estBetaParams(mean, sd^2)
set.seed(21)
visit.18.24 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(visit.18.24) ; sd(visit.18.24)

#"URT OPC visit 25-44", 
sd <-(inputs[58,5]-inputs[58,4])/3.92 #0.053571
mean<-inputs[58,3] #0.475
a <- estBetaParams(mean, sd^2)
set.seed(21)
visit.25.44 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(visit.25.44) ; sd(visit.25.44)

#"URT OPC visit 45-64, 
sd <-(inputs[59,5]-inputs[59,4])/3.92 #0.053571
mean<-inputs[59,3] #0.475
a <- estBetaParams(mean, sd^2)
set.seed(21)
visit.45.64 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(visit.45.64) ; sd(visit.45.64)

#"URT OPC visit 65+, 
sd <-(inputs[60,5]-inputs[60,4])/3.92 #0.053571
mean<-inputs[60,3] #0.475
a <- estBetaParams(mean, sd^2)
set.seed(21)
visit.65plus <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(visit.65plus) ; sd(visit.65plus)


opcvisit.samples <- cbind(samples.data, 
                          visit.0.5, visit.6.12, visit.13.17, 
                          visit.18.24, visit.25.44, visit.45.64, 
                          visit.65plus)


#split anyillnesscases to show cases by individual age
source(here::here("Econ/Scripts", "Kenya_demography.mod.R"))

age.weighting <- demography.pivot.gp2 %>% 
  filter(Year == year) %>% 
  select(Age.edit, Model_age_band, age.wt)

anyillnesscases.mod <- anyillnesscases %>% 
  select(-allages) %>% 
  mutate(sample = samples.data[,"sample"])

names(anyillnesscases.mod) <- c(unique(age.weighting$Model_age_band), "Sample")

OPCvisit <- anyillnesscases.mod %>% 
  pivot_longer(-Sample, names_to = "Model_age_band", values_to = "anyillnesscases") %>% 
  left_join(age.weighting) %>% 
  mutate(anyillnesscases.wtd = anyillnesscases*age.wt) %>% 
  select(Sample, Age.edit, Model_age_band, anyillnesscases.wtd) %>% 
  left_join(opcvisit.samples, by = c("Sample" = "sample")) %>% 
  mutate(OPCvisit = case_when(
    between(Age.edit, 0, 5) ~ anyillnesscases.wtd*visit.0.5,
    between(Age.edit, 6, 12) ~ anyillnesscases.wtd*visit.6.12,
    between(Age.edit, 13, 17) ~ anyillnesscases.wtd*visit.13.17,
    between(Age.edit, 18, 24) ~ anyillnesscases.wtd*visit.18.24,
    between(Age.edit, 25, 44) ~ anyillnesscases.wtd*visit.25.44,
    between(Age.edit, 45, 64) ~ anyillnesscases.wtd*visit.45.64,
    TRUE ~ anyillnesscases.wtd*visit.65plus
  )) %>% 
  select(Sample, Age.edit, Model_age_band, OPCvisit) %>% 
  group_by(Sample, Model_age_band) %>% 
  dplyr::summarise(OPCvisit.total = sum(OPCvisit)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "Model_age_band", values_from = "OPCvisit.total") %>% 
  select(Sample, Below_1, One_to_five, Six_to_14, Fifteen_to_19, Twenty_to_49, Fifty_and_above) %>% 
  mutate(allages = Below_1 + One_to_five + Six_to_14 +
           Fifteen_to_19 + Twenty_to_49 + Fifty_and_above) %>% 
  arrange(match(Sample, samples.data$sample)) %>%  
  select(-Sample)
  

# OPCvisit <-data.frame(matrix(0, ncol = 7, nrow = nsample))
# 
# OPCvisit[,1]<-anyillnesscases[,1]*visit1
# OPCvisit[,2]<-anyillnesscases[,2]*visit2
# OPCvisit[,3]<-anyillnesscases[,3]*visit3
# OPCvisit[,4]<-anyillnesscases[,4]*visit4
# OPCvisit[,5]<-anyillnesscases[,5]*visit5
# OPCvisit[,6]<-anyillnesscases[,6]*visit6
# OPCvisit[,7]<-rowSums(OPCvisit[,1:6], na.rm = FALSE, dims = 1)

# Cases with LRT symptoms that are hospitalised ---- 
#"Hosp <1", 
sd <-(inputs[23,5]-inputs[23,4])/3.92#0.0007142
mean<-inputs[23,3]#0.0102
a <- estBetaParams(mean, sd^2)
set.seed(21)
LRThosp1 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(LRThosp1) ; sd(LRThosp1)

#"Hosp 1-5", 
sd <-(inputs[23,5]-inputs[23,4])/3.92#0.0007142
mean<-inputs[23,3]#0.0102
a <- estBetaParams(mean, sd^2)
set.seed(21)
LRThosp2 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(LRThosp2) ; sd(LRThosp2)

#"Hosp 6-14", 
sd <-(((inputs[24,5]*7/9)+(inputs[25,5]*2/9))-((inputs[24,4]*7/9)+(inputs[25,4]*2/9)))/3.92#0.000119
mean<-((inputs[24,3]*7/9)+(inputs[25,3]*2/9))#0.0006778
a <- estBetaParams(mean, sd^2)
set.seed(21)
LRThosp3 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(LRThosp3) ; sd(LRThosp3)

#"Hosp 15-19", 
sd <-(((inputs[25,5]*3/5)+(inputs[26,5]*2/5))-((inputs[25,4]*3/5)+(inputs[26,4]*2/5)))/3.92#0.00068
mean<-((inputs[25,3]*3/5)+(inputs[26,3]*2/5))#0.00014795
a <- estBetaParams(mean, sd^2)
set.seed(21)
LRThosp4 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(LRThosp4) ; sd(LRThosp4)

#"Hosp 20-49", 
sd <-(((inputs[26,5]*5/30)+(inputs[27,5]*25/30)+(inputs[28,5]*5/30))
      -((inputs[26,4]*5/30)+(inputs[27,4]*25/30)+(inputs[28,4]*5/30)))/3.92#0.0001998
mean<-((inputs[26,3]*5/30)+(inputs[27,3]*25/30)+(inputs[28,3]*5/30))#0.00231667
a <- estBetaParams(mean, sd^2)
set.seed(21)
LRThosp5 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(LRThosp5) ; sd(LRThosp5)

#"Hosp >50"
sd <-(((inputs[28,5]*15/46)+(inputs[29,5]*31/46))-((inputs[28,4]*15/46)+(inputs[29,4]*31/46)))/3.92#0.0004347
mean<-((inputs[28,3]*15/46)+(inputs[29,3]*31/36))#0.003689
a <- estBetaParams(mean, sd^2)
set.seed(21)
LRThosp6 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(LRThosp6) ; sd(LRThosp6)

LRThosp <-data.frame(matrix(0, ncol = 7, nrow = nsample))

LRThosp[,1]<-anyillnesscases[,1]*LRThosp1
LRThosp[,2]<-anyillnesscases[,2]*LRThosp2
LRThosp[,3]<-anyillnesscases[,3]*LRThosp3
LRThosp[,4]<-anyillnesscases[,4]*LRThosp4
LRThosp[,5]<-anyillnesscases[,5]*LRThosp5
LRThosp[,6]<-anyillnesscases[,6]*LRThosp6
LRThosp[,7]<-rowSums(LRThosp[,1:6], na.rm = FALSE, dims = 1)

# Cases with LRT symptoms that are not hospitalised ---- 

LRTnonhosp <-data.frame(matrix(0, ncol = 7, nrow = nsample))

LRTnonhosp[,1]<-lrtcases[,1]-LRThosp[,1]
LRTnonhosp[,2]<-lrtcases[,2]-LRThosp[,2]
LRTnonhosp[,3]<-lrtcases[,3]-LRThosp[,3]
LRTnonhosp[,4]<-lrtcases[,4]-LRThosp[,4]
LRTnonhosp[,5]<-lrtcases[,5]-LRThosp[,5]
LRTnonhosp[,6]<-lrtcases[,6]-LRThosp[,6]
LRTnonhosp[,7]<-lrtcases[,7]-LRThosp[,7]

