#Updated 5/7/2022
# Cases with URT going for outpatient visit----

#"URT OPC visit 0-5", 
upper <- inputs[54,5]; lower <- inputs[54,4]; mean<-inputs[54,3]#0.475
source(here::here("Econ/Scripts", "beta_fit_fn.R"))
set.seed(21)
visit.0.5 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(visit.0.5) ; sd(visit.0.5)

#"URT OPC visit 6-12", 
upper <- inputs[55,5]; lower <- inputs[55,4]; mean<-inputs[55,3] 
source(here::here("Econ/Scripts", "beta_fit_fn.R"))
set.seed(21)
visit.6.12 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(visit.6.12) ; sd(visit.6.12)

#"URT OPC visit 13-17", 
upper <- inputs[56,5]; lower <- inputs[56,4]; mean<-inputs[56,3] 
source(here::here("Econ/Scripts", "beta_fit_fn.R"))
set.seed(21)
visit.13.17 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(visit.13.17) ; sd(visit.13.17)

#"URT OPC visit 18-24", 
upper <- inputs[57,5]; lower <- inputs[57,4];mean<-inputs[57,3] 
source(here::here("Econ/Scripts", "beta_fit_fn.R"))
set.seed(21)
visit.18.24 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(visit.18.24) ; sd(visit.18.24)

#"URT OPC visit 25-44", 
upper <- inputs[58,5]; lower <- inputs[58,4]; mean<-inputs[58,3] 
source(here::here("Econ/Scripts", "beta_fit_fn.R"))
set.seed(21)
visit.25.44 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(visit.25.44) ; sd(visit.25.44)

#"URT OPC visit 45-64, 
upper <- inputs[59,5]; lower <- inputs[59,4]; mean<-inputs[59,3] 
source(here::here("Econ/Scripts", "beta_fit_fn.R"))
set.seed(21)
visit.45.64 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(visit.45.64) ; sd(visit.45.64)

#"URT OPC visit 65+, 
upper <- inputs[60,5]; lower <- inputs[60,4]; mean<-inputs[60,3] 
source(here::here("Econ/Scripts", "beta_fit_fn.R"))
set.seed(21)
visit.65plus <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(visit.65plus) ; sd(visit.65plus)


# #"URT OPC visit <1", 
visit1 <-visit.0.5
# 
# #"URT OPC visit 1-5", 
visit2 <-visit.0.5
# 
# #"URT OPC visit 6-14", 
visit3 <- (visit.6.12*7/9) + (visit.13.17*2/9)
# 
# #"URT OPC visit 15-19", 
visit4 <- (visit.13.17*3/5) + (visit.18.24*2/5)
# 
# #"URT OPC visit 20-49", 
visit5 <- (visit.18.24*5/30) + (visit.25.44*20/30) + (visit.45.64*5/30)
# 
# #"URT OPC visit >50"
visit6 <- (visit.45.64*15/36) + (visit.65plus*21/36)
 
OPCvisit <-data.frame(matrix(0, ncol = 7, nrow = nsample))

OPCvisit[,1]<-anyillnesscases[,1]*visit1
OPCvisit[,2]<-anyillnesscases[,2]*visit2
OPCvisit[,3]<-anyillnesscases[,3]*visit3
OPCvisit[,4]<-anyillnesscases[,4]*visit4
OPCvisit[,5]<-anyillnesscases[,5]*visit5
OPCvisit[,6]<-anyillnesscases[,6]*visit6
OPCvisit[,7]<-rowSums(OPCvisit[,1:6], na.rm = FALSE, dims = 1)



# opcvisit.samples <- cbind(samples.data, 
#                           visit.0.5, visit.6.12, visit.13.17, 
#                           visit.18.24, visit.25.44, visit.45.64, 
#                           visit.65plus)
# 
# 
# #split anyillnesscases to show cases by individual age
# source(here::here("Econ/Scripts", "Kenya_demography.mod.R"))
# 
# age.weighting <- demography.pivot.gp2 %>% 
#   filter(Year == year) %>% 
#   select(Age.edit, Model_age_band, age.wt)
# 
# anyillnesscases.mod <- anyillnesscases %>% 
#   select(-allages) %>% 
#   mutate(sample = samples.data[,"sample"])
# 
# names(anyillnesscases.mod) <- c(unique(age.weighting$Model_age_band), "Sample")
# 
# OPCvisit <- anyillnesscases.mod %>% 
#   pivot_longer(-Sample, names_to = "Model_age_band", values_to = "anyillnesscases") %>% 
#   left_join(age.weighting) %>% 
#   mutate(anyillnesscases.wtd = anyillnesscases*age.wt) %>% 
#   select(Sample, Age.edit, Model_age_band, anyillnesscases.wtd) %>% 
#   left_join(opcvisit.samples, by = c("Sample" = "sample")) %>% 
#   mutate(OPCvisit = case_when(
#     between(Age.edit, 0, 5) ~ anyillnesscases.wtd*visit.0.5,
#     between(Age.edit, 6, 12) ~ anyillnesscases.wtd*visit.6.12,
#     between(Age.edit, 13, 17) ~ anyillnesscases.wtd*visit.13.17,
#     between(Age.edit, 18, 24) ~ anyillnesscases.wtd*visit.18.24,
#     between(Age.edit, 25, 44) ~ anyillnesscases.wtd*visit.25.44,
#     between(Age.edit, 45, 64) ~ anyillnesscases.wtd*visit.45.64,
#     TRUE ~ anyillnesscases.wtd*visit.65plus
#   )) %>% 
#   select(Sample, Age.edit, Model_age_band, OPCvisit) %>% 
#   group_by(Sample, Model_age_band) %>% 
#   dplyr::summarise(OPCvisit.total = sum(OPCvisit)) %>% 
#   ungroup() %>% 
#   pivot_wider(names_from = "Model_age_band", values_from = "OPCvisit.total") %>% 
#   select(Sample, Below_1, One_to_five, Six_to_14, Fifteen_to_19, Twenty_to_49, Fifty_and_above) %>% 
#   mutate(allages = Below_1 + One_to_five + Six_to_14 +
#            Fifteen_to_19 + Twenty_to_49 + Fifty_and_above) %>% 
#   arrange(match(Sample, samples.data$sample)) %>%  
#   select(-Sample)


# #"URT OPC visit <1", 
# upper <- inputs[54,5]; lower <- inputs[54,4]; mean<-inputs[54,3]#0.475
# source(here::here("Econ/Scripts", "beta_fit_fn.R"))
# set.seed(21)
# visit1 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(visit1) ; sd(visit1)
# 
# #"URT OPC visit 1-5", 
# upper <- inputs[54,5]; lower <- inputs[54,4]; mean<-inputs[54,3] #0.475
# source(here::here("Econ/Scripts", "beta_fit_fn.R"))
# set.seed(21)
# visit2 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(visit2) ; sd(visit2)
# 
# #"URT OPC visit 6-14", 
# sd <-(((inputs[55,5]*7/9)+(inputs[56,5]*2/9))-((inputs[56,4]*7/9)+(inputs[56,4]*2/9)))/3.92 #0.0257936
# mean<-((inputs[55,3]*7/9)+(inputs[56,3]*2/9)) #0.111333
# a <- estBetaParams(mean, sd^2)
# set.seed(21)
# visit3 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(visit3) ; sd(visit3)
# 
# #"URT OPC visit 15-19", 
# sd <-(((inputs[56,5]*3/5)+(inputs[57,5]*2/5))-((inputs[57,4]*3/5)+(inputs[57,4]*2/5)))/3.92 #0.022959
# mean<-((inputs[56,3]*3/5)+(inputs[57,3]*2/5))#0.0668
# a <- estBetaParams(mean, sd^2)
# set.seed(21)
# visit4 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(visit4) ; sd(visit4)
# 
# #"URT OPC visit 20-49", 
# sd <-(((inputs[57,5]*5/30)+(inputs[58,5]*25/30)+(inputs[59,5]*5/30))
#       -((inputs[57,4]*5/30)+(inputs[58,4]*25/30)+(inputs[59,4]*5/30)))/3.92 #0.0148809
# mean<-((inputs[57,3]*5/30)+(inputs[58,3]*25/30)+(inputs[59,3]*5/30))#0.0386667
# a <- estBetaParams(mean, sd^2)
# set.seed(21)
# visit5 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(visit5) ; sd(visit5)
# 
# #"URT OPC visit >50"
# sd <-(((inputs[59,5]*15/46)+(inputs[60,5]*31/46))-((inputs[59,4]*15/46)+(inputs[60,4]*31/46)))/3.92 #0.01192
# mean<-((inputs[59,3]*15/46)+(inputs[60,3]*31/36))#0.039804
# a <- estBetaParams(mean, sd^2)
# set.seed(21)
# visit6 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(visit6) ; sd(visit6)
# 
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
#"Hosp 0-5", 
upper <- inputs[23,5]; lower <- inputs[23,4]; mean<-inputs[23,3]#0.0102
source(here::here("Econ/Scripts", "beta_fit_fn.R"))
set.seed(21)
LRThosp.0.5 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta)

#"Hosp 6-12", 
upper <- inputs[24,5]; lower <- inputs[24,4]; mean<-inputs[24,3]#0.0102
source(here::here("Econ/Scripts", "beta_fit_fn.R"))
set.seed(21)
LRThosp.6.12 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta)

#"Hosp 13-17", 
upper <- inputs[25,5]; lower <- inputs[25,4]; mean<-inputs[25,3]
source(here::here("Econ/Scripts", "beta_fit_fn.R"))
set.seed(21)
LRThosp.13.17 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta)

#"Hosp 18-24", 
upper <- inputs[26,5]; lower <- inputs[26,4]; mean<-inputs[26,3]
source(here::here("Econ/Scripts", "beta_fit_fn.R"))
set.seed(21)
LRThosp.18.24 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta)

#"Hosp 25-44", 
upper <- inputs[27,5]; lower <- inputs[27,4]; mean<-inputs[27,3]
source(here::here("Econ/Scripts", "beta_fit_fn.R"))
set.seed(21)
LRThosp.25.44 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta)

#"Hosp 45-64", 
upper <- inputs[28,5]; lower <- inputs[28,4]; mean<-inputs[28,3]
source(here::here("Econ/Scripts", "beta_fit_fn.R"))
set.seed(21)
LRThosp.45.64 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta)

#"Hosp 65+", 
upper <- inputs[29,5]; lower <- inputs[29,4]; mean<-inputs[29,3]
source(here::here("Econ/Scripts", "beta_fit_fn.R"))
set.seed(21)
LRThosp.65plus <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta)


# #"Hosp <1", 
LRThosp1 <-LRThosp.0.5
# 
# #"Hosp 1-5", 
LRThosp2 <-LRThosp.0.5
# 
# #"Hosp 6-14", 
LRThosp3 <- (LRThosp.6.12*7/9) + (LRThosp.13.17*2/9)
# 
# #"Hosp 15-19", 
LRThosp4 <- (LRThosp.13.17*3/5) + (LRThosp.18.24*2/5)
# 
# #"Hosp 20-49", 
LRThosp5 <- (LRThosp.18.24*5/30) + (LRThosp.25.44*20/30) + (LRThosp.45.64*5/30)
# 
# #"Hosp >50"
LRThosp6 <- (LRThosp.45.64*15/36) + (LRThosp.65plus*21/36)

LRThosp <-data.frame(matrix(0, ncol = 7, nrow = nsample))

LRThosp[,1]<-anyillnesscases[,1]*LRThosp1
LRThosp[,2]<-anyillnesscases[,2]*LRThosp2
LRThosp[,3]<-anyillnesscases[,3]*LRThosp3
LRThosp[,4]<-anyillnesscases[,4]*LRThosp4
LRThosp[,5]<-anyillnesscases[,5]*LRThosp5
LRThosp[,6]<-anyillnesscases[,6]*LRThosp6
LRThosp[,7]<-rowSums(LRThosp[,1:6], na.rm = FALSE, dims = 1)


# LRThosp1 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(LRThosp1) ; sd(LRThosp1)
# 
# #"Hosp 1-5", 
# upper <- inputs[23,5]; lower <- inputs[23,4]; mean<-inputs[23,3]#0.0102
# source(here::here("Econ/Scripts", "beta_fit_fn.R"))
# set.seed(21)
# LRThosp2 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(LRThosp2) ; sd(LRThosp2)
# 
# #"Hosp 6-14", 
# sd <-(((inputs[24,5]*7/9)+(inputs[25,5]*2/9))-((inputs[24,4]*7/9)+(inputs[25,4]*2/9)))/3.92#0.000119
# mean<-((inputs[24,3]*7/9)+(inputs[25,3]*2/9))#0.0006778
# a <- estBetaParams(mean, sd^2)
# set.seed(21)
# LRThosp3 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(LRThosp3) ; sd(LRThosp3)
# 
# #"Hosp 15-19", 
# sd <-(((inputs[25,5]*3/5)+(inputs[26,5]*2/5))-((inputs[25,4]*3/5)+(inputs[26,4]*2/5)))/3.92#0.00068
# mean<-((inputs[25,3]*3/5)+(inputs[26,3]*2/5))#0.00014795
# a <- estBetaParams(mean, sd^2)
# set.seed(21)
# LRThosp4 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(LRThosp4) ; sd(LRThosp4)
# 
# #"Hosp 20-49", 
# sd <-(((inputs[26,5]*5/30)+(inputs[27,5]*25/30)+(inputs[28,5]*5/30))
#       -((inputs[26,4]*5/30)+(inputs[27,4]*25/30)+(inputs[28,4]*5/30)))/3.92#0.0001998
# mean<-((inputs[26,3]*5/30)+(inputs[27,3]*25/30)+(inputs[28,3]*5/30))#0.00231667
# a <- estBetaParams(mean, sd^2)
# set.seed(21)
# LRThosp5 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(LRThosp5) ; sd(LRThosp5)
# 
# #"Hosp >50"
# sd <-(((inputs[28,5]*15/46)+(inputs[29,5]*31/46))-((inputs[28,4]*15/46)+(inputs[29,4]*31/46)))/3.92#0.0004347
# mean<-((inputs[28,3]*15/46)+(inputs[29,3]*31/36))#0.003689
# a <- estBetaParams(mean, sd^2)
# set.seed(21)
# LRThosp6 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(LRThosp6) ; sd(LRThosp6)

# Cases with LRT symptoms that are not hospitalised ---- 

LRTnonhosp <-data.frame(matrix(0, ncol = 7, nrow = nsample))

LRTnonhosp[,1]<-lrtcases[,1]-LRThosp[,1]
LRTnonhosp[,2]<-lrtcases[,2]-LRThosp[,2]
LRTnonhosp[,3]<-lrtcases[,3]-LRThosp[,3]
LRTnonhosp[,4]<-lrtcases[,4]-LRThosp[,4]
LRTnonhosp[,5]<-lrtcases[,5]-LRThosp[,5]
LRTnonhosp[,6]<-lrtcases[,6]-LRThosp[,6]
LRTnonhosp[,7]<-lrtcases[,7]-LRThosp[,7]

