#Updated 5/7/2022
#function to estimate beta distribution parameters from mean and SE
# estBetaParams <- function(mu, var) {
#   alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
#   beta <- alpha * (1 / mu - 1)
#   return(params = list(alpha = alpha, beta = beta))
# }

# Cases ----
casesagegrp <- casesagegrp[,2:7]
allages <- rowSums(casesagegrp[,1:6])
casesagegrp <- cbind(casesagegrp, allages)


# Clinical Illness----

upper <- inputs[1,5]; lower <- inputs[1,4]; mean<- inputs[1,3] #0.669
source(here::here("Econ/Scripts", "beta_fit_fn.R"))

set.seed(21)
#anyillness <-rtruncnorm(nsample, a=0, b=1, mean = mean, sd = sd)
anyillness <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta) ; mean(anyillness) ; sd(anyillness)

anyillnesscases<- anyillness*casesagegrp #this is by age group

# Upper respiratory tract symptoms----

upper <- inputs[3,5]; lower <- inputs[3,4]; mean<- inputs[3,3] #0.588
source(here::here("Econ/Scripts", "beta_fit_fn.R"))
set.seed(21)
#urt <-rtruncnorm(nsample, a=0, b=1, mean = mean, sd = sd)
urt <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(urt) ; sd(urt)

urtcases<- urt*casesagegrp

# Lower respiratory tract symptoms----

upper <- inputs[4,5]; lower <- inputs[4,4]; mean<- inputs[4,3]#0.21
source(here::here("Econ/Scripts", "beta_fit_fn.R"))
set.seed(21)
lrt <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta) ; mean(lrt) ; sd(lrt)

lrtcases<- lrt*casesagegrp

# Healthcare utilization ----
source(here::here("Econ/Scripts", "02_b_outputs2healthcareutilization_mine.R"), print.eval=TRUE)

# Hosp deaths----

#"hosp deaths <1", 
#sd <-(inputs[5,5]-inputs[5,4])/3.92#0.01571
lower <- inputs[5,4]; upper <- inputs[5,5]; mean<- inputs[5,3]#0.0274
#a <- estBetaParams(mean, sd^2)
source(here::here("Econ/Scripts", "beta_fit_fn.R"))

set.seed(21)
hospdeathsv1 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(hospdeathsv1);sd(hospdeathsv1)

#"hosp deaths 1-5", 
#sd <-(inputs[6,5]-inputs[6,4])/3.92#0.008214
lower <- inputs[6,4]; upper <- inputs[6,5]; mean<- inputs[6,3]#0.0091
#a <- estBetaParams(mean, sd^2)
source(here::here("Econ/Scripts", "beta_fit_fn.R"))

set.seed(21)
hospdeathsv2 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(hospdeathsv2);sd(hospdeathsv2)

#"hosp deaths 6-14", 
#sd <-(inputs[7,5]-inputs[7,4])/3.92#0.02301
lower <- inputs[7,4] ; upper <- inputs[7,5]; mean<- inputs[7,3]#0.0108
#a <- estBetaParams(mean, sd^2)
source(here::here("Econ/Scripts", "beta_fit_fn.R"))

set.seed(21)
hospdeathsv3 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(hospdeathsv3);sd(hospdeathsv3)

#"hosp deaths 15-19", 
#sd <-(inputs[8,5]-inputs[8,4])/3.92#0.028469
lower <- inputs[8,4]; upper <- inputs[8,5]; mean<- inputs[8,3] 
#Note: mean was initially changed to 0.00085, but now using original value = 0)
#a <- estBetaParams(mean, sd^2)
source(here::here("Econ/Scripts", "beta_fit_fn.R"))

set.seed(21)
hospdeathsv4 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(hospdeathsv4);sd(hospdeathsv4)

#"hosp deaths 20-49", 
#sd <-(inputs[9,5]-inputs[9,4])/3.92#0.020867
lower <- inputs[9,4]; upper <- inputs[9,5]; mean<- inputs[9,3]#0.0331
#a <- estBetaParams(mean, sd^2)
source(here::here("Econ/Scripts", "beta_fit_fn.R"))

set.seed(21)
hospdeathsv5 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(hospdeathsv5);sd(hospdeathsv5)

#"hosp deaths >50"
#sd <-(inputs[10,5]-inputs[10,4])/3.92#0.055382
lower <- inputs[10,4]; upper <- inputs[10,5]; mean<- inputs[10,3]#0.1818
#a <- estBetaParams(mean, sd^2)
source(here::here("Econ/Scripts", "beta_fit_fn.R"))

set.seed(21)
hospdeathsv6 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(hospdeathsv6);sd(hospdeathsv6)

# #hosp deaths all ages"
# sd <-(inputs[11,5]-inputs[11,4])/3.92
# mean<- inputs[11,3]
# set.seed(21)
# hospdeathsv7 <-rtruncnorm(nsample, a=0, b=1, mean = mean, sd = sd)

hospdeaths <-data.frame(matrix(0, ncol = 7, nrow = nsample))

hospdeaths[ ,1]<-LRThosp[,1]*hospdeathsv1
hospdeaths[ ,2]<-LRThosp[,2]*hospdeathsv2
hospdeaths[ ,3]<-LRThosp[,3]*hospdeathsv3
hospdeaths[ ,4]<-LRThosp[,4]*hospdeathsv4
hospdeaths[ ,5]<-LRThosp[,5]*hospdeathsv5
hospdeaths[ ,6]<-LRThosp[,6]*hospdeathsv6
hospdeaths[ ,7]<-rowSums(hospdeaths[,1:6], na.rm = FALSE, dims = 1)


# Total deaths ----
#calculated based on number of hospital deaths and proportion of resp. deaths that occur in hospital

#< 1
upper <- inputs[12,5]; lower <- inputs[12,4]; mean<- inputs[12,3]#0.2794
source(here::here("Econ/Scripts", "beta_fit_fn.R"))
set.seed(21)
hospdeaths1v1 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(hospdeaths1v1); sd(hospdeaths1v1)

#1-5
upper <- inputs[13,5]; lower <- inputs[13,4]; mean<- inputs[13,3]#0.2899
source(here::here("Econ/Scripts", "beta_fit_fn.R"))
set.seed(21)
hospdeaths1v2 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(hospdeaths1v2); sd(hospdeaths1v2)

#6-14
upper <- inputs[14,5]; lower <- inputs[14,4]; mean<- inputs[14,3]#0.4361
source(here::here("Econ/Scripts", "beta_fit_fn.R"))
set.seed(21)
hospdeaths1v3 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(hospdeaths1v3); sd(hospdeaths1v3)

#15-19
upper <- inputs[15,5]; lower <- inputs[15,4]; mean<- inputs[15,3]#0.525
source(here::here("Econ/Scripts", "beta_fit_fn.R"))
set.seed(21)
hospdeaths1v4 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(hospdeaths1v4); sd(hospdeaths1v4)

#20-49
upper <- inputs[16,5]; lower <- inputs[16,4]; mean<- inputs[16,3]#0.5067
source(here::here("Econ/Scripts", "beta_fit_fn.R"))
set.seed(21)
hospdeaths1v5 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(hospdeaths1v5); sd(hospdeaths1v5)

#> 50
upper <- inputs[17,5]; lower <- inputs[17,4]; mean<- inputs[17,3]#0.2715
source(here::here("Econ/Scripts", "beta_fit_fn.R"))
set.seed(21)
hospdeaths1v6 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(hospdeaths1v6); sd(hospdeaths1v6)

# sd <-(inputs[18,5]-inputs[18,4])/3.92
# mean<- inputs[18,3]
# set.seed(21)
# hospdeaths1v7 <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta)


totaldeaths <-data.frame(matrix(0, ncol = 7, nrow = nsample))

totaldeaths[ ,1]<-hospdeaths[,1]/hospdeaths1v1
totaldeaths[ ,2]<-hospdeaths[,2]/hospdeaths1v2
totaldeaths[ ,3]<-hospdeaths[,3]/hospdeaths1v3
totaldeaths[ ,4]<-hospdeaths[,4]/hospdeaths1v4
totaldeaths[ ,5]<-hospdeaths[,5]/hospdeaths1v5
totaldeaths[ ,6]<-hospdeaths[,6]/hospdeaths1v6
totaldeaths[ ,7]<-rowSums(totaldeaths[,1:6], na.rm = FALSE, dims = 1)


# DALYS and costs----

source(here::here("Econ/Scripts", "02_c_outputs3DALYS_I_mine.R"), print.eval=TRUE) 

#source(here::here("Mine/Scripts", "02_d_outputs4DALYS_II_mine.R"), print.eval=TRUE) 

source(here::here("Econ/Scripts", "02_e_outputs5costs_mine.R"), print.eval=TRUE)