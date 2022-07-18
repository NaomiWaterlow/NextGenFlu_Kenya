#Updated 5/7/2022
#function to estimate beta distribution parameters from mean and SE
estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}    

# Cases ----
casesagegrp <- casesagegrp[,2:7]
allages <- rowSums(casesagegrp[,1:6])
casesagegrp <- cbind(casesagegrp, allages)


# Clinical Illness----

sd <-(inputs[1,5]-inputs[1,4])/3.92 #0.04132653
mean<- inputs[1,3] #0.669
a1 <- estBetaParams(mean, sd^2)

set.seed(21)
#anyillness <-rtruncnorm(nsample, a1=0, b=1, mean = mean, sd = sd)
anyillness <-rbeta(nsample, shape1 = a1$alpha, shape2 = a1$beta) ; mean(anyillness) ; sd(anyillness)

anyillnesscases<- anyillness*casesagegrp

# Upper respiratory tract symptoms----

sd <-(inputs[3,5]-inputs[3,4])/3.92 #0.06454082
mean<- inputs[3,3] #0.588
a1 <- estBetaParams(mean, sd^2)

set.seed(21)
#urt <-rtruncnorm(nsample, a1=0, b=1, mean = mean, sd = sd)
urt <-rbeta(nsample, shape1 = a1$alpha, shape2 = a1$beta); mean(urt) ; sd(urt)

urtcases<- urt*casesagegrp

# Lower respiratory tract symptoms----

sd <-(inputs[4,5]-inputs[4,4])/3.92#0.0415816
mean<- inputs[4,3]#0.21
a1 <- estBetaParams(mean, sd^2)

set.seed(21)
lrt <-rbeta(nsample, shape1 = a1$alpha, shape2 = a1$beta) ; mean(lrt) ; sd(lrt)

lrtcases<- lrt*casesagegrp

# Healthcare utilization ----
source(here::here("Econ/Scripts", "02_b_outputs2healthcareutilization_mine.R"), print.eval=TRUE)

# Hosp deaths----

#"hosp deaths <1", 
sd <-(inputs[5,5]-inputs[5,4])/3.92#0.01571
mean<- inputs[5,3]#0.0274
a1 <- estBetaParams(mean, sd^2)

set.seed(21)
hospdeathsv1 <-rbeta(nsample, shape1 = a1$alpha, shape2 = a1$beta); mean(hospdeathsv1);sd(hospdeathsv1)

#"hosp deaths 1-5", 
sd <-(inputs[6,5]-inputs[6,4])/3.92#0.008214
mean<- inputs[6,3]#0.0091
a1 <- estBetaParams(mean, sd^2)
set.seed(21)
hospdeathsv2 <-rbeta(nsample, shape1 = a1$alpha, shape2 = a1$beta); mean(hospdeathsv2);sd(hospdeathsv2)

#"hosp deaths 6-14", 
sd <-(inputs[7,5]-inputs[7,4])/3.92#0.02301
mean<- inputs[7,3]#0.0108
a1 <- estBetaParams(mean, sd^2)
set.seed(21)
hospdeathsv3 <-rbeta(nsample, shape1 = a1$alpha, shape2 = a1$beta); mean(hospdeathsv3);sd(hospdeathsv3)

#"hosp deaths 15-19", 
sd <-(inputs[8,5]-inputs[8,4])/3.92#0.028469
mean<- inputs[8,3] #this has been edited in the .csv file to be a1 non-zero value (fixed at 0.00085, while true value = 0)
a1 <- estBetaParams(mean, sd^2)
set.seed(21)
hospdeathsv4 <-rbeta(nsample, shape1 = a1$alpha, shape2 = a1$beta); mean(hospdeathsv4);sd(hospdeathsv4)

#"hosp deaths 20-49", 
sd <-(inputs[9,5]-inputs[9,4])/3.92#0.020867
mean<- inputs[9,3]#0.0331
a1 <- estBetaParams(mean, sd^2)
set.seed(21)
hospdeathsv5 <-rbeta(nsample, shape1 = a1$alpha, shape2 = a1$beta); mean(hospdeathsv5);sd(hospdeathsv5)

#"hosp deaths >50",
sd <-(inputs[10,5]-inputs[10,4])/3.92#0.055382
mean<- inputs[10,3]#0.1818
a1 <- estBetaParams(mean, sd^2)
set.seed(21)
hospdeathsv6 <-rbeta(nsample, shape1 = a1$alpha, shape2 = a1$beta); mean(hospdeathsv6);sd(hospdeathsv6)

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
sd <-(inputs[12,5]-inputs[12,4])/3.92#0.017576
mean<- inputs[12,3]#0.2794
a1 <- estBetaParams(mean, sd^2)
set.seed(21)
hospdeaths1v1 <-rbeta(nsample, shape1 = a1$alpha, shape2 = a1$beta); mean(hospdeaths1v1); sd(hospdeaths1v1)

#1-5
sd <-(inputs[13,5]-inputs[13,4])/3.92#0.022397
mean<- inputs[13,3]#0.2899
a1 <- estBetaParams(mean, sd^2)
set.seed(21)
hospdeaths1v2 <-rbeta(nsample, shape1 = a1$alpha, shape2 = a1$beta); mean(hospdeaths1v2); sd(hospdeaths1v2)

#6-14
sd <-(inputs[14,5]-inputs[14,4])/3.92#0.044489
mean<- inputs[14,3]#0.4361
a1 <- estBetaParams(mean, sd^2)
set.seed(21)
hospdeaths1v3 <-rbeta(nsample, shape1 = a1$alpha, shape2 = a1$beta); mean(hospdeaths1v3); sd(hospdeaths1v3)

#15-19
sd <-(inputs[15,5]-inputs[15,4])/3.92#0.07767
mean<- inputs[15,3]#0.525
a1 <- estBetaParams(mean, sd^2)
set.seed(21)
hospdeaths1v4 <-rbeta(nsample, shape1 = a1$alpha, shape2 = a1$beta); mean(hospdeaths1v4); sd(hospdeaths1v4)

#20-49
sd <-(inputs[16,5]-inputs[16,4])/3.92#0.022933
mean<- inputs[16,3]#0.5067
a1 <- estBetaParams(mean, sd^2)
set.seed(21)
hospdeaths1v5 <-rbeta(nsample, shape1 = a1$alpha, shape2 = a1$beta); mean(hospdeaths1v5); sd(hospdeaths1v5)

#> 50
sd <-(inputs[17,5]-inputs[17,4])/3.92#0.015076
mean<- inputs[17,3]#0.2715
a1 <- estBetaParams(mean, sd^2)
set.seed(21)
hospdeaths1v6 <-rbeta(nsample, shape1 = a1$alpha, shape2 = a1$beta); mean(hospdeaths1v6); sd(hospdeaths1v6)

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