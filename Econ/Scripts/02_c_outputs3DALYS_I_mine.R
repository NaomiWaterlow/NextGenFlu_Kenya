#Updated 5/7/2022
#Version 1 - no time-discounting ----
#disc_r = 0    #   DALY[0;0]

# DALYs among those with URT----
#sd <-(inputs[32,"Upper.95..CI.limit"]-inputs[32,"Lower.95..CI.limit"])/3.92#0.002551
upper <- inputs[32,"Upper.95..CI.limit"]; lower <- inputs[32,"Lower.95..CI.limit"]; mean<-inputs[32,"Mean"]#0.006
#a <- estBetaParams(mean, sd^2)
source(here::here("Econ/Scripts", "beta_fit_fn.R"))
set.seed(21)
milddaly <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(milddaly); sd(milddaly)

L = durn_illness/365 #median length of hospitalization = 4 days (Emukule et al., 2019, doi:10.1186/s12889-019-6773-6)

YLDmilddaly <- urtcases*milddaly*L

# DALYs among those with LRT not hospitalised----
upper <- inputs[33,"Upper.95..CI.limit"]; lower <- inputs[33,"Lower.95..CI.limit"]; mean<-inputs[33,"Mean"]#0.051
source(here::here("Econ/Scripts", "beta_fit_fn.R"))
set.seed(21)
severenonhospdaly <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(severenonhospdaly); sd(severenonhospdaly)

YLDseverenonhospdaly <- LRTnonhosp*severenonhospdaly*L

# DALYs among those with LRT hospitalised----
upper <- inputs[34,"Upper.95..CI.limit"]; lower <- inputs[34,"Lower.95..CI.limit"]; mean<-inputs[34,"Mean"]#0.133
source(here::here("Econ/Scripts", "beta_fit_fn.R"))
set.seed(21)
severehospdaly <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta); mean(severehospdaly); sd(severehospdaly)

YLDseverehospdaly <- LRThosp*severehospdaly*L

# total YLD 00----

YLD <- data.frame(matrix(0, ncol = 7, nrow = nsample))
YLD <- YLDmilddaly + YLDseverenonhospdaly + YLDseverehospdaly
#YLD <- cbind(YLD, rowSums(YLD))

totalYLD00    <- YLD


# #Version 2 - time-discounting at 3%----
# disc_r = 0.03
# 
# # DALYs among those with URT----
# sd <-(inputs[32,"Upper.95..CI.limit"]-inputs[32,"Lower.95..CI.limit"])/3.92
# mean<-inputs[32,"Mean"]
# a <- estBetaParams(mean, sd^2)
# set.seed(21)
# 
# milddaly <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta)
# 
# #milddaly<-inputs[32,"Mean"]
# 
# L = durn_illness/365
# 
# YLDmilddaly <- urtcases*milddaly*L*exp(-disc_r*L)
# 
# # DALYs among those with LRT not hospitalised----
# sd <-(inputs[33,"Upper.95..CI.limit"]-inputs[33,"Lower.95..CI.limit"])/3.92
# mean<-inputs[33,"Mean"]
# a <- estBetaParams(mean, sd^2)
# set.seed(21)
# severenonhospdaly <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta)
# 
# #severenonhospdaly<-inputs[33,"Mean"]
# 
# YLDseverenonhospdaly <- LRTnonhosp*severenonhospdaly*L*exp(-disc_r*L)
# 
# # DALYs among those with LRT hospitalised----
# sd <-(inputs[34,"Upper.95..CI.limit"]-inputs[34,"Lower.95..CI.limit"])/3.92
# mean<-inputs[34,"Mean"]
# a <- estBetaParams(mean, sd^2)
# set.seed(21)
# severehospdaly <-rbeta(nsample, shape1 = a$alpha, shape2 = a$beta)
# 
# #severehospdaly<-inputs[34,"Mean"]
# 
# YLDseverehospdaly <- LRThosp*severehospdaly*L*exp(-disc_r*L)
# 
# # total YLD 03----
# 
# YLD <- data.frame(matrix(0, ncol = 7, nrow = nsample))
# YLD <- YLDmilddaly + YLDseverenonhospdaly + YLDseverehospdaly
# #YLD <- cbind(YLD, rowSums(YLD))
# 
# 
# totalYLD03    <- YLD