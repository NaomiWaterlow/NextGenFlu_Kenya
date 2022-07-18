# DALYs among those with URT----
sd <-(inputs[32,"Upper.95..CI.limit"]-inputs[32,"Lower.95..CI.limit"])/3.92 #previously 1.96*2
mean<-inputs[32,"Mean"]
set.seed(21)
milddaly <-rtruncnorm(nsample, a=0, b=1, mean = mean, sd = sd)


#Note: L, the duration of illness, set to be 4 days based on values reported in Carrat et al., 2008
YLDmilddaly <- data.frame(matrix(0, ncol = 7, nrow = nsample))
for (i in 1:nsample){
  YLDmilddaly[i,1] <- burden(N = urtcases[i,1], DW = milddaly[i], A = 0.5,  L = durn_illness/365, K = 0, r = r, a = 0.5)
  YLDmilddaly[i,2] <- burden(N = urtcases[i,2], DW = milddaly[i], A = 2.5,  L = durn_illness/365, K = 0, r = r, a = 2.5)
  YLDmilddaly[i,3] <- burden(N = urtcases[i,3], DW = milddaly[i], A = 10,   L = durn_illness/365, K = K, r = r, a = 10)
  YLDmilddaly[i,4] <- burden(N = urtcases[i,4], DW = milddaly[i], A = 17,   L = durn_illness/365, K = K, r = r, a = 17)
  YLDmilddaly[i,5] <- burden(N = urtcases[i,5], DW = milddaly[i], A = 34.5, L = durn_illness/365, K = K, r = r, a = 34.5)
  YLDmilddaly[i,6] <- burden(N = urtcases[i,6], DW = milddaly[i], A = 72.5, L = durn_illness/365, K = 0, r = r, a = 72.5)
  YLDmilddaly[i,7] <- rowSums(YLDmilddaly[i,1:6], na.rm = FALSE, dims = 1)
}

# DALYs among those with LRT not hospitalised----
sd <-(inputs[33,"Upper.95..CI.limit"]-inputs[33,"Lower.95..CI.limit"])/3.92 #previously 1.96*2
mean<-inputs[33,"Mean"]
set.seed(21)
severenonhospdaly <-rtruncnorm(nsample, a=0, b=1, mean = mean, sd = sd)

YLDseverenonhospdaly <- data.frame(matrix(0, ncol = 7, nrow = nsample))
for (i in 1:nsample){
  YLDseverenonhospdaly[i,1] <- burden(N = LRTnonhosp[i,1], DW = severenonhospdaly[i], A = 0.5,  L = durn_illness/365, K = 0, r = r, a = 0.5)
  YLDseverenonhospdaly[i,2] <- burden(N = LRTnonhosp[i,2], DW = severenonhospdaly[i], A = 2.5,  L = durn_illness/365, K = 0, r = r, a = 2.5)
  YLDseverenonhospdaly[i,3] <- burden(N = LRTnonhosp[i,3], DW = severenonhospdaly[i], A = 10,   L = durn_illness/365, K = K, r = r, a = 10)
  YLDseverenonhospdaly[i,4] <- burden(N = LRTnonhosp[i,4], DW = severenonhospdaly[i], A = 17,   L = durn_illness/365, K = K, r = r, a = 17)
  YLDseverenonhospdaly[i,5] <- burden(N = LRTnonhosp[i,5], DW = severenonhospdaly[i], A = 34.5, L = durn_illness/365, K = K, r = r, a = 34.5)
  YLDseverenonhospdaly[i,6] <- burden(N = LRTnonhosp[i,6], DW = severenonhospdaly[i], A = 72.5, L = durn_illness/365, K = 0, r = r, a = 72.5)
  YLDseverenonhospdaly[i,7] <- rowSums(YLDseverenonhospdaly[i,1:6], na.rm = FALSE, dims = 1)
}

# DALYs among those with LRT hospitalised----
sd <-(inputs[34,"Upper.95..CI.limit"]-inputs[34,"Lower.95..CI.limit"])/3.92 #previously 1.96*2
mean<-inputs[34,"Mean"]
set.seed(21)
severehospdaly <-rtruncnorm(nsample, a=0, b=1, mean = mean, sd = sd)

YLDseverehospdaly <- data.frame(matrix(0, ncol = 7, nrow = nsample)) #ignore age-weighting i.e. set K = 0. 
for (i in 1:nsample){
  YLDseverehospdaly[i,1] <- burden(N = LRThosp[i,1], DW = severehospdaly[i], A = 0.5,  L = durn_illness/365, K = 0, r = r, a = 0.5)
  YLDseverehospdaly[i,2] <- burden(N = LRThosp[i,2], DW = severehospdaly[i], A = 2.5,  L = durn_illness/365, K = 0, r = r, a = 2.5)
  YLDseverehospdaly[i,3] <- burden(N = LRThosp[i,3], DW = severehospdaly[i], A = 10,   L = durn_illness/365, K = K, r = r, a = 10)
  YLDseverehospdaly[i,4] <- burden(N = LRThosp[i,4], DW = severehospdaly[i], A = 17,   L = durn_illness/365, K = K, r = r, a = 17)
  YLDseverehospdaly[i,5] <- burden(N = LRThosp[i,5], DW = severehospdaly[i], A = 34.5, L = durn_illness/365, K = K, r = r, a = 34.5)
  YLDseverehospdaly[i,6] <- burden(N = LRThosp[i,6], DW = severehospdaly[i], A = 72.5, L = durn_illness/365, K = 0, r = r, a = 72.5)
  YLDseverehospdaly[i,7] <- rowSums(YLDseverehospdaly[i,1:6], na.rm = FALSE, dims = 1)
}


# total YLD 00----

YLD <- data.frame(matrix(0, ncol = 7, nrow = nsample))
YLD <- YLDmilddaly + YLDseverenonhospdaly + YLDseverehospdaly

# YLL----
YLL <- data.frame(matrix(0, ncol = 7, nrow = nsample))
for (i in 1:nsample){
  YLL[i,1] <- burden(N = totaldeaths[i,1], DW = 1, A = 0.5,  L = lifeexpectancy.ken[1], K = 0, r = r, a = 0.5)
  YLL[i,2] <- burden(N = totaldeaths[i,2], DW = 1, A = 2.5,  L = lifeexpectancy.ken[2], K = 0, r = r, a = 2.5)
  YLL[i,3] <- burden(N = totaldeaths[i,3], DW = 1, A = 10,   L = lifeexpectancy.ken[3], K = K, r = r, a = 10)
  YLL[i,4] <- burden(N = totaldeaths[i,4], DW = 1, A = 17,   L = lifeexpectancy.ken[4], K = K, r = r, a = 17)
  YLL[i,5] <- burden(N = totaldeaths[i,5], DW = 1, A = 34.5, L = lifeexpectancy.ken[5], K = K, r = r, a = 34.5)
  YLL[i,6] <- burden(N = totaldeaths[i,6], DW = 1, A = 72.5, L = lifeexpectancy.ken[6], K = 0, r = r, a = 72.5)
  YLL[i,7] <- rowSums(YLL[i,1:6], na.rm = FALSE, dims = 1)
}

# total DALY 00----
DALY <- data.frame(matrix(0, ncol = 7, nrow = nsample))
DALY <- YLD + YLL