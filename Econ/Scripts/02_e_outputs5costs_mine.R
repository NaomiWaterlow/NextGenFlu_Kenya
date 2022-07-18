#Updated 5/7/2022
#function to estimate gamma distribution parameters from mean and SE
estGammaParams <- function(mu, var) {
  shape <- (mu^2)/var
  scale <- var/mu
  return(params = list(shape = shape, scale=scale))
} 



#1. Direct medical costs----
#1.1 Hospitalization cost following infection (direct medical cost)----

sd <- inputs[45,6]#59.39
mean<- inputs[45,"Mean"]#59.19
a <- estGammaParams(mean, sd^2)
set.seed(21)
#hospcosts1 <-rtruncnorm(nsample, a=0, b=Inf, mean = mean, sd = sd)
hospcosts1 <-rgamma(nsample, shape = a$shape, scale = a$scale); mean(hospcosts1) ; sd(hospcosts1)

hospcostsfacility <- hospcosts1*LRThosp

proppostdischarge <-rep(inputs[22,"Mean"], times=nsample)#0.105 - fixed value
sd <- inputs[46,6]#6.19
mean<- inputs[46,"Mean"]#3.28
a <- estGammaParams(mean, sd^2)

set.seed(21)
hospcosts2 <-rgamma(nsample, shape = a$shape, scale = a$scale); mean(hospcosts2) ; sd(hospcosts2)

hospcostspostdischarge <- hospcosts2*LRThosp*proppostdischarge

hospcostscases<- hospcostsfacility + hospcostspostdischarge

# 1.2 Outpatient visit cost following infection (direct medical cost)----

sd <- inputs[43,6]#1.3
mean<- inputs[43,"Mean"]#4.34
a <- estGammaParams(mean, sd^2)

set.seed(21)
OPCcosts <-rgamma(nsample, shape = a$shape, scale = a$scale); mean(OPCcosts) ; sd(OPCcosts)

OPCcostscases<- OPCcosts*OPCvisit

# 1.3 Vaccination costs (purchase and administration) (direct medical cost)----

sd <- inputs[48,6]#0.72
mean<- inputs[48,"Mean"]#1
a <- estGammaParams(mean, sd^2)
set.seed(21)
Vaccineadmincosts1 <-rgamma(nsample, shape = a$shape, scale = a$scale); mean(Vaccineadmincosts1) ; sd(Vaccineadmincosts1)

Vaccineadmincosts2 <-rep(inputs[49,"Mean"], times=nsample)#0.43 fixed value

Vaccinepurchasecost1 <-rep(vacc_price, times=nsample)# default is 3, fixed value

#Note - vaccine waste of 15% was incorporated here - this has been removed
vaccinecosts.admin<-(Vaccineadmincosts1+Vaccineadmincosts2)*(vaccinedoses) ##(1/(1-0.15)))
vaccinecosts.price<-Vaccinepurchasecost1*(vaccinedoses) ##(1/(1-0.15)))
vaccinecosts.admin <- as.data.frame(vaccinecosts.admin)
vaccinecosts.price <- as.data.frame(vaccinecosts.price)

#vaccinecosts1 <- vaccinecosts1[7, ]


#. 2. Health care related costs----
# 2.1 OPC Transportation cost for seeking care due to illness (direct non-medical cost)----

sd <- inputs[50,6]#0.87
mean<- inputs[50,"Mean"]#0.4
a <- estGammaParams(mean, sd^2)
set.seed(21)
OPCtransport <-rgamma(nsample, shape = a$shape, scale = a$scale); mean(OPCtransport) ; sd(OPCtransport)

OPCtransportcosts<- OPCvisit*OPCtransport

# 2.2 Hospitalisation Transportation cost for seeking care due to illness (direct non-medical cost)----

sd <- inputs[51,6]#8.32
mean<- inputs[51,"Mean"]#5.03
a <- estGammaParams(mean, sd^2)

set.seed(21)
hosptransport <-rgamma(nsample, shape = a$shape, scale = a$scale); mean(hosptransport) ; sd(hosptransport)

hosptransportcosts<- LRThosp*hosptransport

# 2.3 Transportation costs for seeking care to obtain vaccine (direct  non-medical cost)----

sd <- inputs[53,6] #0.435 - assumed value - half the costs of transportation costs to visit OPC
mean<- inputs[53,"Mean"]#02. - assumed value as above
a <- estGammaParams(mean, sd^2)

set.seed(21)
vaccinetransport <-rgamma(nsample, shape = a$shape, scale = a$scale); mean(vaccinetransport) ; sd(vaccinetransport)

vaccinetransportcosts<- vaccinedoses*vaccinetransport

vaccinetransportcosts<- as.data.frame(vaccinetransportcosts)

#vaccinetransportcosts1 <- vaccinetransportcosts1[7, ]

#2.5 Over-the-counter medications cost following infection - paid by individual (direct medical cost)----

propOTCmeds <-rep(inputs[21,"Mean"], times=nsample) #0.718

sd <- inputs[52,6] #3.9
mean<- inputs[52,"Mean"] #1.39
a <- estGammaParams(mean, sd^2)
set.seed(21)
OTCmedscost <-rgamma(nsample, shape = a$shape, scale = a$scale); mean(OTCmedscost) ; sd(OTCmedscost)

OTCmedscosts<- OTCmedscost*OPCvisit*propOTCmeds

# 3. Non-health care costs----
#3.1 Lost wages due to seeking outpatient care due to illness (indirect cost)----

propOPCmissedwork <- rep(inputs[36,"Mean"], times=nsample) #0.518 - fixed value
sd <- inputs[41,6]#27.17
mean<- inputs[41,"Mean"]#12.84
a <- estGammaParams(mean, sd^2)

set.seed(21)
OPClostwages <-rgamma(nsample, shape = a$shape, scale = a$scale); mean(OPClostwages) ; sd(OPClostwages)

OPClostwagescosts<- OPCvisit*OPClostwages*propOPCmissedwork

#3.2 Lost wages due to seeking inpatient care due to illness (indirect cost)----


prophospmissedwork <- rep(inputs[35,"Mean"], times=nsample) #0.848 fixed value
sd <- inputs[42,6]#41.54
mean<- inputs[42,"Mean"]#42.02
a <- estGammaParams(mean, sd^2)
set.seed(21)
hosplostwages <-rgamma(nsample, shape = a$shape, scale = a$scale); mean(hosplostwages) ; sd(hosplostwages)

hosplostwagescosts<- LRThosp*hosplostwages*prophospmissedwork

#3.5 Child care costs due to seeking outpatient care due to illness (indirect cost)----

propOPCchildcare <- rep(inputs[38,"Mean"], times=nsample) #0.018 fixed value
sd <- inputs[39,6]#0.57
mean<- inputs[39,"Mean"]#0.07
a <- estGammaParams(mean, sd^2)

set.seed(21)
OPCchildcosts <-rgamma(nsample, shape = a$shape, scale = a$scale); mean(OPCchildcosts) ; sd(OPCchildcosts)

OPCchildcarecosts<- OPCvisit*OPCchildcosts*propOPCchildcare

#3.6 Childcare costs due to seeking inpatient care due to illness (indirect cost)----

prophospchildcare <- rep(inputs[37,"Mean"], times=nsample) #0.029
sd <- inputs[40,6]#0.75
mean<- inputs[40,"Mean"]#0.11
a <- estGammaParams(mean, sd^2)
set.seed(21)
hospchildcosts <-rgamma(nsample, shape = a$shape, scale = a$scale); mean(hospchildcosts) ; sd(hospchildcosts)

hospchildcarecosts<- LRThosp*hospchildcosts*prophospchildcare
