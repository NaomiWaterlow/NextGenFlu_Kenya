# These two functions have been obtained from Devleesschauwer et al., 2014
# doi: 10.1007/s00038-014-0552-z

# Explanations of the symbols

# Cxe^(-beta*x) – being the standard age-weighting formula, where
# C (0.1658) and beta (0.04) constants
# exp(-r*(x-a)) – being the time discounting formula, where
# r – discount rate (3%), x – age concerned and a – the age to which burden will be assigned.
# N – number of cases (YLD) or number of deaths (YLL)
# DW – disability weight
# A – Age at onset
# L – duration of illness
# K = 1 if age weighting applied, = 0 if otherwise

f <- function(x, K, C = .1658, beta = .04, r, a) {
  K * C * x * exp(-beta * x )*exp(-r * (x - a))+ (1 - K) * exp(-r * (x - a))}

burden <- function(N, DW, A, L, K, r, a) {
  N * DW * integrate(f, lower= A, upper = A + L, K = K, r = r, a = a)$value}


#Note - year has been defined in the file 02_Public-health-outcomes_mine.R
#year <- "2010"

if(year == "2010") {lifeexpectancy.ken <- lifeyears$X2010}
if(year == "2011") {lifeexpectancy.ken <- lifeyears$X2011}
if(year == "2012") {lifeexpectancy.ken <- lifeyears$X2012}
if(year == "2013") {lifeexpectancy.ken <- lifeyears$X2013}
if(year == "2014") {lifeexpectancy.ken <- lifeyears$X2014}
if(year == "2015") {lifeexpectancy.ken <- lifeyears$X2015}
if(year == "2016") {lifeexpectancy.ken <- lifeyears$X2016}
if(year == "2017") {lifeexpectancy.ken <- lifeyears$X2017}
if(year == "2018") {lifeexpectancy.ken <- lifeyears$X2018}
if(year == "2019") {lifeexpectancy.ken <- lifeyears$X2018}



#Version 1 - no age-weighting, no time-discounting ----
K = 0; r = 0    #   DALY[0;0]

source(here::here("Econ/Scripts", "old-02_d_outputs4DALYS_II_mine.R"), print.eval=TRUE) 

totalYLD00    <- YLD
totalYLL00    <- YLL
totalDALY00   <- DALY

#Version 2 - yes age-weighting, no time-discounting----
# K = 1; r = 0    #   DALY[1;0]
# 
# source(here::here("Econ/Scripts", "02_d_outputs4DALYS_II_mine.R"), print.eval=TRUE) 
# 
# totalYLD10    <- YLD
# totalYLL10    <- YLL
# totalDALY10   <- DALY

#Version 3 - no age-weighting, yes time-discounting----
K = 0; r = 0.03 #   DALY[0;0.03]

source(here::here("Econ/Scripts", "old-02_d_outputs4DALYS_II_mine.R"), print.eval=TRUE) 

totalYLD03    <- YLD
totalYLL03    <- YLL
totalDALY03 <- DALY


#Version 4 - yes age-weighting, yes time-discounting----
# K = 1; r = 0.03 #   DALY[1:0.03]
# 
# source(here::here("Econ/Scripts", "02_d_outputs4DALYS_II_mine.R"), print.eval=TRUE) 
# 
# totalYLD13    <- YLD
# totalYLL13    <- YLL
# totalDALY13 <- DALY

