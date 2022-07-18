library(tidyverse)
df <- read.csv(here::here("Econ/Data", "lifetable.values.csv"))

ages <- data.frame(matrix(nrow = nrow(df), ncol = 101))
names(ages) <- paste0("surv", 0:100)

df <- cbind(df, ages)
df<- df %>% 
  mutate(surv = c(1, rep(NA, 100)))

agemax <- max(df$cohort_age)
for (i in 1:agemax){
  if(df$cohort_age[i] == 0)  df$surv[i] = 1
  if(df$cohort_age[i] > 0) df$surv[i] = df$surv[i-1]*(1-df$b_mort.rate[i-1])
  df[,i]  parse_number(colnames(df)[-(1:2)])[i]
}

df$surv
df %>% 
  mutate(surv = lag(first(surv)*(1-b_mort.rate)))# %>% 
  map(~ .x )
  #lag(surv)*(1-lag(b_mort.rate)))

  
  
  
  
  
  
cy <- df[,1]
mort <- df[,2]
surv=rep(0,agemax)
for (i in 1:agemax){
  if (cy[i] == 0) surv[i] = 1
  if (cy[i] > 0) surv[i] = surv[i-1]*(1-mort[i-1])
}

df2 <- df[-c(1:2, 104)] 

for (i in 1:agemax) {
  for (a in 1:agemax){
    if(parse_number(colnames(df2)[i]) < cy[a]) df2[a,i] = NA
    if(parse_number(colnames(df2)[i]) == cy[a]) df2[a,i] = 1
    if(parse_number(colnames(df2)[i]) > cy[a]) df2[a,i] = 
    
  }
}

  
 
  
  
a <- 0:100  
  
  
  
  
  
  
  

  canc_surv=rep(0,agemax)
  for(i in a:agemax){
    if(cy==0) surv[i]=1
    if(cy>0)  surv[i]=surv[i-1]*(1-mortality[i])
    canc_ly[a]=canc_ly[a]+(surv[i]-canc_surv[i]) /(1+disc_b)^cy
  }
    
n <- 10; nn <- 100
g <- factor(round(n * runif(n * nn)))
x <- rnorm(n * nn) + sqrt(as.numeric(g))
xg <- split(x, g)
boxplot(xg, col = "lavender", notch = TRUE, varwidth = TRUE)
sapply(xg, length)
sapply(xg, mean)