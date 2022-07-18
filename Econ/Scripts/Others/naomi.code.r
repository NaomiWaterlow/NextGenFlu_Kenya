
library(tidyverse)
whatdf <- read.csv(here::here("Econ/Data", "lifetable.values.csv"))

flu_mort <- print$wtd.flu_mort
flu_mort.full <- c(flu_mort, rep(tail(flu_mort,1), 15))



whatdf$flu_mort <- flu_mort.full



agemax <- 100 +1# maximum age (adding a plus one as can't use vecotrs with a 0)
#disc_b <- 0.035 # discount rate
# mortality rate - lifted these from the excel sheet
# mortality_base <- c(0.0050,0.0004,0.0002,0.0002,0.0001,0.0001,0.0001,0.0001,0.0001,
#                     0.0001,0.0001,0.0001,0.0001,0.0002,0.0002,0.0002,0.0003,0.0004,
#                     0.0004,0.0004,0.0005,0.0004,0.0005,0.0005,0.0005,0.0005,0.0005,
#                     0.0006,0.0006,0.0006,0.0007,0.0007,0.0007,0.0008,0.0008,0.0009,
#                     0.0010,0.0010,0.0010,0.0011,0.0012,0.0013,0.0014,0.0016,0.0018,
#                     0.0019,0.0020,0.0023,0.0026,0.0027,0.0031,0.0034,0.0036,0.0041,
#                     0.0043,0.0047,0.0054,0.0056,0.0060,0.0065,0.0073,0.0083,0.0091,
#                     0.0099,0.0111,0.0120,0.0131,0.0146,0.0162,0.0170,0.0193,0.0215,
#                     0.0241,0.0264,0.0300,0.0335,0.0372,0.0419,0.0471,0.0531,0.0587,
#                     0.0654,0.0741,0.0826,0.0921,0.1015,0.1144,0.1090,0.1404,0.1570,
#                     0.2276,0.2276,0.2276,0.2276,0.2276,0.2276,0.2276,0.2276,0.2276,
#                     1.0000,0.0000)

#storage_table <- matrix(NA, ncol=101, nrow=101)

mortality <-df$b_mort.rate

surv=rep(0,agemax)
df <- matrix(NA, ncol = agemax, nrow = agemax)

for (a in 1:agemax){
for(i in a:agemax){
  cy <- i-a
  disc_b <- 0.035
  if(cy==0) {surv[i]=1
  df[i,a] = surv[i]}
  
  if(cy>0)  {surv[i]=df[i-1,a]*(1-mortality[i-1])
  df[i,a] = (surv[i]/(1+disc_b))}
  #canc_ly[a]=canc_ly[a]+(surv[i]-canc_surv[i]) /(1+disc_b)^cy
}
}


colSums(df, na.rm = T)






for(a in 1:agemax){ # for each base yr age (i.e. along columns of excel)
  # need a base survival calculation for each of the columns, so reset here.
  surv=rep(1,agemax)
  
  for(i in a:agemax){ # down the cohort age column
    # these two generate the base survival, without discounting
    cy <- i-a
    if(cy == 0){surv[i]=1 
    storage_table[i,a]=1
    }
    if(cy > 0){ surv[i]=storage_table[i-1, a]*(1-mortality_base[i-1])
    # then need to discount
    storage_table[i,a]=(surv[i])/(1+disc_b)}
  }
}#

storage_table <- round(storage_table, 4)

colSums(storage_table, na.rm = T)



table <- as.data.frame(storage_table) 
table <- table %>% 
  mutate(cohort_age = 0:100) %>% 
  select(cohort_age, V1:V101)
names(table)

table <- table %>% 
  mutate(across(where(is.numeric), round, 4))

table %>% 
  select(cohort_age, V1, V2) %>% 
  # pivot_longer(-cohort_age, names_to = "Age", values_to = "val") %>% 
  # group_by(cohort_age, Age) %>% 
  # dplyr::summarise(first = sum(val))
  mutate(across(matches("V"), ~first(rev(cumsum(ifelse(is.na(.x), 0, .x)))), .names = "le_{col}"))
  #replace_na(list(. = 0))
  mutate(c1 = rev(cumsum(V1)),
         c2 = rev(cumsum(ifelse(is.na(V2), 0, V2))))
  mutate(c = rev(cumsum(V1)),
         le = c/V1)


table %>% 
  mutate(across(matches("V"),
                .fns = function(x) (colSums(.)/x) ,
                .names = "le_{col}"))
