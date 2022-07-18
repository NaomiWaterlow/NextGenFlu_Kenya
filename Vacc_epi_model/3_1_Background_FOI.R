# BACKGROUND FOI SCRIPT

# specify which time periods to include in the background rate calculations
# (when there isnt an epidemic / pandemic)
# start date, end date, virus type, immunity to use
dates_for_foi <- data.frame( rbind(c("2010-W09", "2010-W51", "FluB"),
                                   c("2012-W11", "2013-W18", "FluB"),
                                   c("2013-W50", "2015-W48", "FluB"),
                                   c("2016-W21", "2017-W35", "FluB"),
                                   c("2018-W25", "2018-W51", "FluB"),
                                   
                                   c("2010-W09", "2010-W12", "H3N2"),
                                   c("2010-W52", "2011-W50", "H3N2"),
                                   c("2012-W20", "2016-W12", "H3N2"),
                                   c("2016-W49", "2018-W51", "H3N2"),
                                   
                                   c("2012-W01", "2013-W50", "pand09"),
                                   c("2014-W47", "2018-W02", "pand09"),
                                   c("2018-W42", "2018-W51", "pand09")
))


########## 1. data ages ratio ########
# sort into age groups
hosp_dat[age.years < 1, age_grp := 1]
hosp_dat[age.years >=1 & age.years <6, age_grp := 2]
hosp_dat[age.years >= 6 & age.years <15, age_grp := 3]
hosp_dat[age.years >= 15 & age.years <20, age_grp := 4]
hosp_dat[age.years >= 20 & age.years <50, age_grp := 5]
hosp_dat[age.years >= 50, age_grp := 6]

hosp_sum_age <- hosp_dat[, sum(pand09, na.rm = T), by = c("date.onset.illness","age_grp")]
hosp_sum_age$H3N2 <- hosp_dat[, sum(H3N2, na.rm = T), by = c("date.onset.illness","age_grp")]$V1
hosp_sum_age$Flu_B <- hosp_dat[, sum(FluB, na.rm = T), by = c("date.onset.illness","age_grp")]$V1
colnames(hosp_sum_age) <- c("Date", "age_group", "pand09", "H3N2", "FluB")
hosp_sum_age$Date <- as.Date(hosp_sum_age$Date, format = "%d/%m/%Y")

# by week
hosp_sum_age[,week_all :=  as.character(date2ISOweek(Date))]
hosp_sum_age[,week :=  substring(week_all,1, nchar(week_all)-2)]
# hosp_sum_age[as.numeric(week) < 10,week := paste0("0", week)]
# hosp_sum_age[,year := year(Date)]
# hosp_sum_age[,year_week := paste0(year, "_", week)]

hosp_sum_age_week <- hosp_sum_age[, sum(pand09, na.rm = T), by = c("week", "age_group")]
hosp_sum_age_week$H3N2 <- hosp_sum_age[, sum(H3N2, na.rm = T), by = c("week", "age_group")]$V1
hosp_sum_age_week$Flu_B <- hosp_sum_age[, sum(FluB, na.rm = T), by = c("week", "age_group")]$V1

colnames(hosp_sum_age_week)[3] <- "H1N1"
# remove the data from before our time period starts
start_time <- which(hosp_sum_age_week$week == "2010-W09")
hosp_sum_age_week <- hosp_sum_age_week[-(1:start_time[1]-1),]
# H1N1 remove before 2012-01, as pandemic year. Needs to be gone not just 0 else is part of the background rate
end_pandemic <- which(hosp_sum_age_week$week == "2012-W01")
hosp_sum_age_week[1:(end_pandemic[1]-1), "pand09"] <- NA

########## 2. lambda by section ########

lambda_estimates <- data.frame(matrix(nrow = 3, ncol = 3))
hosp_sum_week_temp <- hosp_sum_age_week
hosp_sum_week_temp$keep <- "yes"
for(subtype_num in 1:3){
  subtype <-  c("H1N1", "H3N2", "Flu_B")[subtype_num]
  relevant_dates <- which( dates_for_foi$X3 == subtype)
  
  for(i in relevant_dates){
    start <- as.numeric(which(hosp_sum_week_temp$week == dates_for_foi[i,1]))[1]
    end <- tail(as.numeric(which(hosp_sum_week_temp$week == dates_for_foi[i,2])),1)
    hosp_sum_week_temp[(start:end),"keep"] <- "no"
  }
  
  for( age_gp in 1:6){
    lambda <- fitdistr(unlist(hosp_sum_week_temp[keep == "yes" & age_group == age_gp, ..subtype]), densfun="poisson")
    lambda_estimates[subtype_num+((age_gp-1)*3),] <- c(lambda$estimate, subtype, age_gp)
  }
  }


# lambda_estimates <- data.frame(matrix(nrow = nrow(dates_for_foi), ncol = 6))
# #Then for each season fit the data with the poisson.
# for( i in 1:nrow(dates_for_foi)){
#   
#   start <- as.numeric(which(hosp_sum_week$Date == dates_for_foi[i,1]))
#   end <- as.numeric(which(hosp_sum_week$Date == dates_for_foi[i,2]))
#   type <- dates_for_foi[i,3]
#   
#   lambda <- fitdistr(unlist(hosp_sum_week[start:end, ..type]), densfun="poisson")
#   
#   lambda_estimates[i,] <- c(lambda$estimate,end-start ,i, start, end, type)
#   
# }
# 
# colnames(lambda_estimates) <- c("lambda", "duration", "offseason", "start", "end", "type")
# lambda_estimates$start <- as.numeric(lambda_estimates$start)
# lambda_estimates$end <- as.numeric(lambda_estimates$end)
# lambda_estimates$lambda <- as.numeric(lambda_estimates$lambda)
# lambda_estimates <- data.table(lambda_estimates)


########### 3. get the posteriors #######
multipliers_H3 <- colMeans(ascertainment_H3[1:3])
multipliers_H1 <- colMeans(ascertainment_H1[1:3])
multipliers_B <- colMeans(ascertainment_B[1:3])

