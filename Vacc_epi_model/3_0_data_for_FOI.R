# load in the data
hosp_dat <- data.table(read.csv(here::here("Data","12916_2020_1687_MOESM1_ESM.csv")))
# reformat the data
hosp_dat[test.flu.type.A == "Positive", FluA := 1]
hosp_dat[test.flu.type.A == "Negative", FluA := 0]
hosp_dat[test.flu.type.A == "", FluA := NA]
hosp_dat[test.flu.type.B == "Positive", FluB := 1]
hosp_dat[test.flu.type.B == "Negative", FluB := 0]
hosp_dat[test.flu.type.B == "", FluB := NA]
hosp_dat[has.flu.AH1N1.pandemic.2009 == "Yes", pand09 := 1]
hosp_dat[has.flu.AH1N1.pandemic.2009 == "No", pand09 := 0]
hosp_dat[has.flu.AH1N1.pandemic.2009 == "", pand09 := NA]
hosp_dat[has.flu.AH3N2 == "Yes", H3N2 := 1]
hosp_dat[has.flu.AH3N2 == "No", H3N2 := 0]
hosp_dat[has.flu.AH3N2 == "", H3N2 := NA]
# summarise the data
hosp_sum <- hosp_dat[, sum(FluA, na.rm = T), by = date.onset.illness]
hosp_sum$pand09 <- hosp_dat[, sum(pand09, na.rm = T), by = date.onset.illness]$V1
hosp_sum$H3N2 <- hosp_dat[, sum(H3N2, na.rm = T), by = date.onset.illness]$V1
hosp_sum$Flu_B <- hosp_dat[, sum(FluB, na.rm = T), by = date.onset.illness]$V1
colnames(hosp_sum) <- c("Date", "FluA", "pand09", "H3N2", "FluB")
hosp_sum$Date <- as.Date(hosp_sum$Date, format = "%d/%m/%Y")
# by week
hosp_sum[,week_all :=  date2ISOweek(Date)]
hosp_sum[,week :=  substring(week_all,1, nchar(week_all)-2)]
#hosp_sum[as.numeric(week) < 10,week := paste0("0", week)]
#hosp_sum[,year := year(Date)]
#hosp_sum[,year_week := paste0(year, "_", week)]

hosp_sum_week <- hosp_sum[, sum(FluA, na.rm = T), by = week]
hosp_sum_week$pand09 <- hosp_sum[, sum(pand09, na.rm = T), by = week]$V1
hosp_sum_week$H3N2 <- hosp_sum[, sum(H3N2, na.rm = T), by = week]$V1
hosp_sum_week$Flu_B <- hosp_sum[, sum(FluB, na.rm = T), by = week]$V1

#format to plot
colnames(hosp_sum_week) <- c("Date", "FluA", "pand09", "H3N2", "FluB")
hosp_sum_week <- hosp_sum_week[order(Date)]
hosp_sum_week$time_week <- 1:nrow(hosp_sum_week)

# exclude the data from before our modelling starts (2010-03-01)
start_time <- which(hosp_sum_week$Date == "2010-W09")
hosp_sum_week <- hosp_sum_week[-(1:start_time-1),]
# for pandemic year delete the cases that aren't use (as in J paper)
end_pandemic <- which(hosp_sum_week$Date == "2012-W01")
hosp_sum_week[1:end_pandemic, "pand09"] <- 0
hosp_sum_week_m <- melt.data.table(hosp_sum_week, id.vars = c("Date", "time_week"))
#plot


