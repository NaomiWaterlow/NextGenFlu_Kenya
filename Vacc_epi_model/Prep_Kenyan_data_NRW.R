# _____________________________________________________________________________________________________
# -----------------------------------------------------------------------------------------------------
#                                   CALLING IN KENYAN DATA
# -----------------------------------------------------------------------------------------------------
# _____________________________________________________________________________________________________

#       Activities in this script
# In the following section, we:
#       1.  import the Kenyan social contact survey (polymod)
#       2.  import the Kenyan SARI (severe acute respiratory illness) data and call it ILI to 
#           match the terminology of the fluEvidenceSynthesis package
#       3.  import a vector of the Kenyan population 
#       4.  make our base vaccination scenario of no vaccination across all 6 age groups

#       Calling in the packages

#filename <- paste0(startdate, " to ", enddate, " ", flutype, " ")  

#
#       1. Importing the Kenyan contact survey data
#
# Here we import the Kenya contact survey data and reformat it.
#       
# This is the key of the Kenyan contact survey data: 
#     v1:age of subject 
#     v2:data taken in the weekend (1) or during the week (0)
#     v3:<1 yr,  v4:1-5 yrs,  v5:6-14 yrs,  v6:15-19 yrs, v7:20-49 yrs, v8:>=50 yrs
#      
# At the end, the Kenyan contact survey data will be in the same format as polymod_uk

polymod.ken   <- read.csv("Data/polymod_ken.csv")
polymod.ken   <- polymod.ken[ , c(1, 4, 7, 26)]    # Only picking the relevant variables
polymod.ken1  <- polymod.ken[ ,c(1, 2, 4, 3)]
polymod.ken2  <- polymod.ken[ ,c(1, 2, 4, 3)]
polymod.ken2  <- polymod.ken[ ,c(1, 2, 3, 4)]
polymod.ken2  <- polymod.ken[ ,c(1, 2, 4)]
polymod.ken2  <- unique(polymod.ken2)
polymod.ken3  <- tally(group_by(polymod.ken1, csid, age_class_cont))
polymod.ken3  <- merge(polymod.ken3,polymod.ken2,by.x = "csid", by.y = "csid", all.x=T)
polymod.ken3  <- reshape(polymod.ken3, 
                         timevar = "age_class_cont",
                         idvar = c("csid", "age_years_part", "day"),
                         direction = "wide")

polymod.ken3$day[polymod.ken3$day == 0] <- 2
polymod.ken3$day[polymod.ken3$day == 1] <- 0
polymod.ken3$day[polymod.ken3$day == 2] <- 1
polymod.ken3                            <- polymod.ken3[order(polymod.ken3$age_years_part), ]
polymod.ken3$age_years_part             <- round(polymod.ken3$age_years_part, 0)
polymod.ken3[is.na(polymod.ken3)]       <- 0

polymod.ken4    <- plyr::rename(polymod.ken3, c(age_years_part= "v1", day="v2", n.Primary = "v5", 
                                                n.Secondary="v6", n.Adult="v7", n.Older="v8", "n.Pre-School"="v4", n.Infant="v3"))
polymod.ken5    <- polymod.ken4[ ,c("v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8")]
polymod.ken6    <- polymod.ken5
polymod.ken7    <- data.frame(matrix(0, ncol = 8, nrow = 568))

polymod.ken7$X1 <- as.integer(polymod.ken6$v1)
polymod.ken7$X2 <- as.integer(polymod.ken6$v2)
polymod.ken7$X3 <- as.integer(polymod.ken6$v3)
polymod.ken7$X4 <- as.integer(polymod.ken6$v4)
polymod.ken7$X5 <- as.integer(polymod.ken6$v5)
polymod.ken7$X6 <- as.integer(polymod.ken6$v6)
polymod.ken7$X7 <- as.integer(polymod.ken6$v7)
polymod.ken7$X8 <- as.integer(polymod.ken6$v8)

polymod.ken7    <- plyr::rename(polymod.ken7, c(X1= "V1", X2="V2", X3 = "V3", 
                                                X4="V4", X5="V5", X6="V6", 
                                                X7="V7", X8="V8"))

polymod.ken     <- polymod.ken7

rm(polymod.ken1, polymod.ken2, polymod.ken3, polymod.ken4, polymod.ken5, polymod.ken6, polymod.ken7)


#       2. Importing Kenyan SARI data, reformatting it and calling it ILI like the UK data

# sari.ken      <- read.csv("Data/infsurveillance_ken.csv")
# sari.ken      <- sari.ken[sari.ken$sentinel.site != "Kenyatta NH", ]
# sari.ken      <- sari.ken[c("date.onset.illness", "age")]
# 
# sari.ken$date.onset.illness   <- as.Date(sari.ken$date.onset.illness, format = "%d/%m/%Y")
# sari.ken$age  <-as.numeric(as.character(sari.ken$age))
# #str(sari.ken)
# 
# sari.ken      <- sari.ken[sari.ken$date.onset.illness >= as.Date(startdate) 
#                           & sari.ken$date.onset.illness <= as.Date(enddate+6), ]
# 
# sari.ken$age.group[sari.ken$age>=50]                    <- "v6"
# sari.ken$age.group[sari.ken$age>=20 & sari.ken$age<50]  <- "v5"
# sari.ken$age.group[sari.ken$age>=15 & sari.ken$age<20]  <- "v4"
# sari.ken$age.group[sari.ken$age>=6 & sari.ken$age<15]   <- "v3"
# sari.ken$age.group[sari.ken$age>=1 & sari.ken$age<6]    <- "v2"
# sari.ken$age.group[sari.ken$age<1]                      <- "v1"
# 
# sari.ken$weeks  <- 1+as.numeric(sari.ken$date.onset.illness - as.Date(startdate)) %/% 7
# sari.ken        <- sari.ken[ ,c("weeks", "age.group")]
# sari.ken        <- tally(group_by(sari.ken, weeks, age.group))
# sari.ken        <- cast(sari.ken, weeks~age.group, value ="n")
# sari.ken[is.na(sari.ken)] <- 0
# sari.ken        <- sari.ken[c(1:(nrow(sari.ken)-1)), c(1:7)]
# 
# 
# #       4. Getting the total monitored population, tested and positive samples
# # We upload the dataframe with the weekly number of individuals per age group 
# # and get the number of individuals in each age group on each surveillance week 
# # Note that v1:<1 yr,  v2:1-5 yrs,  v3:6-14 yrs,  v4:15-19 yrs, v5:20-49 yrs,  v6:>=50 yrs
# # Remember it is called ILI to fit into the UK model naming system but its actually SARI
# 
# ili.ken                     <- plyr::rename(sari.ken, c("v1"="ili.V1", "v2"="ili.V2", "v3"="ili.V3",
#                                                         "v4"="ili.V4", "v5"="ili.V5", "v6"="ili.V6"))


# surveillancedays <- seq(as.Date(startdate), as.Date(enddate+6), by = "1 week") 
# 
# # To get the daily monitored population for each age group
# dailymonpop <- read.csv("Data/dailymonpop.csv")
# dailymonpop$day <- as.Date(dailymonpop$day, format = "%Y-%m-%d")
# dailymonpop <- dailymonpop[ ,-1]
# 
# # Make a function that picks the total monitored population during the weeks of surveillance
# daily_totmonpop <- function(surveillancedays)
# {
#   total.monitored <- data.frame(matrix(NA, ncol = 7, nrow=length(surveillancedays)))
#   total.monitored <- plyr::rename(total.monitored, c(X1="day", X2="v1", X3="v2", X4="v3", X5="v4", X6="v5", X7="v6"))
#   
#   for(ii in 1:length(surveillancedays)) 
#   {
#     p <- dailymonpop[ which(dailymonpop$day == as.Date(surveillancedays[ii])), ]
#     total.monitored[ii, ] <- p
#   }
#   return(total.monitored[ , -1])
# }
# 
# set.seed(21)
# dailytotmon <- daily_totmonpop(surveillancedays)
# dailytotmon[ ,1] <- sample(dailytotmon[ ,1], nrow(dailytotmon))
# dailytotmon[ ,2] <- sample(dailytotmon[ ,2], nrow(dailytotmon))
# dailytotmon[ ,3] <- sample(dailytotmon[ ,3], nrow(dailytotmon))
# dailytotmon[ ,4] <- sample(dailytotmon[ ,4], nrow(dailytotmon))
# dailytotmon[ ,5] <- sample(dailytotmon[ ,5], nrow(dailytotmon))
# dailytotmon[ ,6] <- sample(dailytotmon[ ,6], nrow(dailytotmon))


# ili.ken$total.monitored <- dailytotmon
# 
# ili.ken        <- ili.ken[ ,c(-1)]
# ili.ken$ili.V1 <- as.integer(ili.ken$ili.V1)
# ili.ken$ili.V2 <- as.integer(ili.ken$ili.V2)
# ili.ken$ili.V3 <- as.integer(ili.ken$ili.V3)
# ili.ken$ili.V4 <- as.integer(ili.ken$ili.V4)
# ili.ken$ili.V5 <- as.integer(ili.ken$ili.V5)
# ili.ken$ili.V6 <- as.integer(ili.ken$ili.V6)
# 
# ili.test                            <- list()
# ili.test$ili                        <- as.matrix(ili.ken[ ,1:6])
# ili.test$total.monitored            <- as.matrix(dailytotmon)
# colnames(ili.test$ili)              <- c("V1", "V2", "V3", "V4", "V5", "V6")
# colnames(ili.test$total.monitored)  <- c("V1", "V2", "V3", "V4", "V5", "V6")
# ili.ken                             <- ili.test
# 
# rm(ili.test)


# To get the influenza positive cases (NB: I have selected a particular strain)

# infpos <- read.csv("Data/infsurveillance_ken.csv")
# infpos <- infpos[infpos$sentinel.site != "Kenyatta NH", ]
# 
# # Pick the flu subtype
# if (flutype == "B") {
#   infpos <- infpos[infpos$test.flu.type.B == "Positive", ]
# }
# 
# if (flutype == "AH1N1") {
#   infpos <- infpos[infpos$has.flu.AH1N1.pandemic.2009 == "Yes", ]
# }
# 
# if (flutype == "AH3N2") {
#   infpos <- infpos[infpos$has.flu.AH3N2 == "Yes", ]
# }
# 
# infpos <- infpos[c("date.onset.illness", "age")]
# infpos$date.onset.illness <- as.Date(infpos$date.onset.illness, format = "%d/%m/%Y")
# infpos$age                <-as.numeric(as.character(infpos$age))
# #str(infpos)
# infpos <- infpos[infpos$date.onset.illness >= as.Date(startdate) & infpos$date.onset.illness <= as.Date(enddate+6), ]
# infpos$age.group[infpos$age>=50]                  <- "V6"
# infpos$age.group[infpos$age>=20 & infpos$age<50]  <- "V5"
# infpos$age.group[infpos$age>=15 & infpos$age<20]  <- "V4"
# infpos$age.group[infpos$age>=6 & infpos$age<15]   <- "V3"
# infpos$age.group[infpos$age>=1 & infpos$age<6]    <- "V2"
# infpos$age.group[infpos$age<1]                    <- "V1"
# 
# infpos$weeks  <- 1 + as.numeric(infpos$date.onset.illness  - as.Date(startdate)) %/% 7
# infpos                <- infpos[ ,c("weeks", "age.group")]
# infpos                <- tally(group_by(infpos, weeks, age.group))
# infpos                <- cast(infpos, weeks~age.group, value ="n")
# infpos[is.na(infpos)] <- 0
# infpos <- as.data.frame(infpos)
# 
# infpos5 <- data.frame(matrix(0, ncol = 7, nrow=nrow(infpos)))
# colnames(infpos5)       <- c("weeks", "V1", "V2", "V3", "V4", "V5", "V6")
# infpos5$weeks <- infpos$weeks
# 
# dropVars <- intersect(names(infpos), names(infpos5))[-1]
# 
# infpos <- as.data.table(infpos)
# infpos1 <- as.data.table(infpos5)
# 
# infpos <- infpos[infpos1[, mget(names(infpos5)[-which(names(infpos1) %in% dropVars)])], on="weeks"]
# 
# infpos <- as.data.frame(infpos)
# 
# infpos$V1 <- as.integer(infpos$V1)
# infpos$V2 <- as.integer(infpos$V2)
# infpos$V3 <- as.integer(infpos$V3)
# infpos$V4 <- as.integer(infpos$V4)
# infpos$V5 <- as.integer(infpos$V5)
# infpos$V6 <- as.integer(infpos$V6)
# 
# infpos <- infpos[ , c("weeks", "V1", "V2", "V3", "V4", "V5", "V6")]
# 
# infpos <- plyr::rename(infpos, c("V1"="positive.V1", "V2"="posivite.V2", "V3"="positive.V3",
#                                  "V4"="positive.V4", "V5"="positive.V5", "V6"="positive.V6"))        


# Get the tested samples

# tested.ken      <- read.csv("Data/infsurveillance_ken.csv")
# tested.ken      <- tested.ken[tested.ken$sentinel.site != "Kenyatta NH", ] #Remove KNH data
# tested.ken$date.onset.illness <- as.Date(tested.ken $date.onset.illness, format 
#                                          = "%d/%m/%Y")
# tested.ken$age  <-as.numeric(as.character(tested.ken$age))
# 
# 
# # Changing the data to reflect that everyone with a negative or positive flu test result 
# # was actually tested and only picking that dataset
# 
# tested.ken$tested.for.influenza[tested.ken$test.any.influenza == "Positive" 
#                                 | tested.ken$test.any.influenza == "Negative"
#                                 | tested.ken$has.flu.AH1N1.pandemic.2009 == "Yes"
#                                 | tested.ken$has.flu.AH1N1.pandemic.2009 == "No"
#                                 | tested.ken$has.flu.AH3N2 == "Yes"
#                                 | tested.ken$has.flu.AH3N2 == "No"
#                                 | tested.ken$test.flu.type.B == "Negative"
#                                 | tested.ken$test.flu.type.B == "Positive"
#                                 | tested.ken$test.flu.type.A == "Negative"
#                                 | tested.ken$test.flu.type.A == "Positive"] <- "Yes"
# tested.ken <- tested.ken[tested.ken$tested.for.influenza == "Yes", ]
# 
# # Defining the time period
# 
# tested.ken      <- tested.ken[tested.ken$date.onset.illness >= as.Date(startdate) & tested.ken$date.onset.illness <= as.Date(enddate+6), ]
# 
# # Assigning each entry to a week number in order to arrange data by week 
# 
# tested.ken$weeks  <-1 + as.numeric(tested.ken$date.onset.illness - as.Date(startdate)) %/% 7
# 
# tested.ken  <- tested.ken[ , c("weeks", "age")]
# 
# tested.ken$age.group[tested.ken$age>=50]                  <- "V6"
# tested.ken$age.group[tested.ken$age>=20 & tested.ken$age<50]  <- "V5"
# tested.ken$age.group[tested.ken$age>=15 & tested.ken$age<20]  <- "V4"
# tested.ken$age.group[tested.ken$age>=6 & tested.ken$age<15]   <- "V3"
# tested.ken$age.group[tested.ken$age>=1 & tested.ken$age<6]    <- "V2"
# tested.ken$age.group[tested.ken$age<1]                    <- "V1"
# 
# tested.ken  <- tally(group_by(tested.ken, weeks, age.group))
# tested.ken  <- dcast(tested.ken, weeks~age.group, value ="n")
# tested.ken[is.na(tested.ken)] <- 0
# tested.ken        <- tested.ken[c(1:(nrow(tested.ken)-1)), ]
# 
# tested.ken5 <- data.frame(matrix(0, ncol = 7, nrow=nrow(tested.ken)))
# colnames(tested.ken5)       <- c("weeks", "V1", "V2", "V3", "V4", "V5", "V6")
# tested.ken5$weeks <- tested.ken$weeks
# 
# dropVars <- intersect(names(tested.ken), names(tested.ken5))[-1]
# 
# tested.ken <- as.data.table(tested.ken)
# tested.ken1 <- as.data.table(tested.ken5)
# 
# tested.ken <- tested.ken[tested.ken1[, mget(names(tested.ken5)[-which(names(tested.ken1) %in% dropVars)])], on="weeks"]
# 
# tested.ken <- as.data.frame(tested.ken)
# 
# 
# 
# tested.ken$V1 <- as.integer(tested.ken$V1)
# tested.ken$V2 <- as.integer(tested.ken$V2)
# tested.ken$V3 <- as.integer(tested.ken$V3)
# tested.ken$V4 <- as.integer(tested.ken$V4)
# tested.ken$V5 <- as.integer(tested.ken$V5)
# tested.ken$V6 <- as.integer(tested.ken$V6)
# 
# 
# samples <- plyr::rename(tested.ken, c("V1"="total.samples.V1", "V2"="total.samples.V2", "V3"="total.samples.V3",
#                                       "V4"="total.samples.V4", "V5"="total.samples.V5", "V6"="total.samples.V6"))
# 
# 
# confirmed.samples <- merge(samples, infpos,  by.x = "weeks", by.y = "weeks", all.x=T)
# confirmed.samples <- confirmed.samples[ , c("positive.V1", "posivite.V2", "positive.V3", "positive.V4","positive.V5", "positive.V6"
#                                             ,"total.samples.V1", "total.samples.V2", "total.samples.V3","total.samples.V4", "total.samples.V5" ,"total.samples.V6")]
# 
# confirmed.samples[is.na(confirmed.samples)] <- 0
# confirmed.samples.ken                       <- confirmed.samples
# rm(sari.ken, infpos, samples)
# 
# 
# #confirmed.samples.ken[, 1:length(confirmed.samples.ken)]    <- sapply(confirmed.samples.ken[, 1:length(confirmed.samples.ken)], as.integer)
# confirmed.samples.test                          <- list()
# confirmed.samples.test$positive                 <- as.data.frame(confirmed.samples.ken[ ,1:6])
# confirmed.samples.test$total.samples            <- as.data.frame(confirmed.samples.ken[ ,7:12])
# colnames(confirmed.samples.test$positive)       <- c("V1", "V2", "V3", "V4", "V5", "v6")
# colnames(confirmed.samples.test$total.samples)  <- c("V1", "V2", "V3", "V4", "V5", "v6")
# confirmed.samples.ken                           <- confirmed.samples.test
# 
# confirmed.samples.ken$positive <- as.matrix.cast_df(confirmed.samples.ken$positive)
# confirmed.samples.ken$total.samples <- as.matrix.cast_df(confirmed.samples.ken$total.samples)
# 
# 
# rm(confirmed.samples.test, confirmed.samples)
# 
# 
# #       5. Make the Kenyan population data a vector
# 
 popken          <- read.csv(here::here("Data", "demography_ken.csv"))
# if(year == "2010") {demography.ken <- popken$X2010}
# if(year == "2011") {demography.ken <- popken$X2011}
# if(year == "2012") {demography.ken <- popken$X2012}
# if(year == "2013") {demography.ken <- popken$X2013}
# if(year == "2014") {demography.ken <- popken$X2014}
# if(year == "2015") {demography.ken <- popken$X2015}
# if(year == "2016") {demography.ken <- popken$X2016}
# if(year == "2017") {demography.ken <- popken$X2017}
# if(year == "2018") {demography.ken <- popken$X2018}
# 
# #     6. Making the Kenyan base vaccination calendar for 6 age groups & 3 risk groups which is zero vaccination
# dates                 <- seq(as.Date("2010-01-01"), as.Date("2010-01-01") + 90, by = "1 week")
# efficacy              <-  rep(0,18)
# coverage              <-  matrix(rep(0,18*length(dates)),ncol = 18)
# 
# vaccine.calendar.ken <- as_vaccination_calendar(efficacy = efficacy, dates = dates, 
#                                                 coverage = coverage, no_age_groups = 6,
#                                                 no_risk_groups = 2)


