#Updated 05/07/2022
#for SA where the vaccination scenarios are different to the main model----
#Use this script to run all the scripts sequentially
library(tidyverse)
#Load data files
filename <- "SA-coverage"

source(here::here("Econ/Scripts/SA", paste0("00_load_data_SA.R")))

create.date <- lubridate::today()
read.date <- lubridate::today() #as.Date("2022-05-18")
base.year <- 2010

#These MUST be specified at all times
#default values are 
#vacc_price - 3; durn_illness - 4
vacc_price <- 3 #1.5,3, 6, 10
durn_illness <- 4 #7

#create new folder to save output
dir.create(here::here(paste0("Econ/Model_outcomes_output/Final", "/", 
                             filename, "/", 
                             create.date)),
           recursive = TRUE) #automatically creates subfolders

##run for all SAs - no change----
source(here::here("Econ/Scripts", 
                  "01_Public-health-outcomes_mine_short.R"))

source(here::here("Econ/Scripts", 
                  "03_totalyearoutputs1_1_mine_short.R"))

# 

##run if SA using different coverage levels / year-round vaccination - scenarios are different----
source(here::here("Econ/Scripts/SA",
                  "04_a_summaries_SA.R"))
# #
source(here::here("Econ/Scripts/SA",
                  "04_b_two.way.plots_SA.R"))

source(here::here("Econ/Scripts/SA",
                  "04_c_icers_nmbs_SA.R"))

source(here::here("Econ/Scripts/SA",
                  "04_d_threshold.vaccine.price_SA.R"))

source(here::here("Econ/Scripts/SA",
                  "04_e_boxplots.all.params_SA.R"))

##############################
#for SA where the vaccination scenarios are the same as the main model----
#i.e. the following
# a. SA-exact-eff
# b. SA-fixed-reduction
# c. SA-pop-add
# d. SA for vaccine prices
# e. SA for duration of illness (if running)

#Use this script to run all the scripts sequentially
library(tidyverse)
#Load data files
# filename <- "SA-pop-add"
# source(here::here("Econ/Scripts/SA", paste0("00_load_data_SA.R")))

#filename <- paste0("Sensitivity_vprice", ".", vacc_price)
#source(here::here("Econ/Scripts/SA", "00_load_data.R"))


create.date <- lubridate::today()
read.date <- lubridate::today() #as.Date("2022-05-18")
base.year <- 2010

#These MUST be specified at all times
#default values are 
#vacc_price - 3; durn_illness - 4
vacc_price <- 3 #1.5,3, 6, 10
durn_illness <- 4 #7



#create new folder to save output
dir.create(here::here(paste0("Econ/Model_outcomes_output/Final", "/", 
                             filename, "/", 
                             create.date)),
           recursive = TRUE)

#run for all SAs - no change----
source(here::here("Econ/Scripts", 
                  "01_Public-health-outcomes_mine_short.R"))

source(here::here("Econ/Scripts", 
                  "03_totalyearoutputs1_1_mine_short.R"))


#run if SA using main model - scenarios are same i.e. vaccine price / durn illness/ susceptibility change----
#.rs.restartR() # to restart R
source(here::here("Econ/Scripts", "00_1_read.all.samples.R"))


source(here::here("Econ/Scripts",
                  "04_a_summaries.R"))

source(here::here("Econ/Scripts",
                  "04_b_two.way.plots.R"))

source(here::here("Econ/Scripts",
                  "04_c_icers_nmbs.R"))

source(here::here("Econ/Scripts",
                  "04_d_threshold.vaccine.price.R"))

source(here::here("Econ/Scripts",
                  "04_e_boxplots.all.params.R"))


##############For vaccine price SAs only ----
filename <- "Naomi"
source(here::here("Econ/Scripts", "00_load_data.R"))

#These MUST be specified at all times
#default values are 
#vacc_price - 3; durn_illness - 4
vacc_price <- 10 #1.5,3, 6, 10
durn_illness <- 4 #7


create.date <- paste0("SA-vprice-", vacc_price, "_", lubridate::today())
read.date <- create.date
base.year <- 2010





#create new folder to save output
dir.create(here::here(paste0("Econ/Model_outcomes_output/Final", "/", 
                             filename, "/", 
                             create.date)),
           recursive = TRUE)

#run for all SAs - no change----
source(here::here("Econ/Scripts", 
                  "01_Public-health-outcomes_mine_short.R"))

source(here::here("Econ/Scripts", 
                  "03_totalyearoutputs1_1_mine_short.R"))


#run if SA using main model - scenarios are same i.e. vaccine price / durn illness/ susceptibility change----
#.rs.restartR() # to restart R
source(here::here("Econ/Scripts", "00_1_read.all.samples.R"))


source(here::here("Econ/Scripts",
                  "04_a_summaries.R"))

source(here::here("Econ/Scripts",
                  "04_b_two.way.plots.R"))

source(here::here("Econ/Scripts",
                  "04_c_icers_nmbs.R"))

source(here::here("Econ/Scripts",
                  "04_d_threshold.vaccine.price.R"))

source(here::here("Econ/Scripts",
                  "04_e_boxplots.all.params.R"))

