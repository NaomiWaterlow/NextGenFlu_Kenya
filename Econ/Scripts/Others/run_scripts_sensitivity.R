#Use this script to run all the scripts sequentially

#Load data files
source(here::here("Econ/Scripts", "00_load_data.R"))

 
create.date <- lubridate::today()
read.date <- lubridate::today() #as.Date("2022-05-18") 

vacc_price <- 3 #1.5,3, 6, 10
durn_illness <- 7
filename <- paste0("Sensitivity_d.illness", ".", durn_illness)



#create new folder to save output
dir.create(here::here(paste0("Econ/Model_outcomes_output", "/", 
                             filename, "/", 
                             create.date)),
           recursive = TRUE)


source(here::here("Econ/Scripts", 
                  "01_Public-health-outcomes_mine_short.R"))

source(here::here("Econ/Scripts", 
                  "03_totalyearoutputs1_1_mine_short.R"))

# source(here::here("Econ/Scripts", "00_1_read.all.samples.R"))
# 
# source(here::here("Econ/Scripts", 
#                   "04_a_summaries.R"))
# 
# source(here::here("Econ/Scripts", 
#                   "04_b_two.way.plots.R"))
# 
# source(here::here("Econ/Scripts", 
#                   "04_c_icers_nmbs.R"))
# 
# source(here::here("Econ/Scripts",
#                   "04_d_threshold.vaccine.price.R"))