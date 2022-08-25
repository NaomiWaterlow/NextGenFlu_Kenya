# Kenya model setup
#set the file path to the main folder <- change here and all other should be read in. 
setwd("~/Documents/GitHub/NextGenFlu_Kenya/")
library(here)
library(dplyr)
library(fluEvidenceSynthesis)
library(ISOweek)
####### VARIABLES  ########

# specify user input
location <- "Kenya"
posterior_sample_size <- 10
set.seed(100)
target_scenarios <- c(1,4,28,53,75,122)
#coverage at 75%
# target_scenarios <- c(1,12,36,61, 83, 130)
exact_efficacies <- F

name_run <- "default"#"POP_ADD_WANING" #"year_round"#"default"#"coverage"EXACT_EFF
use_presampled <- F
save_samples <- F
change_susceptibility_switch <- "OFF" # Can be OFF or POP_ADD_WANING or FIXED_REDUCTION
save_susceptibility <- T
end_first_year_vaccination <- "2011-03-01"

# Parameter details
num_parameters_posteriors <- 9
transmisibility_location <- 5 # position in posterior of the transmissibility parameter
infection_delays <- c(0.8,1.8)

####### KENYA CONSTANTS ######

# these are defined globally but called within various functions
age_groups_model <- c(1, 6, 15, 20, 50)
max_age <- 70
num_age_groups <- 6
risk_ratios_input <- matrix(c(0, 0, 0, 0, 0, 0, 
                              0, 0, 0, 0, 0, 0),
                            ncol = num_age_groups , byrow = T)
no_risk_groups <- 1
susceptibility_pattern <- c(6,6,6,7,7,8)
calculate_FOI <- "Yes" # this should always be Yes for Kenya
num_years <- 8
years <- c(2010:2019)
# using one fixed value for the contact matrices on vaccination - it doesn't matter as theres no infection
contact_ids_input <- as.matrix(read.csv(here::here("Posteriors","2010-03-12 to 2010-12-17 AH3N2 UK_presampled_suscchange.csv"))[1,13:579])
high_risk <- rep(0,6)
risk_group_labels <- data.frame(id =c(1:18), 
                                label =c(rep("Risk_group1",num_age_groups),
                                         rep("Risk_group2",num_age_groups),
                                         rep("Risk_group3",num_age_groups)))

age_group_labels <- data.frame(id =c(1:18), 
                               label =rep(c("Age0","Age1-5","Age6-14","Age15-19","Age20-49","Age50+"),3))

##### SOURCE AND RUN #######

# Load population and contact matrices for Kenya
source(here::here("Vacc_epi_model","Prep_Kenyan_data_NRW.R"))
relevant_polymod <- polymod.ken
# The vaccination scenario <- edit to get the required scenarios
source(here::here("Vacc_epi_model","1_0_vaccination_scenario_list_Kenya.R"))
#The list of epidemics in Kenya
source(here::here("Vacc_epi_model","2_0_epidemics_list_Kenya.R"))

# Run all the modelling!
source(here::here("Vacc_epi_model", "0_Main_NextGen.R"))

