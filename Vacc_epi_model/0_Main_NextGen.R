# Main script for the Next Gen models 

####### LOAD AND SOURCE ########

# Load packages
library(data.table)
library(MASS)
library(ggplot2)
library(fluEvidenceSynthesis)
library(plyr)
library(reshape2)
library(ISOweek)

###### VACCINATION MODEL ######
# the vaccination list will have been loaded from the specifications sheet

# The vaccination model functions
source(here::here("Vacc_epi_model","1_1_model_vaccine_waning.R"))
# Setup and run the vaccination model
source(here::here("Vacc_epi_model","1_2_Vaccination_Model.R"))

###### EPIDEMIC MODEL #######

#The epidemic model function
source(here::here("Vacc_epi_model","2_1b_model_epidemic_yearcross.R"))
# Run the vaccination model for different epidemics and vaccine scenarios
source(here::here("Vacc_epi_model","2_2_Epidemic_Model.R"))

###### BACKGROUND FOI MODEL #########

if (calculate_FOI == "Yes"){
# Read in the data for the FOI calculations
source(here::here("Vacc_epi_model","3_0_data_for_FOI.R"))
# Background FOI timings
source(here::here("Vacc_epi_model","3_1_Background_FOI.R"))
# work out background FOI for different vaccination scenarios
source(here::here("Vacc_epi_model","3_2_FOI_with_vaccinations.R"))
}
###### COMINE AND PLOT #######

source(here::here("Vacc_epi_model","4_0_combine_and_plot.R"))





