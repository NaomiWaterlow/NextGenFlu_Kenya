
#Use this script to run all the code files sequentially

################# These scripts specify file paths to read in epi model data and create file paths for output files
################# Must be run at the start at all times 
################# Begin----
#Step 0:
#Load data files and relevant R packages
#Loads two files from the Data/Final folder - these are the output from the epidemiological model
#a. Grouped_totals.csv -  1000 samples of influenza infection data across six age groups
#b. Vaccine_model_output - data on total vaccine doses used per vaccination scenario in each year from 2010-2019
#This script also loads demography data for Kenya, probability and costs parameters used in the economic model
source(here::here("Econ/Scripts", "00_load_data.R"))


#Specifying vaccine price (assumed to be $3 in the main model) and duration of illness (assumed to be 4 days)
#These can be changed here for sensitivity analyses
vacc_price <- inputs[47,"Mean"]
durn_illness <- 4

#Specifies the file path to save plots and output files.
#Note: If generating new model output for the first time, then read.date must be = create.date
filename <- "Naomi"
create.date <- paste0(lubridate::today())
read.date <- paste0(lubridate::today()) #as.Date("2022-06-09")
dir.create(here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date)),
           recursive = TRUE)

################# End----


################ Steps 1 and 2 need to be run only if generating new output from the economic model 
#Step 1:
#this script generates 1000 samples for each health outcome (e.g. hospitalisations, deaths, YLDs) and 
#costs (e.g. vaccination costs, direct or indirect costs), per age group, per vaccine scenario, per year. 
#The output is saved as a .csv file in the Model_outcomes_output folder whose file path is specified in Step 0
#This script calls four other scripts internally
#02_a_outputs1outcomes_mine.R
#02_b_outputs2healthcareutilization_mine.R
#02_c_outputs3DALYS_I_mine.R
#02_e_outputs5costs_mine.R


source(here::here("Econ/Scripts", 
                  "01_Public-health-outcomes_mine_short.R"))

#Step 2:
#this script reads in the output file generated in Step 1, calculates total and incremental costs and health outcomes,
#YLLs, total DALYs, various health outcomes and DALYs averted and 
#inflates all costs to 2019 USD terms, for each scenario and year.
#The output is saved as a .csv file in the Model_outcomes_output folder whose file path is specified in Step 0

source(here::here("Econ/Scripts", 
                  "03_totalyearoutputs1_1_mine_short.R"))

##############Note: you can skip running code from Steps 1 and 2 if not generating new model output and 
############## start from Step 3. If doing this, make sure that read.date is clearly specified.
#Step 3:
#This script reads in the output file generated in Step 2, generates discounted costs and
#calculates total health outcomes and costs across the ten years (2010-2019) for each vaccination scenario. 
#The output (costs and outcomes for all years combined) is saved as a .csv file 
#in the Model_outcomes_output folder whose file path is specified in Step 0
source(here::here("Econ/Scripts", "00_1_read.all.samples.R"))


#Step 4:
#The following files (starting with 04_) generate summary tables and plots
#All output is saved in the Model_outcomes_output folder whose file path is specified in Step 0

#Summary tables for each year and all years combined
source(here::here("Econ/Scripts", 
                  "04_a_summaries.R"))

#Plots of interactions between vaccine doses, discounted incremental costs and cases averted
source(here::here("Econ/Scripts", 
                  "04_b_two.way.plots.R"))

#ICERs, INMBs, CEACs
source(here::here("Econ/Scripts", 
                  "04_c_icers_nmbs.R"))

#Calculating threshold vaccine prices
source(here::here("Econ/Scripts",
                  "04_d_threshold.vaccine.price.R"))

#Generate boxplots of all outcomes and costs for each scenario per year and across all years combined
source(here::here("Econ/Scripts",
                  "04_e_boxplots.all.params.R"))
