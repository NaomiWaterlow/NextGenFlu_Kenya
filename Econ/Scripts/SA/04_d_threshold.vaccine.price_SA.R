library(here)

#Load data, modify----
#Run only if running this script directly without running 01_Public-health-outcomes_mine_short.T

################NOTE - ALWAYS RUN run_scripts.R first
source(here::here("Econ/Scripts/SA", paste0("00_1_read.all.samples_SA", ".R")))
source(here::here("Econ/Scripts", "04_d_threshold.vaccine.price.R"))