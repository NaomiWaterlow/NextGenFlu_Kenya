# NextGenFlu_Kenya

This repository contains the code for running an influenza transmission model and cost-effectiveness analysis for next generation vaccines. Please see manuscript for details (DOI and link will be added once accepted on MedRxiv). 

To run the epidemiological model for Kenya, please run 0_Kenya_specifications.R in the Vacc_epi_model folder. This will run the whole epidemiological analysis, and any input changes (such as sample size) can be changed in this Rscript. In addition various sensitivity analyses can be specified in this Rscript. 

The output from running this analysis is overall_storage_default.Rdata and Vaccine_model_output_default.Rdata. These are then used as inputs in the economic analysis. 

To run the economic model for Kenya, please navigate to the Econ/Scripts folder and run run_scripts.R, which details the sequence of files called and output generated. All input files and output generated for the economic analyses are saved in the Data and Model_outcomes_output folders, respectively. 


NOTE: Due to the size of the posterior files, only a subset of the posterior is included in this repository



