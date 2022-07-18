# NextGenFlu_Kenya

This repository contains the code for running an influenza transmission model and cost-effectiveness analysis or next generation vaccines. Please see manuscript for details (DOI and link will be added once accepted on MedRxiv). 

To run the epidemiological model for Kenya, please run 0_Kenya_specifications.R in the Vacc_epi_model folder. This will run the who epidemiological analysis, and any input changes (such as sample size) can be changed in this Rscript. In addition various sensitivity analyses can be specified in this Rscript. 

The output from running this analysis is overall_storage_default.Rdata and Vaccine_model_output_default.Rdata. These are then used as inputs in the economic analysis. 




