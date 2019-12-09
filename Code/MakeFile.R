# This can be run start to finish to run all code.
# Neccesary Pacakges: ADD LIST OF ALL PACKAGES
# Assumes initial working directory is the same as this script
# Assumes machine has at least 11 cores


# clear workspace
rm(list=ls())
set.seed(19)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


start_time2 <- Sys.time()
source("contagion_simulation_interpretation.R") 
end_time2 <- Sys.time()
end_time2 - start_time2

###################################### create_datasets ######################################

# Run each of these in order and before any other scripts

setwd("create_datasets/")
source("raw_download.R.R") #Time depends on internet speeds: usually < 10 minutes
source("raw_clean_data.R") # 6.5 seconds
source("clean_add_covariates.R") # 30 seconds
source("clean_subset_panel.R") # 30 seconds

###################################### main  ######################################

# Run first two scripts in order listed. 
# All others can be run simulataneously after 'main_ergm.R' is completed.)
setwd("../main/")
source("main_ergm_prep.R") # 7 minutes
source("main_ergm.R") # 50.58 hours
source("ModelFitComparison.R") # 10 seconds
source("main_rl_plots.R") # 30 seconds
source("ModelInterpretation.R") # 1.1 hours
source("NetworkStatisticsFit.R") # 45 seconds
source("CovariateInterpretation.R") # 15.1 seconds
source("contagion_simulation_interpretation.R") # <1 second



###################################### supplementary ######################################



setwd("../supplementary/TERGM/")
source("TERGM_OutsidePooledFunction.R")
source("TERGM_rl_plots.R")

setwd("../Omit_Tax_Havens/")
source("tax_ergm_prep.R")
source("tax_ergm.R")
source("tax_plots.R")
source("haven_ergm_prep.R")
source("haven_ergm.R")
source("haven_rl_plots.R")

setwd("../Omit_EU/")
source("EU_ergm_prep.R")
source("EU_ergm.R")
source("EU_rl_plots.R")

setwd("../Omit_Missing_Values")
source("q_p.R")
source("q_ergm_prep.R")
source("q_ergm.R")
source("q_rl_plots.R")

setwd("../Impute_Missing_Values")
source("amelia_ergm_prep.R")
source("amelia_ergm.R")
source("amelia_rl_plots.R")
