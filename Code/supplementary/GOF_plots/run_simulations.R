# clear workspace
rm(list=ls())
set.seed(19)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#libraries
library(ergm.count)

#load data
load("../../main/main_network_results_RR.RData")


ncores=2
#run simulations
require(ergm)
require(ergm.count)
require(doParallel)
require(foreach)

registerDoParallel(cores=ncores) 
sim_list <- foreach(yr=1:length(list_of_networks)) %dopar% {
sim.net <- simulate(ergms[[yr]],nsim=100)
}
stopImplicitCluster()
#save data
save(sim_list,file="network_result_sims.RData")


#load data
load("../../main/main_independent_results_RR.RData")
ncores=2

registerDoParallel(cores=ncores) 
sim_list <- foreach(yr=1:length(list_of_networks)) %dopar% {
  sim.net <- simulate(ergms[[yr]],nsim=100)
}
stopImplicitCluster()
#save data
save(sim_list,file="network_independent_sims.RData")