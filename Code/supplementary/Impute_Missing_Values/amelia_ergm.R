# clear workspace, setting random seed, and setting work directory
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
i=1

for(i in 1:10){

### Read in data
load(paste0("imputed_dfs/MIdf", i, "_net.RData"))
load(paste0("imputed_dfs/MIdf", i, "_cov.RData"))


#Node list: "polity","trade_opennes",gdp.pc", "OECD_mem"
#Edge list:  1:9 = c(lag, mass, dist, alliance, defense, trade_vol, bit, both_oecd, pta_depth) 

# network models
ergm_network <- expression(ergm(net ~ sum+ sum(1/2) +nonzero+#mutual+
                                  edgecovmutual(oecd_both)+ edgecovmutual(not_oecd_both)+
                                  transitiveweights("min", "max", "min")+
                                  nodeicov("polity")+nodeocov("polity")+
                                  nodeicov("trade_opennes")+nodeocov("trade_opennes")+
                                  nodeicov("gdp.pc")+nodeocov("gdp.pc")+
                                  nodematch("OECD_mem")+
                                  edgecov(edgecovars[[1]])+edgecov(edgecovars[[2]])+
                                  edgecov(edgecovars[[3]])+edgecov(edgecovars[[4]])+
                                  edgecov(edgecovars[[5]])+edgecov(edgecovars[[6]])+
                                  edgecov(edgecovars[[7]])+edgecov(edgecovars[[9]]),
                                response="tValue",
                                reference=~Poisson,
                                control=control.ergm(MCMLE.trustregion=100,
                                                     MCMLE.maxit=100, 
                                                     MCMC.samplesize=10000,
                                                     MCMC.burnin=500,
                                                     MCMC.interval=1000)
))
list_of_networks=netlist
list_of_edgecovs=covlist
seed=1234
ncores=11

require(ergm)
require(ergm.count)
require(doParallel)
require(foreach)


## Doing individual ergm estimation 
registerDoParallel(cores=ncores) 
ergms <- foreach(i=1:length(list_of_networks)) %dopar% {
  net <- list_of_networks[[i]]
  if(length(list_of_networks)==length(list_of_edgecovs)){
    edgecovars <- list_of_edgecovs[[i]]
  }
  set.seed(seed)
  oecd_both <- edgecovars[["OECD_both"]]
  not_oecd_both <- 1-oecd_both
  eval(ergm_network)
}
stopImplicitCluster()

print(paste("finished imputation", i, "models"))


save.image(paste0("MI_results/MIresult_", i,".RData"))

}

