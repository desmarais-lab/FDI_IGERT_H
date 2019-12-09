# clear workspace
rm(list=ls())
set.seed(19)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


### Read in data
load("tax_cov.Rdata")
load("tax_net.Rdata")

for(i in 1:8){
  print(netlist[[i]])
}
#108 to 113 nodes
#Node list: "polity","trade_opennes",gdp.pc", "OECD_mem"
#Edge list:  1:9 = c(lag, mass, dist, alliance, defense, trade_vol, bit, both_oecd, pta_depth) 

# network models
ergm_network <- expression(ergm(net ~ sum+ sum(1/2) +nonzero+#mutual+
                                  edgecovmutual(oecd_both)+ edgecovmutual(not_oecd_both)+
                                  transitiveweights("min", "max", "min")+
                                  nodeicov("polity")+nodeocov("polity")+
                                  nodeicov("trade_opennes")+nodeocov("trade_opennes")+
                                  nodeicov("gdp.pc")+nodeocov("gdp.pc")+
                                  nodeicov("corporate_tax_rate")+nodeocov("corporate_tax_rate")+
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
                                                     MCMC.interval=1000)))


list_of_networks=netlist
list_of_edgecovs=covlist
seed=1234
ncores=8

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
  edge_vals <- net %e% "Value"
  net %e% "tValue" <- ceiling(log(edge_vals)*2)
  edge_vals2 <- net %e% "tValue"
  eval(ergm_network)
}
stopImplicitCluster()

print("finished network models")

save.image("tax_results_RR.RData")
