# clear workspace, set seed, and set wd
rm(list=ls())

set.seed(19)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Load Libraries
library(ergm)

### Read in data
load("q25_cov.Rdata")
load("q25_net.Rdata")


#Node list: "polity","trade_opennes",gdp.pc"
#Edge list:  lag, mass, dist, alliance, defense, trade_vol, bit 




# the no offset call strips the ERGM call of the offset() function and the offset.coef argument
# terms must match and be in the same order
ergm.call.no.offset <- expression(ergm(net ~ sum + sum(pow=1/2)+ nonzero +
                                         mutual(form="min")+transitiveweights("min", "max", "min")+
                                         nodeicov("polity")+nodeocov("polity")+
                                         nodeicov("trade_opennes")+nodeocov("trade_opennes")+
                                         nodeicov("pop")+nodeocov("pop")+
                                         nodeicov("gdp.pc")+nodeocov("gdp.pc")+
                                         edgecov(edgecovars[[1]])+edgecov(edgecovars[[2]])+
                                         edgecov(edgecovars[[3]])+edgecov(edgecovars[[4]])+
                                         edgecov(edgecovars[[5]])+edgecov(edgecovars[[6]])+
                                         edgecov(edgecovars[[7]]),
                                       response="Value_ln",
                                       reference=~Poisson,
                                       control=control.ergm(MCMLE.trustregion=100,
                                                            MCMLE.maxit=50, 
                                                            MCMC.samplesize=10000,
                                                            MCMC.burnin=500,
                                                            MCMC.interval=1000)
))


list_of_networks=netlist
ergm_call=ergm.call
ergm_call_no_offset=ergm.call.no.offset
list_of_edgecovs=covlist
seed=1234
ncores=12

require(ergm)
require(ergm.count)
require(doParallel)
require(foreach)


## Doing individual ergm estimation 
ergms <- foreach(i=1:length(list_of_networks)) %dopar% {
  net <- list_of_networks[[i]]
  if(length(list_of_networks)==length(list_of_edgecovs)){
    edgecovars <- list_of_edgecovs[[i]]
  }
  set.seed(seed)
  eval(ergm_call_no_offset)
}
stopImplicitCluster()

print("finished stage 1")

save.image("ERGMResults25.RData")
