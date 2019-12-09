# clear workspace
rm(list=ls())
set.seed(19)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(amen)
library(statnet)
library(coda) 

### Read in data
load("../../main/main_net.Rdata")
load("../../main/main_cov.Rdata")


list_of_networks=netlist
list_of_edgecovs=covlist
seed=1234
ncores=11
mcmc_iter = 5000000
fits <- expression(ame(Y,Xrow=node_attr,Xcol=node_attr,Xdyad=Xd,R=2,family="nrm", nscan = mcmc_iter, burn = 1000, odens=1000))

require(amen)
require(doParallel)
require(foreach)
## Doing individual model estimation in parallel
#LSMs <- list()
registerDoParallel(cores=ncores) 
LSMs <- foreach(i=1:11) %dopar% {
  net <- list_of_networks[[i]]
  n_nodes <- length(net[[3]])
  node_attr <- matrix(nrow=n_nodes, ncol= 3) #create matrix of node variables
  node_attr[,1] <- get.node.attr(net, attrname ="polity")
  node_attr[,2] <- get.node.attr(net, attrname ="trade_opennes")
  node_attr[,3] <- get.node.attr(net, attrname ="gdp.pc")
  colnames(node_attr) <-c("polity","trade_openness", "gdp.pc")
  edge_vals <- net %e% "Value"
  net %e% "tValue" <- ceiling(log(edge_vals)*2)
  Y <- as.sociomatrix(net, attrname = "tValue")
  Xd <- sapply(list_of_edgecovs[[i]], identity, simplify="array")
  set.seed(seed)
  eval(fits)
}
stopImplicitCluster()

print("finished LSM models")

save.image("LSM_nrm_results2.RData")

for(i in 1:11){
  beta.mcmc <- as.mcmc(LSMs[[i]]$BETA)
  print(geweke.diag(beta.mcmc))
}

