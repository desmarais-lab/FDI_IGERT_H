# clear workspace
rm(list=ls())
set.seed(19)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load packages
library(network)


for(q in c(25,50)){
  
  #load in data
  ### Read in data
  load("../../main/main_net.Rdata")
  load("../../main/main_cov.Rdata")

  #read in q list
  nodes <- read.csv(paste0("q_",q,".csv"), stringsAsFactors=FALSE)[,2]

  #update data
  for(y in 1:11){
    #update drop nodes in covlist based on q list
    for(v in 1:9){
      covlist[[y]][[v]] <- subset(covlist[[y]][[v]], rownames(covlist[[y]][[v]]) %in% nodes)
      covlist[[y]][[v]] <- covlist[[y]][[v]][,(colnames(covlist[[y]][[v]]) %in% nodes)]
    }
    #drop nodes in networks with missing data
    netlist[[y]] <- netlist[[y]] %s% which(netlist[[y]] %v% "vertex.names" %in% nodes)
  }
  
#SAVE 
save(netlist, file = paste0("q",q,"_net.Rdata"))
save(covlist, file = paste0("q",q,"_cov.Rdata"))
}