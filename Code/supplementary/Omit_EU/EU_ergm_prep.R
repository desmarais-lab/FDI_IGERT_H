# clear workspace
rm(list=ls())
set.seed(19)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load packages
library(network)

#load in data
### Read in data
load("../../main/main_net.Rdata")
load("../../main/main_cov.Rdata")



# drop EU countries. 25 countries
EU <- c("Austria","Belgium","Bulgaria", "Cyprus","Germany","Denmark","Estonia","Spain",
"Finland","France","United Kingdom","Greece","Croatia","Hungary","Ireland",
"Italy","Lithuania" ,"Luxembourg","Latvia","Netherlands","Poland" ,"Portugal",
"Slovakia","Slovenia","Sweden")
`%notin%` <- Negate(`%in%`)

for(n in 1:11){
  netlist[[n]] <- netlist[[n]] %s% which(netlist[[n]] %v% "vertex.names" %notin% EU)
}

save(netlist, file = "EU_net.Rdata")

for(y in 1:11){
  for(v in 1:9){
    covlist[[y]][[v]] <- subset(covlist[[y]][[v]], rownames(covlist[[y]][[v]]) %notin% EU)
    covlist[[y]][[v]] <- covlist[[y]][[v]][,(colnames(covlist[[y]][[v]]) %notin% EU)]
  }
}

#SAVE as Rdata
save(covlist, file = "EU_cov.Rdata")
