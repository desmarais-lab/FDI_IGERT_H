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
tax_havens <- c("Namibia","Trinidad and Tobago","Bahrain", "Luxembourg",
                "United Kingdom","Ireland","Netherlands")
`%notin%` <- Negate(`%in%`)

for(n in 1:11){
  netlist[[n]] <- netlist[[n]] %s% which(netlist[[n]] %v% "vertex.names" %notin% tax_havens)
}

save(netlist, file = "haven_net.Rdata")

for(y in 1:11){
  for(v in 1:9){
    covlist[[y]][[v]] <- subset(covlist[[y]][[v]], rownames(covlist[[y]][[v]]) %notin% tax_havens)
    covlist[[y]][[v]] <- covlist[[y]][[v]][,(colnames(covlist[[y]][[v]]) %notin% tax_havens)]
  }
}

#SAVE as Rdata
save(covlist, file = "haven_cov.Rdata")