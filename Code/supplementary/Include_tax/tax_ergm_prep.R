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

########## Add tax rates ##############


# add country codes for merging
codes <- read.csv("WB_country_codes.csv", stringsAsFactors=FALSE)
codes <- unique(c(codes$Origin.Code, codes$Origin))
codes <- as.data.frame(codes[1:173], codes[174:346])
colnames(codes)[1] <- "code"
codes$name <- rownames(codes)
# Corporate Tax rates
taxWB <- read.csv("WB_tax_rates.csv", stringsAsFactors=FALSE) 
# clean off variables
taxWB <- taxWB[,-2:-3]
colnames(taxWB)[3] <- "corporate_tax_rate"
#merge to panel and drop missing obs.
taxWB <- na.omit(taxWB)
taxWB$Time <- as.numeric(taxWB$Time)
taxWB <- merge(taxWB, codes, by.x = "Country.Code", by.y = "code")
rm(codes)

netlist <- netlist[4:11]
covlist <- covlist[4:11]
yrs <- 2005:2012

#update data
for(y in 1:8){
#identify countries that have data
df_tax <- subset(taxWB, taxWB$Time==yrs[y])[,3:4]
nodes <- subset(unique(df_tax$name), 
                unique(df_tax$name) %in% network.vertex.names(netlist[[y]]))
df_tax <- subset(df_tax, df_tax$name %in% nodes)
#update drop nodes in covlist with missing data
for(v in 1:9){
  covlist[[y]][[v]] <- subset(covlist[[y]][[v]], rownames(covlist[[y]][[v]]) %in% nodes)
  covlist[[y]][[v]] <- covlist[[y]][[v]][,(colnames(covlist[[y]][[v]]) %in% nodes)]
}
#drop nodes in networks with missing data
netlist[[y]] <- netlist[[y]] %s% which(netlist[[y]] %v% "vertex.names" %in% nodes)
#add node level data
row.names(df_tax) <- df_tax$name
df_tax <- df_tax[c(network.vertex.names(netlist[[y]])),]
netlist[[y]] %v% "corporate_tax_rate" <- df_tax$corporate_tax_rate
}
#save files
save(covlist, file = "tax_cov.Rdata")
save(netlist, file = "tax_net.Rdata")