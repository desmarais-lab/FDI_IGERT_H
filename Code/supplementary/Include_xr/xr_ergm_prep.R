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
WDI <- read.csv("WDI_xr_inflation.csv", stringsAsFactors=FALSE) 
# clean off variables
WDI <- WDI[,-2:-3]
colnames(WDI)[3:4] <- c("inflation", "xr")
#merge to panel and drop missing obs.
WDI[,1] <- as.numeric(WDI[,1])
WDI[,3] <- as.numeric(WDI[,3])
WDI[,4] <- as.numeric(WDI[,4])
WDI <- na.omit(WDI)
WDI <- merge(WDI, codes, by.x = "Country.Code", by.y = "code")
rm(codes)

yrs <- 2002:2012

#update data
for(y in 1:11){
#identify countries that have data
df_xr<- subset(WDI, WDI$Time==yrs[y])[,3:5]
nodes <- subset(unique(df_xr$name), 
                unique(df_xr$name) %in% network.vertex.names(netlist[[y]]))
df_xr <- subset(df_xr, df_xr$name %in% nodes)
#update drop nodes in covlist with missing data
for(v in 1:9){
  covlist[[y]][[v]] <- subset(covlist[[y]][[v]], rownames(covlist[[y]][[v]]) %in% nodes)
  covlist[[y]][[v]] <- covlist[[y]][[v]][,(colnames(covlist[[y]][[v]]) %in% nodes)]
}
#drop nodes in networks with missing data
netlist[[y]] <- netlist[[y]] %s% which(netlist[[y]] %v% "vertex.names" %in% nodes)
#add node level data
row.names(df_xr) <- df_xr$name
df_xr <- df_xr[c(network.vertex.names(netlist[[y]])),]
netlist[[y]] %v% "inflation" <- df_xr$inflation
netlist[[y]] %v% "xr" <- df_xr$xr
}
#save files
save(covlist, file = "xr_cov.Rdata")
save(netlist, file = "xr_net.Rdata")