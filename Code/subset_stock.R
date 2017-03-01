#=============================================================#
# John Schoeneman
# Work Done For: FDI Network Analysis RA-IGERT
# Date: Fall 2016
# Work Done: Network Measures and MRQAP
# Machine: MacPro OSX Yosemite
#=============================================================#


# clear workspace
rm(list=ls())

set.seed(19)

# libraries
library(GERGM)
library(network)
library(igraph)
library(doBy)


#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load in data
fdi <- read.csv("panel_stock.csv", stringsAsFactors=FALSE)        #FDI
#201 countries, 12 years (2000-2012), 
fdi$Value <- ifelse(is.na(fdi$Value), 0, fdi$Value)
fdi$defense.max.x <- ifelse(is.na(fdi$defense.max.x), 0, fdi$defense.max.x)
fdi$nonaggression.max.x <- ifelse(is.na(fdi$nonaggression.max.x), 0, fdi$nonaggression.max.x)
fdi$neutrality.max.x <- ifelse(is.na(fdi$neutrality.max.x), 0, fdi$neutrality.max.x)
fdi$entente.max.x <- ifelse(is.na(fdi$entente.max.x), 0, fdi$entente.max.x)



fdi <- fdi[c("Destination","Origin","Year",  "Value", "contig","comlang_off", "comlang_ethno","colony",
             "comcol", "curcol","dist","Dest.GDP","Origin.GDP","defense.max.x","nonaggression.max.x",
             "neutrality.max.x","entente.max.x","Dest.polity","Origin.polity","Dest.TO", "Dest.GDP.g", 
             "Origin.pop", "Origin.TO", "Origin.GDP.g", "Dest.pv", "Origin.pv", "depth_index", "depth_latent",
             "trade_hco", "trade_int", "trade_cap", "trade_mix")]

fdi01 <- subset(fdi, fdi$Year ==2001)
for(i in 3:32){
  fdi01[,i] <- as.numeric(fdi01[,i])
  
}
fdi01 <- na.omit(fdi01) 
names <- data.frame(unique(fdi01$Destination))
colnames(names)[1] <- "Destination"
names$year = 2001


for(i in 2002:2012){
  
  fdi_t <- subset(fdi, fdi$Year ==i)
  for(i in 3:32){
    fdi_t[,i] <- as.numeric(fdi_t[,i])
    
  }
  fdi_t <- na.omit(fdi_t)
  names_t <- data.frame(unique(fdi_t$Destination))
  colnames(names_t)[1] <- "Destination"
  names_t$year = i
  names <- merge(names, names_t, by = "Destination")
  
}


fdi01 <- subset(fdi, fdi$Year ==2001)
for(i in 3:32){
  fdi01[,i] <- as.numeric(fdi01[,i])
  
}
fdi01 <- na.omit(fdi01) 
names2 <- data.frame(unique(fdi01$Origin))
colnames(names2)[1] <- "Destination"
names2$year = 2001


for(i in 2002:2012){
  
  fdi_t <- subset(fdi, fdi$Year ==i)
  for(i in 3:32){
    fdi_t[,i] <- as.numeric(fdi_t[,i])
    
  }
  fdi_t <- na.omit(fdi_t)
  names_t <- data.frame(unique(fdi_t$Origin))
  colnames(names_t)[1] <- "Destination"
  names_t$year = i
  names2 <- merge(names2, names_t, by = "Destination")
  
}
names3 <- merge(names, names2, by = "Destination")

names <- data.frame(unique(names3$Destination))
rm(fdi_t, fdi01, names_t, names2, names3, i)
colnames(names)[1] <- "Country"
names$new = 1

fdi_n  <- merge(fdi, names, by.x = "Destination", by.y = "Country")
fdi_n <- fdi_n[,-33]
fdi_n  <- merge(fdi, names, by.x = "Origin", by.y = "Country")
fdi_n <- fdi_n[,-33]

fdi_n <- unique(fdi_n[, 1:32]) 
fdi_n <- fdi_n[,c(1,3,2, 4:32)]


#create full panel
library(gtools)
id1 <- data.frame(unique(names$Country)) # 186
years <- seq(2001, 2012)
panel <- expand.grid(x = id1[,1], y = id1[,1])
panel <- subset(panel, panel[,1] != panel[,2])
colnames(panel)[1] <- "Destination"
colnames(panel)[2] <- "Origin"
panel <- do.call("rbind", replicate(12, panel, simplify = FALSE)) # 408480
yearid <- data.frame()
for(i in years){
  year <- data.frame(rep(i, length(names[,1])*(length(names[,1])-1)))
  yearid <- rbind(yearid,year)
}
panel$Year <- yearid[,1]

rm(i, id1, years, yearid, year)



fdi_m <- merge(panel, fdi_n, by= c("Destination", "Origin", "Year"))

fdi_m <- na.omit(fdi_m)




#write csv
write.csv(fdi_m, file = "sub_stock.csv")


