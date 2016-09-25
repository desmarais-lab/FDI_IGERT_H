#=============================================================#
# John Schoeneman
# Work Done For: R GIS
# Date: August 2016
# Work Done: Clean FDI data
# Machine: MacPro OSX Yosemite
#=============================================================#

# clear workspace
rm(list=ls())


setwd("/Users/johnpschoeneman/Dropbox/FA16 - RA - ZHU")


fdi <- read.csv("fdi_panel", stringsAsFactors=FALSE)
distance <- read.csv("dist.csv", stringsAsFactors=FALSE)
GDP <- read.csv("WB_GDP.csv", stringsAsFactors=FALSE)


# add distance data


fdi <- merge(fdi, distance, by.x =c("Dest.Code", "Origin.Code"), by.y = c("iso_d", "iso_o"))
fdi <- fdi[order(fdi$Destination, fdi$Year),] 

# GDP data

fdi <- merge(fdi, GDP, by.x = c("Dest.Code", "Year"), by.y = c("Country.Code", "Year"))
colnames(fdi)[20] <- "Dest.GDP"
fdi <- merge(fdi, GDP, by.x = c("Origin.Code", "Year"), by.y = c("Country.Code", "Year"))
colnames(fdi)[21] <- "Origin.GDP"



#write csv
write.csv(fdi, file = "fdi_panel")
