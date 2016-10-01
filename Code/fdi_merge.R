#=============================================================#
# John Schoeneman
# Work Done For: FDI Network AnalysisRA-IGERT
# Date: FAll 2016
# Work Done: Merge Other Variables
# Project: 
# Machine: MacPro OSX Yosemite
#=============================================================#



# clear workspace
rm(list=ls())



setwd("/Users/johnpschoeneman/Documents/school/Penn State/RA:TA/FDI_IGERT_H/Code")

#load in data
fdi <- read.csv("fdi_clean.csv", stringsAsFactors=FALSE)
distance <- read.csv("dist.csv", stringsAsFactors=FALSE)
GDP <- read.csv("WB_GDP.csv", stringsAsFactors=FALSE)
violence <- read.csv("pv_total.csv", stringsAsFactors=FALSE)
WB <- read.csv("WB_var.csv", stringsAsFactors=FALSE)
polity <- read.csv("PolityIV.csv", stringsAsFactors=FALSE)
alliance <- read.csv("alliance.csv", stringsAsFactors=FALSE)
transparency <- read.csv("cpi.csv", stringsAsFactors=FALSE)


# add distance data
fdi <- merge(fdi, distance, by.x =c("Dest.Code", "Origin.Code"), by.y = c("iso_d", "iso_o"))
fdi <- fdi[order(fdi$Destination, fdi$Year),]

# GDP data
fdi <- merge(fdi, GDP, by.x = c("Dest.Code", "Year"), by.y = c("Country.Code", "Year"))
colnames(fdi)[20] <- "Dest.GDP"
fdi <- merge(fdi, GDP, by.x = c("Origin.Code", "Year"), by.y = c("Country.Code", "Year"))
colnames(fdi)[21] <- "Origin.GDP"


# Alliances

# Transparency

# PolityIV

# WB: Population, trade openness, and growth rate

# Supply Chains

# Political Violence

# PTA network

# Diaspora Network

# Transparency

# Resource Endowment

#write csv
write.csv(fdi, file = "fdi_edge.csv")



