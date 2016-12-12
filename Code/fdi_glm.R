#=============================================================#
# John Schoeneman
# Work Done For: FDI Network Analysis RA-IGERT
# Date: Fall 2016
# Work Done: GLM models
# Machine: MacPro OSX Yosemite
#=============================================================#


# clear workspace
rm(list=ls())

set.seed(19)

setwd("/Users/johnpschoeneman/Documents/school/Penn State/RA:TA/FDI_IGERT_H/Code")

#load in data
fdi <- read.csv("fdi_panel.csv", stringsAsFactors=FALSE)        #FDI

fdi$Value <- ifelse(is.na(fdi$Value), 0, fdi$Value)



# pooled OLS
fit.1 <- lm(Value ~ dist + Dest.GDP + Origin.GDP + Dest.polity + Origin.polity+ depth_index,data = fdi)






