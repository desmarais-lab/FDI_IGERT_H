#=============================================================#
# John Schoeneman
# Work Done For: FDI Network Analysis RA-IGERT
# Date: Fall 2016
# Work Done: Perform GERGM analysis
# Machine: MacPro OSX Yosemite
#=============================================================#


# clear workspace
rm(list=ls())



setwd("/Users/johnpschoeneman/Documents/school/Penn State/RA:TA/FDI_IGERT_H/Code")

#load in data
fdi <- read.csv("fdi_panel.csv", stringsAsFactors=FALSE)        #FDI
#201 countries, 12 years (2000-2012), 