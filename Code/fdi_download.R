#=============================================================#
# John Schoeneman
# Work Done For: R GIS
# Date: August 2016
# Work Done: Call UNCTAD API for FDI data
# Machine: MacPro OSX Yosemite
#=============================================================#

# clear workspace
rm(list=ls())


#Load Necessary Packages
library(gdata)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#create function to create API

get.FDI <- function(url="http://unctad.org/Sections/dite_fdistat/docs/webdiaeia2014d3_"
                         ,code = "", file_ext= ".xls")
{
  #create address
  string <- paste(url,code,file_ext, sep = "")
  #read in file
  fdi_file <- read.xls(string,header=TRUE, sheet=3)
  #write file to folder
  folder = "/Stock_Raw/FDI_in_"
  file_1 <- paste(code, ".csv", sep = "")
  write.csv(fdi_file, file = file_1)
}



# Read in country list
codes <- read.csv("country_codes.csv",header=TRUE, sep=",")
codes <- codes[170:205,]

#loop downloading data into folder
for(ab in codes[,3]){
  
  get.FDI(code=ab)
  
  #Sys.sleep(10)
  
}



