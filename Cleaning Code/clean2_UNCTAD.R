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


appended_fdi <- read.csv("appended_fdi", stringsAsFactors=FALSE)


#get rid of empty rows and duplicate years
appended_fdi <- subset(appended_fdi, X2001!="")
appended_fdi <- subset(appended_fdi, X2001!="2001")

# get rid of aggregates
appended_fdi <- subset(appended_fdi, 
                         Origin != "World" &                                   
                         Origin != "Europe"&                             
                         Origin != "North America"&                 
                         Origin != "Other developed countries"&               
                         Origin != "Developing economies" &              
                         Origin != "South Asia"            &                            
                         Origin != "West Asia"              &            
                         Origin != "Other developed Europe"  &       
                         Origin != "Asia"              &
                         Origin != "Caribbean"          &            
                         Origin != "European Union"      &     
                         Origin != "Developed economies"  &
                         Origin != "Africa"&
                         Origin != "East Asia" &
                         Origin != "South-East Asia" &
                         Origin != "South America"&
                         Origin != "Unspecified" &
                         Origin != ".." &
                         Origin != "Transition economies"&
                         Origin != "International organizations"&
                         Origin != "Other Africa")


#rehape data
library(reshape2)
fdi <- reshape(appended_fdi, direction="long", varying=list(names(appended_fdi)[3:14]), v.names="Value", 
               idvar=c("Origin","destination"), timevar="Year", times=2001:2012)
fdi <- fdi[order(fdi$destination, fdi$Year),] 


  
# .. is equal to not available or reported
fdi$Value <- ifelse(fdi$Value =="..", NA, fdi$Value)

#  - create negative numbers
fdi$Value  <- gsub("- -", "-", fdi$Value)
fdi$Value  <- gsub("--", "-", fdi$Value)
fdi$Value  <- gsub(" ", "", fdi$Value)
fdi$Value  <- gsub("-0", "0", fdi$Value)
fdi$Value <- ifelse(fdi$Value =="-", 0, fdi$Value)

# Turn data back to numeric
fdi$Value <- as.numeric(fdi$Value)
fdi <- fdi[,2:5]


# Country Codes
ccodes <- read.csv("country_codes.csv", stringsAsFactors=FALSE, header=FALSE)
ccodes <- ccodes[,-4]
ccodes <- ccodes[,-2]


# merge to get Destination names
fdi <- merge(fdi, ccodes, by.x = "Origin", by.y = "V1")
fdi <- merge(fdi, ccodes, by.x = "destination", by.y = "V3")


#reorder and name variables
fdi <- fdi[,c(6, 1,2,5,3,4)]
names(fdi) <- c("Destination","Dest.Code","Origin", "Origin.Code","Year","Value") 
fdi <- fdi[order(fdi$Destination, fdi$Year),] 


#write csv
write.csv(fdi, file = "fdi_panel")

