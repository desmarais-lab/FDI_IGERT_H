# clear workspace
rm(list=ls())

set.seed(19)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load packages
library(magic)
library(network)
library(igraph)
library(doBy)
library(plyr)

#setwd("/Users/johnpschoeneman/Desktop/ACI/Count")

#load in data
fdi <- read.csv("sub_stock.csv", stringsAsFactors=FALSE)        #FDI
fdi <- fdi[,-1]
#125 countries, 12 years (2001-2012),
fdi <- fdi[,c(2,1,3:44)]

range_0to1 <- function(x){(x-min(x))/(max(x)-min(x))}

#scale continuous variables
vars <- c(18:34,36, 38:39, 41:44)
for(i in vars){
  
  fdi[,i] <- range_0to1(fdi[,i]) 
  
}

#subset by year, extract adjaceny matrices, and stack as diagonals #########################################

#FDI Stock

fdi02 <- subset(fdi, fdi$Year ==2002)
fdi_graph <- graph.data.frame(fdi02)
full <- get.adjacency(fdi_graph,attr='Value_ln', sparse=FALSE)
colnames(full) <- paste(colnames(full), "." ,"2002", sep = "")
row.names(full) <- paste(row.names(full), "." ,"2002", sep = "")

for(i in 2003:2012){

fdi_yr <- subset(fdi, fdi$Year ==i)
yr <- toString(i)
fdi_graph <- graph.data.frame(fdi_yr)
full_add <- get.adjacency(fdi_graph,attr='Value_ln', sparse=FALSE)
colnames(full_add) <- paste(colnames(full_add), "." , yr, sep = "")
row.names(full_add) <- paste(row.names(full_add), "." ,yr, sep = "")

full <- adiag(full, full_add)
}
write.csv(full, file = "fdi.csv") 

#lag FDI Stock
fdi_graph <- graph.data.frame(fdi02)
full <- get.adjacency(fdi_graph,attr='Value_ln.1', sparse=FALSE)
colnames(full) <- paste(colnames(full), "." ,"2002", sep = "")
row.names(full) <- paste(row.names(full), "." ,"2002", sep = "")

for(i in 2003:2012){
  
  fdi_yr <- subset(fdi, fdi$Year ==i)
  yr <- toString(i)
  fdi_graph <- graph.data.frame(fdi_yr)
  full_add <- get.adjacency(fdi_graph,attr='Value_ln.1', sparse=FALSE)
  colnames(full_add) <- paste(colnames(full_add), "." , yr, sep = "")
  row.names(full_add) <- paste(row.names(full_add), "." ,yr, sep = "")
  
  full <- adiag(full, full_add)
}
write.csv(full, file = "fdi_lag.csv") 

#  mass
fdi_graph <- graph.data.frame(fdi02)
full <- get.adjacency(fdi_graph,attr='mass', sparse=FALSE)
colnames(full) <- paste(colnames(full), "." ,"2002", sep = "")
row.names(full) <- paste(row.names(full), "." ,"2002", sep = "")

for(i in 2003:2012){
  
  fdi_yr <- subset(fdi, fdi$Year ==i)
  yr <- toString(i)
  fdi_graph <- graph.data.frame(fdi_yr)
  full_add <- get.adjacency(fdi_graph,attr='mass', sparse=FALSE)
  colnames(full_add) <- paste(colnames(full_add), "." , yr, sep = "")
  row.names(full_add) <- paste(row.names(full_add), "." ,yr, sep = "")
  
  full <- adiag(full, full_add)
}
write.csv(full, file = "mass.csv") 

#  distance
fdi_graph <- graph.data.frame(fdi02)
full <- get.adjacency(fdi_graph,attr='dist', sparse=FALSE)
colnames(full) <- paste(colnames(full), "." ,"2002", sep = "")
row.names(full) <- paste(row.names(full), "." ,"2002", sep = "")

for(i in 2003:2012){
  
  fdi_yr <- subset(fdi, fdi$Year ==i)
  yr <- toString(i)
  fdi_graph <- graph.data.frame(fdi_yr)
  full_add <- get.adjacency(fdi_graph,attr='dist', sparse=FALSE)
  colnames(full_add) <- paste(colnames(full_add), "." , yr, sep = "")
  row.names(full_add) <- paste(row.names(full_add), "." ,yr, sep = "")
  
  full <- adiag(full, full_add)
}
write.csv(full, file = "dist.csv") 




#stack node level covariates   ###########################################################################

#Polity

vertex <- summaryBy(Origin.GDP+Origin.polity+Origin.TO+Origin.pop+Origin.GDPg+ 
                           Origin.GDPpc+Origin.pv ~ Origin, data=fdi02)
names(vertex) <- c("name","GDP", "Polity", "TradeOpen", "Pop", "GDP.g","GDPpc", "PV")
vertex_attr <- data.frame(vertex[,3])
row.names(vertex_attr) <- paste(vertex[,1], "." ,"2002", sep = "")
 
for(i in 2003:2012){
  fdi_yr <- subset(fdi, fdi$Year ==i)
  yr <- toString(i)
  vertex <- summaryBy(Origin.GDP+Origin.polity+Origin.TO+Origin.pop+Origin.GDPg+ 
                        Origin.GDPpc+Origin.pv ~ Origin, data=fdi02)
  names(vertex) <- c("name","GDP", "Polity", "TradeOpen", "Pop", "GDP.g","GDPpc", "PV")
  vertex_add <- data.frame(vertex[,3])
  row.names(vertex_add) <- paste(vertex[,1], "." ,yr, sep = "")
  vertex_attr  <- rbind(vertex_attr, vertex_add)
}
write.csv(vertex_attr, file = "polity.csv") 

#Trade Openness
vertex <- summaryBy(Origin.GDP+Origin.polity+Origin.TO+Origin.pop+Origin.GDPg+ 
                      Origin.GDPpc+Origin.pv ~ Origin, data=fdi02)
names(vertex) <- c("name","GDP", "Polity", "TradeOpen", "Pop", "GDP.g","GDPpc", "PV")
vertex_attr <- data.frame(vertex[,4])
row.names(vertex_attr) <- paste(vertex[,1], "." ,"2002", sep = "")

for(i in 2003:2012){
  fdi_yr <- subset(fdi, fdi$Year ==i)
  yr <- toString(i)
  vertex <- summaryBy(Origin.GDP+Origin.polity+Origin.TO+Origin.pop+Origin.GDPg+ 
                        Origin.GDPpc+Origin.pv ~ Origin, data=fdi02)
  names(vertex) <- c("name","GDP", "Polity", "TradeOpen", "Pop", "GDP.g","GDPpc", "PV")
  vertex_add <- data.frame(vertex[,4])
  row.names(vertex_add) <- paste(vertex[,1], "." ,yr, sep = "")
  vertex_attr  <- rbind(vertex_attr, vertex_add)
}
write.csv(vertex_attr, file = "trade_openness.csv") 

