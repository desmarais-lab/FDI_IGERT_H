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
library(DataCombine)

#load in data
fdi <- read.csv("zero_stock.csv", stringsAsFactors=FALSE)        #FDI
fdi <- fdi[,-1]


########## Variable Transformations ##############

#log
fdi$dist <- log(fdi$dist)
fdi$mass <- log(fdi$mass)
fdi$trade_ln <- log(fdi$dyadic_trade+1)
fdi$Origin.GDPpc <- log(fdi$Origin.GDPpc)
fdi$Dest.GDPpc <- log(fdi$Dest.GDPpc)

#function that scales variables
range_0to1 <- function(x){(x-min(x))/(max(x)-min(x))}
#scale continuous variables
for(i in 6:18){
  
  fdi[,i] <- range_0to1(fdi[,i]) 
  
}

# log and round DV, create lag
fdi$Value <- ifelse(fdi$Value<0, 0, fdi$Value)
fdi$Value_ln <- log(fdi$Value+1)
fdi$Value_ln <- round(fdi$Value_ln, digits = 0)
fdi <- slide(fdi, Var = "Value_ln", GroupVar = "dyadid", slideBy = -1)
fdi <- na.omit(fdi)

#subset by q list
q25 <- read.csv("q_50.csv", stringsAsFactors=FALSE)
fdi_q25 <- fdi[fdi$Origin %in% q25[,2],]
fdi <- fdi_q25[fdi_q25$Destination %in% q25[,2],]



#subset by year, extract adjaceny matrices, and stack as diagonals #########################################


# Create an empty list in which to store the networks
netlist <- list()
years <- 2002:2012

for(i in 1:11){
  #subset by year
  fdi_yr <- subset(fdi, fdi$Year == years[i])
  #turn into graph object
  fdi_graph <- graph.data.frame(fdi_yr)
  #extract adjacency matrix
  adj <- get.adjacency(fdi_graph,attr='Value_ln', sparse=FALSE)
  full <- as.network(adj)
  #loop and add to list
  netlist[[i]] <- full
  
  # add all the vertex attributes to the networks
  vertex <- summaryBy(Origin.polity+Origin.TO+Origin.GDPpc ~ Origin, data=fdi_yr)
  names(vertex) <- c("name","Polity","TradeOpen", "GDPpc")
  
  netlist[[i]] %e% "Value_ln" <- adj
  netlist[[i]] %v% "polity" <- vertex$Polity
  netlist[[i]] %v% "trade_opennes" <- vertex$TradeOpen
  netlist[[i]] %v% "gdp.pc" <- vertex$GDPpc
  
}
save(netlist, file = "q50_net.Rdata")

#lag FDI Stock, mass, and distance

covlist <- list()

for(i in 1:11){
  #subset by year
  fdi_yr <- subset(fdi, fdi$Year == years[i])
  #turn into graph object
  fdi_graph <- graph.data.frame(fdi_yr)
  #extract adjacency matrix
  lag <- get.adjacency(fdi_graph,attr='Value_ln-1', sparse=FALSE)
  mass <- get.adjacency(fdi_graph,attr='mass', sparse=FALSE)
  dist <- get.adjacency(fdi_graph,attr='dist', sparse=FALSE)
  alliance <- get.adjacency(fdi_graph,attr='alliance', sparse=FALSE)
  defense <- get.adjacency(fdi_graph,attr='defense.max.x', sparse=FALSE)
  trade_vol <- get.adjacency(fdi_graph,attr='trade_ln', sparse=FALSE)
  bit <- get.adjacency(fdi_graph,attr='bit_dummy', sparse=FALSE)
  #put covariates into list
  covlist_yr <- list(lag=lag, mass=mass, dist=dist, alliance = alliance, 
                     defense= defense, trade_vol = trade_vol, bit = bit)
  #add to main list
  covlist[[i]] <- covlist_yr
}


#SAVE as Rdata
save(covlist, file = "q50_cov.Rdata")
