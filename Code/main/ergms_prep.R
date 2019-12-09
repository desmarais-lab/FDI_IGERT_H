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
fdi <- read.csv("subset_stock2.csv", stringsAsFactors=FALSE)        #FDI
fdi <- fdi[,-1]
fdi$Value <- ifelse(is.na(fdi$Value),0,fdi$Value)
fdi$Value.1 <- ifelse(is.na(fdi$Value.1),0,fdi$Value.1)





#subset by year, extract adjaceny matrices, and stack as diagonals #########################################
years <- 2002:2012


# Create an empty list in which to store the networks
netlist <- list()

for(i in 1:11){
  #subset by year
  fdi_yr <- subset(fdi, fdi$Year == years[i])
  #turn into graph object
  fdi_graph <- graph.data.frame(fdi_yr)
  #extract adjacency matrix
  adj <- get.adjacency(fdi_graph,attr='Value', sparse=FALSE)
  full <- as.network(adj)
  #loop and add to list
  netlist[[i]] <- full
  
  # add all the vertex attributes to the networks
  vertex <- summaryBy(Origin.polity+Origin.TO+Origin.GDPpc +Origin.oecd~ Origin, data=fdi_yr)
  names(vertex) <- c("name","Polity","TradeOpen", "GDPpc", "OECD_mem")
  
  netlist[[i]] %e% "Value" <- adj
  netlist[[i]] %v% "polity" <- vertex$Polity
  netlist[[i]] %v% "trade_opennes" <- vertex$TradeOpen
  netlist[[i]] %v% "gdp.pc" <- vertex$GDPpc
  netlist[[i]] %v% "OECD_mem" <- vertex$OECD_mem
  
}
save(netlist, file = "main_net.Rdata")

#lag FDI Stock, mass, and distance

covlist <- list()

for(i in 1:11){
  #subset by year
  fdi_yr <- subset(fdi, fdi$Year == years[i])
  #turn into graph object
  fdi_graph <- graph.data.frame(fdi_yr)
  #extract adjacency matrix
  lag <- get.adjacency(fdi_graph,attr='Value.1', sparse=FALSE)
  mass <- get.adjacency(fdi_graph,attr='mass', sparse=FALSE)
  dist <- get.adjacency(fdi_graph,attr='dist', sparse=FALSE)
  alliance <- get.adjacency(fdi_graph,attr='alliance', sparse=FALSE)
  defense <- get.adjacency(fdi_graph,attr='defense.max.x', sparse=FALSE)
  trade_vol <- get.adjacency(fdi_graph,attr='trade_ln', sparse=FALSE)
  bit <- get.adjacency(fdi_graph,attr='bit_dummy', sparse=FALSE)
  OECD_both <- get.adjacency(fdi_graph,attr='both_oecd', sparse=FALSE)
    pta_depth <- get.adjacency(fdi_graph,attr='depth_latent', sparse=FALSE)
  #put covariates into list
  covlist_yr <- list(lag=lag, mass=mass, dist=dist, alliance = alliance, defense= defense,
                     trade_vol = trade_vol, bit = bit, OECD_both=OECD_both, pta_depth=pta_depth)
  #add to main list
  covlist[[i]] <- covlist_yr
}


#SAVE as Rdata
save(covlist, file = "main_cov.Rdata")
