# clear workspace, setting random seed, and setting work directory
rm(list=ls())
set.seed(19)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load packages
library(network)
library(igraph)
library(DataCombine)
library(doBy)
#data
load("amelia_fit.Rdata")



#loop through imputations
for(i in 6:10){
df <- amelia_fit$imputations[[i]]
df$Value <- ifelse(df$Value<1, 0, df$Value)
df$tValue <-  ifelse(df$Value>1, ceiling(log(df$Value)*2), 0)
df <- df[order(df$dyadid),]
df <- slide(df, Var = "Value", GroupVar = "dyadid", slideBy = -1)

 
   # Create an empty list in which to store the networks
  netlist <- list()
  covlist <- list()
  years <- 2002:2012
  
  for(y in 1:11){
    #subset by year
    fdi_yr <- subset(df, df$Year == years[y])
    #turn into graph object
    fdi_graph <- graph.data.frame(fdi_yr)
    #extract adjacency matrix
    adj <- get.adjacency(fdi_graph,attr='tValue', sparse=FALSE)
    full <- as.network(adj)
    #loop and add to list
    netlist[[y]] <- full
    
    # add all the vertex attributes to the networks
    vertex <- summaryBy(Origin.polity+Origin.TO+Origin.GDPpc +Origin.oecd~ Origin, data=fdi_yr)
    names(vertex) <- c("name","Polity","TradeOpen", "GDPpc", "OECD_mem")
    
    netlist[[y]] %e% "tValue" <- adj
    netlist[[y]] %v% "polity" <- vertex$Polity
    netlist[[y]] %v% "trade_opennes" <- vertex$TradeOpen
    netlist[[y]] %v% "gdp.pc" <- vertex$GDPpc
    netlist[[y]] %v% "OECD_mem" <- vertex$OECD_mem
  }

  
  #lag FDI Stock, mass, and distance
  for(y in 1:11){
    #subset by year
    fdi_yr <- subset(df, df$Year == years[y])
    #turn into graph object
    fdi_graph <- graph.data.frame(fdi_yr)
    #extract adjacency matrix
    lag <- get.adjacency(fdi_graph,attr='Value-1', sparse=FALSE)
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
    covlist[[y]] <- covlist_yr
  }
  

save(netlist, file = paste0("imputed_dfs/MIdf", i, "_net.RData"))
save(covlist, file = paste0("imputed_dfs/MIdf", i, "_cov.RData"))
print(i)
}


 
 