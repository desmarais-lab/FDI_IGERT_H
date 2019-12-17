# clear workspace, setting random seed, and setting work directory
rm(list=ls())
set.seed(19)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load packages
library(network)
library(igraph)


#data
fdi <- read.csv("amelia_fitsubset_stock.csv", stringsAsFactors=FALSE)[,-1]        #FDI



#loop through subsets
for(q in data_file){
  
  #load subset list and subset data
  q_list <- read.csv(paste0("../Omit_Missing_Values/q_", q ,".csv"), stringsAsFactors=FALSE)
  df <- subset(fdi, fdi$Origin %in% q_list[,2])
  df <- subset(df, df$Destination %in% q_list[,2])

  
  #set id variables for ameilia
  ids <-  c("Origin","Destination")
  #impute missing values
  amelia_fit <- amelia(df, m =10, idvars =ids,
                         ts = "Year", cs = c("dyadid"), polytime = 1)
  
netlist_m <- list()
covlist_m <- list()
for(j in 1:10){
 df <- amelia_fit$imputations[[j]]
 df$Value_ln <-  log(ifelse(df$Value<0, 1, df$Value+1))
 df$Value_ln  <- round(df$Value_ln, digits = 0)
 df <- slide(df, Var = "Value_ln", GroupVar = "dyadid", slideBy = -1)

 
   # Create an empty list in which to store the networks
  netlist <- list()
  covlist <- list()
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
  netlist_m[j] <- netlist
  
  #lag FDI Stock, mass, and distance
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
  
  covlist_m[j] <- covlist
}

save(netlist_m, file = paste0("q", q, "_net.RData"))
save(covlist_m, file = paste0("q", q, "_cov.RData"))
}


 
 