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
#plot dependent variable distribution
hist_1 <- hist(subset(fdi$Value_ln, fdi$Value_ln!=0))
hist_1$counts <- hist_1$counts/12
plot(hist_1, col="lightgray", main = "Distribution of FDI Flows", ylim = c(0,600),
     xlab="Value, logged (Excludes Zero Values, ~85% of obs.)", ylab = "Per Year")
length(subset(fdi$Value_ln, fdi$Value_ln==0))/186000
#125 countries, 12 years (2001-2012),
fdi <- fdi[,c(2,1,3:44)]

range_0to1 <- function(x){(x-min(x))/(max(x)-min(x))}

#scale continuous variables
vars <- c(18:34,36, 38:39, 41:44)
for(i in vars){
  
  fdi[,i] <- range_0to1(fdi[,i]) 
  
}

#subset by year, get adjacency matrix, and create list of DV matrices #########################################

#FDI Stock

# Create an empty list in which to store the networks
netlist <- list()
years <- 2002:2012

for(i in 1:11){
#subset by year
fdi_yr <- subset(fdi, fdi$Year == years[i])
#turn into graph object
fdi_graph <- graph.data.frame(fdi_yr)
#extract adjacency matrix
full <- as.network(get.adjacency(fdi_graph,attr='Value_ln', sparse=FALSE))
#loop and add to list
netlist[[i]] <- full

# add all the vertex attributes to the networks
vertex <- summaryBy(Origin.GDP+Origin.polity+Origin.TO+Origin.pop+Origin.GDPg+ 
                      Origin.GDPpc+Origin.pv ~ Origin, data=fdi_yr)
names(vertex) <- c("name","GDP", "Polity", "TradeOpen", "Pop", "GDP.g","GDPpc", "PV")


netlist[[i]] %v% "polity" <- vertex$Polity
netlist[[i]] %v% "trade_opennes" <- vertex$TradeOpen


}



#lag FDI Stock, mass, and distance

covlist <- list()

for(i in 1:11){
  #subset by year
  fdi_yr <- subset(fdi, fdi$Year == years[i])
  #turn into graph object
  fdi_graph <- graph.data.frame(fdi_yr)
  #extract adjacency matrix
  lag <- get.adjacency(fdi_graph,attr='Value_ln.1', sparse=FALSE)
  mass <- get.adjacency(fdi_graph,attr='mass', sparse=FALSE)
  dist <- get.adjacency(fdi_graph,attr='dist', sparse=FALSE)
  #put covariates into list
  covlist_yr <- list(lag=lag, mass=mass, dist=dist)
  #add to main list
  covlist[[i]] <- covlist_yr
}


#Clean out unneeded data
rm(dist, fdi, fdi_yr, full, lag, mass, vertex, covlist_yr, fdi_graph, i, vars, years)
#SAVE as Rdata
save(covlist, file = "fdi_cov.Rdata")
save(netlist, file = "fdi_net.Rdata")

