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

#setwd("/Users/johnpschoeneman/Desktop/ACI/Count")

#load in data
fdi <- read.csv("sub_stock.csv", stringsAsFactors=FALSE)        #FDI
fdi <- fdi[,-1]

# create new variable transformations
fdi$trade_ln <- log(fdi$trade_int+fdi$trade_hco+fdi$trade_cap+fdi$trade_mix+1) 
fdi$dyad <- paste(fdi$Destination, fdi$Origin, sep = "")
fdi$mass <- (log(fdi$Dest.GDP*fdi$Origin.GDP))
fdi$dist_ln <- log(fdi$dist)
fdi$Value_ln <- round(log(ifelse(fdi$Value<0, 0, fdi$Value)+1)) 
fdi$Dest.pop_ln <- log(fdi$Dest.pop)
fdi$Origin.pop_ln <- log(fdi$Origin.pop)

#lag DV and GDP (to create growth rates)
fdi <- slide(fdi, Var = "Dest.GDP", GroupVar = "dyad", slideBy = -1)
fdi <- slide(fdi, Var = "Origin.GDP", GroupVar = "dyad", slideBy = -1)
fdi <- slide(fdi, Var = "Value_ln", GroupVar = "dyad", slideBy = -1)

#Create GDP pc (logged) and growth rate
fdi$Origin.GDPpc_ln <- log(fdi$Origin.GDP/fdi$Origin.pop)
fdi$Dest.GDPpc_ln <- log(fdi$Dest.GDP/fdi$Dest.pop)
fdi$Origin.GDPg <- (fdi$Origin.GDP-fdi$`Origin.GDP-1`)/fdi$`Origin.GDP-1`
fdi$Dest.GDPg <- (fdi$Dest.GDP-fdi$`Dest.GDP-1`)/fdi$`Dest.GDP-1`





#plot dependent variable distribution
fdi$Value_h <- ifelse(fdi$Value < 0, 0, fdi$Value)
fdi_col <- subset(fdi, fdi$Value_ln == 0)
fdi_h <- subset(fdi, fdi$Value_ln != 0)
fdi_col$Value  <- 1
fdi_col <- summaryBy(Value~Year, data = fdi_col, FUN=sum)
fdi_col$prop <- fdi_col$Value.sum/15500
#hist_1 <- hist(fdi$Value_h)
#hist_1$counts <- hist_1$counts/12
#plot(hist_1+1, col="lightgray", main = "Distribution of FDI Flows", ylim = c(0,600),
#     xlab="Value, logged (Excludes Zero Values, ~85% of obs.)", ylab = "Per Year")
par(mar = c(5,5,5,5))
boxplot(fdi_h$Value_h~fdi_h$Year, log = "y",col="lightgray", main = "Non-Zero FDI Stocks",yaxt='n',
        xlab="Year", ylab = "FDI Stock (in mil USD)",outline=TRUE, cex=0.5, pch = 16, par(las=1))
axis(2,at=c(1, 100, 10000),labels=c("1","100","10K"))
#mtext("Removed Zeroes from Box-Plots")
par(new = T)
plot(fdi_col$prop, axes=F, xlab=NA, ylab=NA, type="l", col = "#D55E00", pch = 16)
axis(side = 4, col = "#D55E00", col.axis ="#D55E00",col.ticks="#D55E00")
par(las=0)
mtext(side = 4, line = 3, 'Proportion of Zeroes', col ="#D55E00")

rm(fdi_col, fdi_h, i, vars)

#125 countries, 12 years (2001-2012),

range_0to1 <- function(x){(x-min(x))/(max(x)-min(x))}
fdi <- na.omit(fdi)
#scale continuous variables
vars <- c(18:33,35, 37:49)
for(i in vars){
  
  fdi[,i] <- range_0to1(fdi[,i]) 
  
}

# create alliance dummy
fdi$alliance <- (fdi$nonaggression.max.x + fdi$entente.max.x + fdi$neutrality.max.x)
fdi$alliance <- as.numeric(ifelse(fdi$alliance >0, 1, 0))
#make bit  and defense numeric
fdi$bit_dummy <- as.numeric(fdi$bit_dummy)
fdi$defense.max.x <- as.numeric(fdi$defense.max.x)
#subset by year, get adjacency matrix, and create list of DV matrices #########################################


#FDI Stock

# Create an empty list in which to store the networks
netlist <- list()
years <- 2002:2012

for(i in 1:11){
#subset by year
fdi_yr <- subset(fdi, fdi$Year == years[11])
#turn into graph object
fdi_graph <- graph.data.frame(fdi_yr)
#extract adjacency matrix
adj <- get.adjacency(fdi_graph,attr='Value_ln', sparse=FALSE)
full <- as.network(adj)
#loop and add to list
netlist[[i]] <- full

# add all the vertex attributes to the networks
vertex <- summaryBy(Origin.GDP+Origin.polity+Origin.TO+Origin.pop_ln +Origin.GDPg+ 
                      Origin.GDPpc_ln+Origin.pv ~ Origin, data=fdi_yr)
names(vertex) <- c("name","GDP", "Polity", "TradeOpen", "Pop", "GDP.g","GDPpc", "PV")

netlist[[i]] %e% "Value_ln" <- adj
netlist[[i]] %v% "polity" <- vertex$Polity
netlist[[i]] %v% "trade_opennes" <- vertex$TradeOpen
netlist[[i]] %v% "pop" <- vertex$Pop
netlist[[i]] %v% "gdp.pc" <- vertex$GDPpc

}
save(netlist, file = "fdi_net.Rdata")

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


#Clean out unneeded data
rm(dist, fdi, fdi_yr, full, lag, mass, vertex, covlist_yr, fdi_graph, i, vars, years)
#SAVE as Rdata
save(covlist, file = "fdi_cov.Rdata")


