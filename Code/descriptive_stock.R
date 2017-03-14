#=============================================================#
# John Schoeneman
# Work Done For: FDI Network Analysis RA-IGERT
# Date: Spring 2017
# Work Done: Descriptive Statistics
# Machine: MacPro OSX Yosemite
#=============================================================#


# clear workspace
rm(list=ls())

set.seed(19)

# libraries
library(doBy)
library(foreach)
library(doMC)
library(stargazer)
library(RCurl)
library(plyr)
library(car)
library(MASS)
library(igraph)
library(DataCombine)


#no_cores <- detectCores()
#registerDoMC(no_cores)  #change the 2 to your number of CPU cores  
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load in data
fdi <- read.csv("sub_stock.csv", stringsAsFactors=FALSE)        #FDI
fdi <- fdi[,-1]
#201 countries, 12 years (2000-2012),





# create new variable transformations
fdi$Dest.GDP <- fdi$Dest.GDP/1000000 # scale to millions
fdi$Origin.GDP <- fdi$Origin.GDP/1000000
fdi$trade_ln <- log(fdi$trade_int+1) 
fdi$dyad <- paste(fdi$Destination, fdi$Origin, sep = "")
fdi$mass <- (log(fdi$Dest.GDP*fdi$Origin.GDP))
fdi$dist <- log(fdi$dist)
fdi$Value_ln <- round(log(ifelse(fdi$Value<0, 0, fdi$Value)+1)) 
fdi <- slide(fdi, Var = "Dest.GDP", GroupVar = "dyad", slideBy = -1)
fdi <- slide(fdi, Var = "Origin.GDP", GroupVar = "dyad", slideBy = -1)

#Create GDP pc and growth rate
fdi$Origin.GDPpc <- fdi$Origin.GDP/fdi$Origin.pop*1000000
fdi$Dest.GDPpc <- fdi$Dest.GDP/fdi$Dest.pop*1000000
fdi$Origin.GDPg <- (fdi$Origin.GDP-fdi$`Origin.GDP-1`)/fdi$`Origin.GDP-1`
fdi$Dest.GDPg <- (fdi$Dest.GDP-fdi$`Dest.GDP-1`)/fdi$`Dest.GDP-1`

# rho as reciprocity
rho <- function(g){
  g2 <- g
  diag(g2) <- NA
  g_mean <- mean(g2, na.rm=T)
  num <- sum((g-g_mean)*t(g-g_mean))
  denom <- sum((g-g_mean)^2)
  stat <- num/denom
  stat
}

#descriptive stats by whole network#######################

descriptive_stats <- matrix(nrow=12, ncol=6)

for(i in 1:12){
  
  descriptive_stats[i,1] <- i +2000  
  
  #extract one year
  fdi_yr <- subset(fdi, fdi$Year ==i +2000)
  # create graph object
  fdi_graph <- graph.data.frame(fdi_yr, directed=TRUE, vertices=NULL)
  fdi_y <- get.adjacency(fdi_graph, attr='Value_ln', names=TRUE, sparse=FALSE)
  
  #measure reciprocity
  descriptive_stats[i,2] <- rho(fdi_y)
  
  # Transitivity
  fdi_g <- graph.adjacency(fdi_y, mode="directed")
  t <- transitivity(fdi_g, type="weighted", vids=NULL,isolates="NaN")
  t <- na.omit(t)
  descriptive_stats[i,3] <- mean(t)
  descriptive_stats[i,4] <- max(t)
  descriptive_stats[i,5] <- min(t)
  
  # assortativity by Polity
  library(doBy)
  agg<-summaryBy(Dest.polity~ Destination, fdi_yr)
  # Attach attribute Polity to each vertex
  fdi_g<-set_vertex_attr(fdi_g,"Polity",value=agg$Dest.polity.mean)
  list.vertex.attributes(fdi_g)
  # Calculate the assortativity coefficient for polity
  descriptive_stats[i,6] <- assortativity(fdi_g,types1=V(fdi_g)$Polity,directed=T)
  
}


# plot network stats
library(plotly)
m <- list(
  l = 50,
  r = 50,
  b = 50,
  t = 50,
  pad = 1
)

data <- data.frame(descriptive_stats)

plot_ly(data, x = ~X1, y = ~X2, type = 'scatter', mode = 'lines') %>%
  layout(title = "FDI Network Reciprocity",
         xaxis = list(title = "Year"),
         yaxis = list (title = "Reciprocity"),
         margin = m)

plot_ly(data, x = ~X1, y = ~X3, type = 'scatter', mode = 'lines') %>%
  layout(title = "FDI Network Transitivity",
         xaxis = list(title = "Year"),
         yaxis = list (title = "Transitivity"),
         margin = m)

plot_ly(data, x = ~X1, y = ~X6, type = 'scatter', mode = 'lines') %>%
  layout(title = "Assortativity for Polity",
         xaxis = list(title = "Year"),
         yaxis = list (title = "Assortativity"),
         margin = m)

# plot network
plot.igraph(fdi_g,vertex.label=V(fdi_g)$name,layout=layout.fruchterman.reingold, 
            edge.color="light grey",edge.width=((E(fdi_g)$ln_Value)/100), edge.arrow.size=.2,
            vertex.size=3, vertex.label.cex=.4, vertex.label.color="black", 
            vertex.color="yellow")


#descriptive of covariates#######################
agg<-summaryBy(Dest.GDP + Dest.polity+ Dest.TO+Dest.GDP.g + Dest.pv ~Destination+Year, fdi)


# summary stats
summary(fdi)
stargazer(fdi)

# distribution plots
par(mfrow=c(3,3))
plot(density(log(agg$Dest.GDP.mean+1)))
plot(density(agg$Dest.polity.mean))
plot(density(agg$Dest.TO.mean))
plot(density(agg$Dest.GDP.g.mean))
plot(density(agg$Dest.pv.mean))
plot(density(fdi$Value))
plot(density(fdi$ln_Value))
plot(density(log(fdi$trade_int+1)))
plot(density(fdi$depth_latent))

# scatter plots

par(mfrow=c(2,2))
plot(fdi$Dest.polity, log(fdi$Value+1))
abline(lm(fdi$Dest.polity~log(fdi$trade_int+1)), col="red") # regression line (y~x) 
lines(lowess(fdi$Dest.polity,log(fdi$Value+1)), col="blue") # lowess line (x,y)

plot(fdi$Dest.TO, log(fdi$Value+1))
abline(lm(fdi$Dest.TO~log(fdi$trade_int+1)), col="red") # regression line (y~x) 
lines(lowess(fdi$Dest.TO,log(fdi$Value+1)), col="blue") # lowess line (x,y)

plot(log(fdi$trade_int+1), log(fdi$Value+1))
abline(lm(log(fdi$Value+1)~log(fdi$trade_int+1)), col="red") # regression line (y~x) 
lines(lowess(log(fdi$trade_int+1),log(fdi$Value+1)), col="blue") # lowess line (x,y)

plot(fdi$depth_latent, log(fdi$Value+1))
abline(lm(log(fdi$Value+1)~fdi$depth_latent), col="red") # regression line (y~x) 
lines(lowess(fdi$depth_latent,log(fdi$Value+1)), col="blue") # lowess line (x,y)


#Split by low income, middle income, and high income#######################

# low-income: $1,025 or less in 2015: conversion to 2005 dollars for GDP: .82
descriptive_stats <- matrix(nrow=12, ncol=6)

for(i in 1:12){
  
  descriptive_stats[i,1] <- i +2000  
  
  #extract one year and drop by income
  fdi_yr <- subset(fdi, fdi$Year ==i +2000)
  fdi_yr <- subset(fdi_yr, fdi_yr$Dest.GDPpc<(1025*.82) & fdi_yr$Origin.GDPpc<(1025*.82))
  # create graph object
  fdi_graph <- graph.data.frame(fdi_yr, directed=TRUE, vertices=NULL)
  fdi_y <- get.adjacency(fdi_graph, attr='Value_ln', names=TRUE, sparse=FALSE)
  
  #measure reciprocity
  descriptive_stats[i,2] <- rho(fdi_y)
  
  # Transitivity
  fdi_g <- graph.adjacency(fdi_y, mode="directed")
  t <- transitivity(fdi_g, type="weighted", vids=NULL,isolates="NaN")
  t <- na.omit(t)
  descriptive_stats[i,3] <- mean(t)
  descriptive_stats[i,4] <- max(t)
  descriptive_stats[i,5] <- min(t)
  
  # assortativity by Polity
  library(doBy)
  agg<-summaryBy(Dest.polity~ Destination, fdi_yr)
  # Attach attribute Polity to each vertex
  fdi_g<-set_vertex_attr(fdi_g,"Polity",value=agg$Dest.polity.mean)
  list.vertex.attributes(fdi_g)
  # Calculate the assortativity coefficient for polity
  descriptive_stats[i,6] <- assortativity(fdi_g,types1=V(fdi_g)$Polity,directed=T)
  
}

ds_low <- descriptive_stats

# lower middle-income $1,026 to $4,035 
descriptive_stats <- matrix(nrow=12, ncol=6)

for(i in 1:12){
  
  descriptive_stats[i,1] <- i +2000  
  
  #extract one year
  fdi_yr <- subset(fdi, fdi$Year ==i +2000)
  fdi_yr <- subset(fdi_yr, fdi_yr$Dest.GDPpc<(4036*.82) & fdi_yr$Origin.GDPpc<(4036*.82)&
                     fdi_yr$Dest.GDPpc>(1025*.82) & fdi_yr$Origin.GDPpc>(1025*.82))
  # create graph object
  fdi_graph <- graph.data.frame(fdi_yr, directed=TRUE, vertices=NULL)
  fdi_y <- get.adjacency(fdi_graph, attr='Value_ln', names=TRUE, sparse=FALSE)
  
  #measure reciprocity
  descriptive_stats[i,2] <- rho(fdi_y)
  
  # Transitivity
  fdi_g <- graph.adjacency(fdi_y, mode="directed")
  t <- transitivity(fdi_g, type="weighted", vids=NULL,isolates="NaN")
  t <- na.omit(t)
  descriptive_stats[i,3] <- mean(t)
  descriptive_stats[i,4] <- max(t)
  descriptive_stats[i,5] <- min(t)
  
  # assortativity by Polity
  library(doBy)
  agg<-summaryBy(Dest.polity~ Destination, fdi_yr)
  # Attach attribute Polity to each vertex
  fdi_g<-set_vertex_attr(fdi_g,"Polity",value=agg$Dest.polity.mean)
  list.vertex.attributes(fdi_g)
  # Calculate the assortativity coefficient for polity
  descriptive_stats[i,6] <- assortativity(fdi_g,types1=V(fdi_g)$Polity,directed=T)
  
}

ds_low_m <- descriptive_stats

# upper middle-income  $4,036 to $12,475
descriptive_stats <- matrix(nrow=12, ncol=6)

for(i in 1:12){
  
  descriptive_stats[i,1] <- i +2000  
  
  #extract one year
  fdi_yr <- subset(fdi, fdi$Year ==i +2000)
  fdi_yr <- subset(fdi_yr, fdi_yr$Dest.GDPpc<(12476*.82) & fdi_yr$Origin.GDPpc<(12476*.82)&
                     fdi_yr$Dest.GDPpc>(4035*.82) & fdi_yr$Origin.GDPpc>(4035*.82))
  # create graph object
  fdi_graph <- graph.data.frame(fdi_yr, directed=TRUE, vertices=NULL)
  fdi_y <- get.adjacency(fdi_graph, attr='Value_ln', names=TRUE, sparse=FALSE)
  
  #measure reciprocity
  descriptive_stats[i,2] <- rho(fdi_y)
  
  # Transitivity
  fdi_g <- graph.adjacency(fdi_y, mode="directed")
  t <- transitivity(fdi_g, type="weighted", vids=NULL,isolates="NaN")
  t <- na.omit(t)
  descriptive_stats[i,3] <- mean(t)
  descriptive_stats[i,4] <- max(t)
  descriptive_stats[i,5] <- min(t)
  
  # assortativity by Polity
  library(doBy)
  agg<-summaryBy(Dest.polity~ Destination, fdi_yr)
  # Attach attribute Polity to each vertex
  fdi_g<-set_vertex_attr(fdi_g,"Polity",value=agg$Dest.polity.mean)
  list.vertex.attributes(fdi_g)
  # Calculate the assortativity coefficient for polity
  descriptive_stats[i,6] <- assortativity(fdi_g,types1=V(fdi_g)$Polity,directed=T)
  
}

ds_hi_m <- descriptive_stats
# high-income economies $12,476 or more.

descriptive_stats <- matrix(nrow=12, ncol=6)

for(i in 1:12){
  
  descriptive_stats[i,1] <- i +2000  
  
  #extract one year
  fdi_yr <- subset(fdi, fdi$Year ==i +2000)
  fdi_yr <- subset(fdi_yr, fdi_yr$Dest.GDPpc>(12476*.82) & fdi_yr$Origin.GDPpc>(12476*.82))
  # create graph object
  fdi_graph <- graph.data.frame(fdi_yr, directed=TRUE, vertices=NULL)
  fdi_y <- get.adjacency(fdi_graph, attr='Value_ln', names=TRUE, sparse=FALSE)
  
  #measure reciprocity
  descriptive_stats[i,2] <- rho(fdi_y)
  
  # Transitivity
  fdi_g <- graph.adjacency(fdi_y, mode="directed")
  t <- transitivity(fdi_g, type="weighted", vids=NULL,isolates="NaN")
  t <- na.omit(t)
  descriptive_stats[i,3] <- mean(t)
  descriptive_stats[i,4] <- max(t)
  descriptive_stats[i,5] <- min(t)
  
  # assortativity by Polity
  library(doBy)
  agg<-summaryBy(Dest.polity~ Destination, fdi_yr)
  # Attach attribute Polity to each vertex
  fdi_g<-set_vertex_attr(fdi_g,"Polity",value=agg$Dest.polity.mean)
  list.vertex.attributes(fdi_g)
  # Calculate the assortativity coefficient for polity
  descriptive_stats[i,6] <- assortativity(fdi_g,types1=V(fdi_g)$Polity,directed=T)
  
}
ds_hi <- descriptive_stats



par(mfrow=c(1,1))
par(las=1)
par(mar=c(5,5,4,4))
# Plot Reciprocity
# set up the plot 
plot(c(2001,2012), c(-.25,1), type="n", xlab="Year",
     ylab="Rho") 

# add lines 
lines(ds_low_m[,1], ds_low_m[,2], type="b", lwd=1.5,lty=1, col=1, pch=1)
lines(ds_low_m[,1], ds_hi_m[,2], type="b", lwd=1.5,lty=2, col=2, pch=1)
lines(ds_low_m[,1], ds_hi[,2], type="b", lwd=1.5,lty=3, col=3, pch=1)

# add a legend 
legend(2001,1, c("Low-Middle", "High-Middle", "High"), bty = "n",
       lty = c(1,2, 3), col= c(1,2,3), merge = TRUE)


# Plot Transitivity
# set up the plot 
plot(c(2001,2012), c(0,1), type="n", xlab="Year",
     ylab="Transitivity") 

# add lines 
lines(ds_low_m[,1], ds_low_m[,3], type="b", lwd=1.5,lty=1, col=1, pch=1)
lines(ds_low_m[,1], ds_hi_m[,3], type="b", lwd=1.5,lty=2, col=2, pch=1)
lines(ds_low_m[,1], ds_hi[,3], type="b", lwd=1.5,lty=3, col=3, pch=1)

# add a legend 
legend(2008,0.2, c("Low-Middle", "High-Middle", "High"), bty = "n",
       lty = c(1,2, 3), col= c(1,2,3), merge = TRUE)


# Plot Assortavity

plot(c(2001,2012), c(-.5,.5), type="n", xlab="Year",
     ylab="Assortavity") 

# add lines 
lines(ds_low_m[,1], ds_low_m[,6], type="b", lwd=1.5,lty=1, col=1, pch=1)
lines(ds_low_m[,1], ds_hi_m[,6], type="b", lwd=1.5,lty=2, col=2, pch=1)
lines(ds_low_m[,1], ds_hi[,6], type="b", lwd=1.5,lty=3, col=3, pch=1)

# add a legend 
legend(2001,.5, c("Low-Middle", "High-Middle", "High"), bty = "n",
       lty = c(1,2, 3), col= c(1,2,3), merge = TRUE)



