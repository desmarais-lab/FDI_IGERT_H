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



#no_cores <- detectCores()
#registerDoMC(no_cores)  #change the 2 to your number of CPU cores  
setwd("/Users/johnpschoeneman/Documents/school/Penn State/RA:TA/FDI_IGERT_H/Code")


#load in data
fdi <- read.csv("fdi_sub.csv", stringsAsFactors=FALSE)        #FDI


fdi <- fdi[,-1]



#Create GDP pc by origin
fdi$Origin.GDPpc <- fdi$Origin.GDP/fdi$Origin.pop
gdppc <- summaryBy(Origin.GDPpc ~ Origin + Year, data=fdi)




#log FDI
fdi$Value_ln <- round(log(ifelse(fdi$Value<0, 0, fdi$Value)+1)) 


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


#descriptive stats by OECD or not#######################
# load OECD list
oecd <- read.csv("OECD_list.csv", stringsAsFactors=FALSE)        #FDI
oecd <- oecd[,1:2]
#Split by OECD

#Merge on dest and year, equal to one, sort, carryforward, turn to zeros

#Repeat for Origin

#Split by OECD status



