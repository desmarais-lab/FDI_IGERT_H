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
#setwd("/Users/johnpschoeneman/Documents/school/Penn State/RA:TA/FDI_IGERT_H/Code")


#load in data
fdi <- read.csv("fdi_sub.csv", stringsAsFactors=FALSE)        #FDI


fdi <- fdi[,-1]

fdi$trade_int_sq = fdi$trade_int^2

fdi$dyad <- paste(fdi$Destination, fdi$Origin, sep = "")



#Create GDP pc by origin
fdi$Origin.GDPpc <- fdi$Origin.GDP/fdi$Origin.pop
gdppc <- summaryBy(Origin.GDPpc ~ Origin + Year, data=fdi)

#create GDP pc by Destination
fdi <- merge(fdi, gdppc, by.x = c("Destination", "Year"), by.y = c("Origin", "Year"), all.x =TRUE)
colnames(fdi)[36] <- "Dest.GDPpc"
rm(gdppc)


fdi <- fdi[,c(1,3,2,4:36)]


#log FDI
fdi$ln_Value <- log(fdi$Value+1)

# create rho function
r <- function(g){
  diag(g) <- 0
  stat<- sum(g*t(g))/sum(g)
  stat
  # g is an adjacency matrix
  ## g has no self-loops
  # calculate r
}
# rho as reciprocity
rho <- function(g){
  g2 <- g
  diag(g2) <- NA
  (r(g)-mean(g2, na.rm=T))/(1-mean(g2, na.rm=T))
  # g is an adjacency matrix
  ## g has no self-loops
  # calculate rho
}


#descriptive stats by whole network#######################

descriptive_stats <- matrix(nrow=12, ncol=6)

for(i in 1:12){

descriptive_stats[i,1] <- i +2000  

#extract one year
fdi_yr <- subset(fdi, fdi$Year ==i +2000)
# create graph object
fdi_graph <- graph.data.frame(fdi_yr, directed=TRUE, vertices=NULL)
fdi_y <- get.adjacency(fdi_graph, attr='ln_Value', names=TRUE, sparse=FALSE)

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


# plot stats
library(plotly)

data <- data.frame(descriptive_stats)

plot_ly(data, x = ~X1, y = ~X2, type = 'scatter', mode = 'lines') %>%
layout(title = "FDI Network Reciprocity",
       xaxis = list(title = "Year"),
       yaxis = list (title = "Reciprocity"))

plot_ly(data, x = ~X1, y = ~X3, type = 'scatter', mode = 'lines') %>%
  layout(title = "FDI Network Transitivity",
         xaxis = list(title = "Year"),
         yaxis = list (title = "Transitivity"))

plot_ly(data, x = ~X1, y = ~X6, type = 'scatter', mode = 'lines') %>%
  layout(title = "Assortativity for Polity",
         xaxis = list(title = "Year"),
         yaxis = list (title = "Assortativity"))

#descriptive stats by OECD or not#######################
# load OECD list
oecd <- read.csv("OECD_list.csv", stringsAsFactors=FALSE)        #FDI
oecd <- oecd[,1:2]
#Split by OECD

#Merge on dest and year, equal to one, sort, carryforward, turn to zeros

#Repeat for Origin

#Split by OECD status


#descriptive of covariates#######################

# summary stats

# distribution plots

# scatter plots







