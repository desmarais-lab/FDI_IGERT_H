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


#load in data
fdi <- read.csv("fdi_sub.csv", stringsAsFactors=FALSE)        #FDI
oecd <- read.csv("OECD_list.csv", stringsAsFactors=FALSE)        #FDI
oecd <- oecd[,1:2]

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


#split by OECD



#descriptive stats by whole network#######################
#extract one year
fdi01 <- subset(fdi, fdi$Year ==2001)
fdi01$ln_Value <- log(fdi01$Value+1)


# create graph object
fdi_graph <- graph.data.frame(fdi01)
fdi_y <- get.adjacency(fdi_graph, attr='ln_Value', sparse=FALSE)




# create rho function from class example
r <- function(g){
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


rho_fdi <- rho(fdi_y)
rho_fdi


# Transitivity
fdi_g <- graph.adjacency(fdi_y, mode="directed")
t <- transitivity(fdi_g, type="weighted", vids=NULL,isolates="NaN")
t <- na.omit(t)
mean(t)
range(t)




# assortativity by Polity
library(doBy)
agg<-summaryBy(Dest.polity~ Destination, fdi01)
# Attach attribute Polity to each vertex
fdi_g<-set_vertex_attr(fdi_g,"Polity",value=agg$Dest.polity.mean)
list.vertex.attributes(fdi_g)
# Calculate the assortativity coefficient for polity
a.polity <- assortativity(fdi_g,types1=V(fdi_g)$Polity,directed=T)
a.polity


#descriptive stats by OECD or not#######################
