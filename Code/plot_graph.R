#=============================================================#
# John Schoeneman
# Work Done For: FDI Network Analysis RA-IGERT
# Date: Fall 2016
# Work Done: Perform ERGM Count analysis
# Machine: MacPro OSX Yosemite
#=============================================================#


# clear workspace
rm(list=ls())

set.seed(19)

# libraries
library(network)
library(igraph)



#load in data
fdi <- read.csv("sub_stock.csv", stringsAsFactors=FALSE)        #FDI
fdi <- fdi[,-1]


# plot 2002
fdi02 <- subset(fdi, fdi$Year==2002)
fdi_net <- graph_from_data_frame(d=fdi02,directed=T) 
plot(fdi_net, edge.arrow.size=.01, edge.color="gray",edge.width=fdi_net$Value_ln/50,
     vertex.color="orange", vertex.size=3,vertex.label.cex=.5,
     vertex.label.color="black")

# plot 2012


