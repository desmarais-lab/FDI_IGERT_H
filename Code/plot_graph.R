#=============================================================#
# John Schoeneman
# Work Done For: FDI Network Analysis RA-IGERT
# Date: Fall 2016
# Work Done: Perform ERGM Count analysis
# Machine: MacPro OSX Yosemite
#=============================================================#


set.seed(19)

# libraries
library(network)
library(igraph)



#load in data
fdi <- read.csv("./Code/sub_stock.csv", stringsAsFactors=FALSE)        #FDI
fdi <- fdi[,-1]

# Important to note that the igraph function below is expecting an edgelist, which means it will record an edge between every pair of vertices if given a dyadic dataset

years <- unique(fdi$Year)
set.seed(5)
for(yr in years){

# plot 2002
fdi02 <- subset(fdi, fdi$Year==yr)

# Create an edgelist and vertex-level dataset
uniqueStates <- sort(unique(fdi02$Origin))

originPolity <- fdi02$Origin.polity[match(uniqueStates,fdi02$Origin)]

vertexData <- data.frame(uniqueStates,originPolity)

# re-arrange sender-receiver
fdi02[,c(1,2)] <- fdi02[,c(2,1)]

# remove non-edges
fdi02 <- fdi02[-which(fdi02$Value <= 0),]

fdi_net <- graph_from_data_frame(d=fdi02,directed=T,vertices=vertexData) 

# function to remove isolates
delete.isolates <- function(graph, mode = 'all') {
  require(igraph)
  isolates <- which(degree(graph, mode = mode) == 0)
  delete.vertices(graph, isolates)
}

fdi_netNoIsolates <- delete.isolates(fdi_net)

coordinates <- layout_with_fr(fdi_netNoIsolates,weights=get.edge.attribute(fdi_net,"Value"))

library(colorRamps)

colorSequence <- blue2red(length(-10:10))

vertex.colors <- colorSequence[get.vertex.attribute(fdi_netNoIsolates,"originPolity")+11]

fileName <- paste("./networkPlots/fdiNet",yr,".pdf",sep="")
pdf(file=fileName,height=6,width=6)
plot(fdi_netNoIsolates, edge.arrow.size=.25, edge.color=rgb(.2,.2,.2,.1), vertex.color=vertex.colors, vertex.size=4,vertex.label=NA,layout=coordinates)
dev.off()

}

# Hadleigh says this network looks like a chicken stuck in a spider web

