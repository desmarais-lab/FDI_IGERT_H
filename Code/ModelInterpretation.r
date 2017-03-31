# Code to plot BIC of model with and without network effects

library(ergm)


#interpretFunction <- function(

# Get file names for ERGM results
modelFiles <- dir("./Code/models_tweight")
modelFiles <- modelFiles[which(grepl("rda",modelFiles))]

# Matrix to store results
BICMat <- matrix(0,length(modelFiles)/2,2)

# Row labels for fit matrix

cols <- 1+grepl("model2",modelFiles)


rowLabels <- sort(unique(substr(modelFiles,6+cols,7+cols)))
rownames(BICMat) <- rowLabels
colnames(BICMat) <- c("Independent","Network")

i = length(modelFiles)

# Create environment in which to load results
fiti <- new.env()

# load the fit in the ith model file
load(paste("./Code/models_tweight/",modelFiles[i],sep=""))

# Identify whether there are network effects based on file name
col <- 1+grepl("model2",modelFiles[i])

# Extract the year from the file name
yri <- substr(modelFiles,6+col,7+col)[i]

# libraries
library(ergm.count)
library(network)
library(igraph)
library(doBy)
library(plyr)

#setwd("/Users/johnpschoeneman/Desktop/ACI/Count")

#load in data
fdi <- read.csv("./Code/sub_stock.csv", stringsAsFactors=FALSE)        #FDI
fdi <- fdi[,-1]
#125 countries, 12 years (2001-2012),
fdi <- fdi[,c(2,1,3:44)]
#extract one year
fdi02 <- subset(fdi, fdi$Year ==2012)

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

#scale continuous variables
vars <- c(18:34,36, 38:39, 41:44)
for(i in vars){
    
    fdi02[,i] <- range01(fdi02[,i])
    
}






pdf("./Draft/draft_figures/BICdiff.pdf",height=4,width=6.5)
par(las=1)
barplot(BICMat[,1]-BICMat[,2],ylab="BIC Difference (Ind-Net)",xlab="Year")
dev.off()

fdi02 <- subset(fdi, fdi$Year ==2012)

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

#scale continuous variables
vars <- c(18:34,36, 38:39, 41:44)
for(i in vars){
    
    fdi02[,i] <- range01(fdi02[,i])
    
}


###

fdi02Base <- fdi02

#create vertex dataset
vertex_attr <- summaryBy(Origin.GDP+Origin.polity+Origin.TO+Origin.pop+Origin.GDPg+
Origin.GDPpc+Origin.pv ~ Origin, data=fdi02)
#vertex attr: "Origin.GDP","Origin.polity","Origin.TO", "Origin.pop",  "Origin.GDP.g",　Origin.pv"
#rename vertex dataset
names(vertex_attr) <- c("name","GDP", "Polity", "TradeOpen", "Pop", "GDP.g","GDPpc", "PV")

#create network object
detach("package:igraph", unload=TRUE)
fdi_net <- network(fdi02, matrix.type="edgelist", directed=TRUE)

#set edge attributes
set.edge.attribute(fdi_net, attrname="Value_ln", value=fdi02$Value_ln)
set.edge.attribute(fdi_net, attrname="distance", value=fdi02$dist)
set.edge.attribute(fdi_net, attrname="contig", value=fdi02$contig)
set.edge.attribute(fdi_net, attrname="colony", value=fdi02$colony)
set.edge.attribute(fdi_net, attrname="lang_ethno", value=fdi02$comlang_ethno)
set.edge.attribute(fdi_net, attrname="defence_t", value=fdi02$defense.max.x)
set.edge.attribute(fdi_net, attrname="nonagg_t", value=fdi02$nonaggression.max.x)
set.edge.attribute(fdi_net, attrname="neut_t", value=fdi02$neutrality.max.x)
set.edge.attribute(fdi_net, attrname="entente_t", value=fdi02$entente.max.x)
set.edge.attribute(fdi_net, attrname="depth", value=fdi02$depth_latent)
set.edge.attribute(fdi_net, attrname="trade_int", value=fdi02$trade_int)
set.edge.attribute(fdi_net, attrname="mass", value=fdi02$mass)
set.edge.attribute(fdi_net, attrname="lag_stock", value=fdi02$Value_ln.1)

#set vertex attributes
set.vertex.attribute(fdi_net, attrname="GDP", value=vertex_attr$GDP)
set.vertex.attribute(fdi_net, attrname="Polity", value=vertex_attr$Polity)
set.vertex.attribute(fdi_net, attrname="TradeOpen", value=vertex_attr$TradeOpen)
set.vertex.attribute(fdi_net, attrname="GDPpc", value=vertex_attr$GDPpc)
set.vertex.attribute(fdi_net, attrname="GDP.g", value=vertex_attr$GDP.g)
set.vertex.attribute(fdi_net, attrname="PV", value=vertex_attr$PV)

# fixallbut(free.dyads)

# Find an FDI value that is 6
focalEdge <- which(fdi02$Value_ln==6)[1]
fdi02Base[focalEdge,]

FreeDyads <- fdi_net$mel[focalEdge]

simNets <- simulate(fit.01.2,nsim=10,constraints=~fixallbut(as.edgelist(t(c(3,30)),n=length(network.vertex.names(fdi_net)))))










