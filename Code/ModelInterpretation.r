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


fdi02 <- subset(fdi, fdi$Year ==2012)

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

#scale continuous variables
vars <- c(18:34,36, 38:39, 41:44)
for(i in vars){
    
    fdi02[,i] <- range01(fdi02[,i])
    
}

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

simNum <- 1000

set.seed(5)
system.time(simNets <- simulate(fit.01.2,nsim=simNum,control=control.simulate.ergm(MCMC.interval=10000)))
save(list="simNets",file="./Code/simulatedNetworks.RData")

nodes <- network.size(fdi_net)

amatArray <- array(0,dim=c(nodes,nodes,simNum))

for(i in 1:simNum){
    amat0 <- matrix(0,nodes,nodes)
    indexMatrix <- cbind(unlist(do.call('rbind',simNets[[i]]$mel)[,c(2)]),unlist(do.call('rbind',simNets[[i]]$mel)[,c(1)]))
    amat0[indexMatrix] <- get.edge.attribute(simNets[[i]],"Value_ln")
    amatArray[,,i] <- amat0
}

set.vertex.attribute(fdi_net,"nodeid",1:nodes)


MPLEformula <- fdi_net ~ edgecov(fdi_net,
"lag_stock") + edgecov(fdi_net, "mass") +
edgecov(fdi_net, "distance") + edgecov(fdi_net,
"contig") + edgecov(fdi_net, "colony") +
edgecov(fdi_net, "lang_ethno") + edgecov(fdi_net,
"defence_t") + edgecov(fdi_net, "nonagg_t") + edgecov(fdi_net, "neut_t") +
edgecov(fdi_net, "entente_t") + edgecov(fdi_net,
"depth") + nodeocov("Polity") +
nodeocov("TradeOpen") + nodeocov("GDP.g") +
nodeocov("PV") + nodeocov("GDPpc") +
nodeicov("Polity") + nodeicov("TradeOpen") + nodeicov("GDP.g") + nodeicov("PV") + nodeicov("GDPpc") + nodeocov("nodeid")+nodeicov("nodeid")

MPLEData <- ergmMPLE(MPLEformula)

amatMean <- matrix(0,nodes,nodes)

for(i in 1:nodes){
    for(j in 1:nodes){
        amatMean[i,j] <- mean(amatArray[i,j,])
    }
}

covariates <- MPLEData$predictor

nodeInd <- covariates[,c(ncol(covariates)-1,ncol(covariates))]
predictors <- covariates[,-c(ncol(covariates)-1,ncol(covariates))]
covariateCoef <- coef(fit.01.2)[-c(1:5)]

predictorMat <- matrix(0,nodes,nodes)
linearPredictor <- predictors%*%cbind(covariateCoef)
predictorMat[nodeInd] <- linearPredictor

diag(amatMean) <- NA

meanPredict <- lm(c(amatMean)~c(predictorMat))

# Covariate-predicted Adjacency matrix
predictedAmat <- coef(meanPredict)[1]+coef(meanPredict)[2]*predictorMat

deviationArray <- array(0,dim=c(nodes,nodes,simNum))
transposedArray <- array(0,dim=c(nodes,nodes,simNum))

for(i in 1:simNum){
    deviationMat <- amatArray[,,i]
    diag(deviationMat) <- NA
    deviationMat <- deviationMat - predictedAmat
    deviationArray[,,i] <- deviationMat
    transposedArray[,,i] <- t(amatArray[,,i])
}

boxplot(c(deviationArray)~c(transposedArray))


pdf("./Draft/draft_figures/mutualBoxplot.pdf",height=4,width=8,pointsize=11)
par(las=1)
boxplot(c(deviationArray)~c(transposedArray),xlab="Mutual Edge Value",ylab="Residual",subset=which(c(transposedArray) <= 20),outline=F)
dev.off()




### Difference from the covariate-predicted mean and the edge value




