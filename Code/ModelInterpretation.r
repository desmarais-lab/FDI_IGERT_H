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

# Load in independent model
i = 11
# load the fit in the ith model file
load(paste("./Code/models_tweight/",modelFiles[i],sep=""))

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

# set.seed(5)
# system.time(simNets <- simulate(fit.01.2,nsim=simNum,control=control.simulate.ergm(MCMC.interval=10000)))
# save(list="simNets",file="./Code/simulatedNetworks.RData")

# set.seed(5)
# system.time(simNets <- simulate(fit.01.1,nsim=simNum,control=control.simulate.ergm(MCMC.interval=10000)))
# save(list="simNets",file="./Code/simulatedNetworksIndependent.RData")

load("./Code/simulatedNetworks.RData")
simNetsDep <- simNets

load("./Code/simulatedNetworksIndependent.RData")
simNets <- simNets

nodes <- network.size(fdi_net)

### Calculating interpretation quantities for independent model
amatArray <- array(0,dim=c(nodes,nodes,simNum))

for(i in 1:simNum){
    amat0 <- matrix(0,nodes,nodes)
    indexMatrix <- cbind(unlist(do.call('rbind',simNets[[i]]$mel)[,c(2)]),unlist(do.call('rbind',simNets[[i]]$mel)[,c(1)]))
    amat0[indexMatrix] <- get.edge.attribute(simNets[[i]],"Value_ln")
    amatArray[,,i] <- amat0
}


deviationArray <- array(0,dim=c(nodes,nodes,simNum))
transposedArray <- array(0,dim=c(nodes,nodes,simNum))
transitiveArray <- array(0,dim=c(nodes,nodes,simNum))

getTransitiveWeights <- function(wAmat){
    n <- ncol(wAmat)
    transWeights <- matrix(0,n,n)
    for(i in 1:n){
        for(j in 1:n){
            kmins <- numeric(n-2)
            kminInd <- 1
            for(k in c(1:n)[-c(i,j)]){
                kmins[kminInd] <- min(c(wAmat[i,k],wAmat[k,j]))
                kminInd <- kminInd + 1
            }
         transWeights[i,j] <- max(kmins)
        }
    }
    transWeights
}

for(i in 1:simNum){
    deviationMat <- amatArray[,,i]
    diag(deviationMat) <- NA
    deviationMat <- deviationMat
    deviationArray[,,i] <- deviationMat
    transposedArray[,,i] <- t(amatArray[,,i])
    transitiveArray[,,i] <- getTransitiveWeights(amatArray[,,i])
    print(i)
    
}

# Caclulating interpretation quantities for dependent sample
amatArrayDep <- array(0,dim=c(nodes,nodes,simNum))

for(i in 1:simNum){
    amat0 <- matrix(0,nodes,nodes)
    indexMatrix <- cbind(unlist(do.call('rbind',simNetsDep[[i]]$mel)[,c(2)]),unlist(do.call('rbind',simNetsDep[[i]]$mel)[,c(1)]))
    amat0[indexMatrix] <- get.edge.attribute(simNetsDep[[i]],"Value_ln")
    amatArrayDep[,,i] <- amat0
}


deviationArrayDep <- array(0,dim=c(nodes,nodes,simNum))
transposedArrayDep <- array(0,dim=c(nodes,nodes,simNum))
transitiveArrayDep <- array(0,dim=c(nodes,nodes,simNum))

getTransitiveWeights <- function(wAmat){
    n <- ncol(wAmat)
    transWeights <- matrix(0,n,n)
    for(i in 1:n){
        for(j in 1:n){
            kmins <- numeric(n-2)
            kminInd <- 1
            for(k in c(1:n)[-c(i,j)]){
                kmins[kminInd] <- min(c(wAmat[i,k],wAmat[k,j]))
                kminInd <- kminInd + 1
            }
            transWeights[i,j] <- max(kmins)
        }
    }
    transWeights
}

for(i in 1:simNum){
    deviationMatDep <- amatArrayDep[,,i]
    diag(deviationMatDep) <- NA
    deviationMatDep <- deviationMatDep
    deviationArrayDep[,,i] <- deviationMatDep
    transposedArrayDep[,,i] <- t(amatArrayDep[,,i])
    transitiveArrayDep[,,i] <- getTransitiveWeights(amatArrayDep[,,i])
    print(i)
    
}






save(list=c("deviationArray","transposedArray","transitiveArray","deviationArrayDep","transposedArrayDep","transitiveArrayDep"),file="./Code/interpretationStats.RData")

intersectRecip <- sort(intersect(unique(c(transposedArray)),unique(c(transposedArrayDep))))

diffRecip <- NULL
for(i in 1:length(intersectRecip)){
    valInd <- c(deviationArray)[which(c(transposedArray)==intersectRecip[i])]
    valDep <- c(deviationArrayDep)[which(c(transposedArrayDep)==intersectRecip[i])]
    ttobj <- t.test(valDep,valInd)
    diffRecip <- rbind(diffRecip,c(ttobj$conf.int[c(1)],-diff(ttobj$estimate),ttobj$conf.int[c(2)]))
    print(i)
}


pdf("./Draft/draft_figures/mutualInterpretation.pdf",height=4,width=8,pointsize=12)
par(las=1)
plot(intersectRecip[intersectRecip <= 20],diffRecip[intersectRecip <= 20,2],xlab="Mutual Edge Value",ylab="Reciprocity Effect",type="n",ylim=c(min(diffRecip[,1]),max(diffRecip[,2])))
points(intersectRecip[intersectRecip <= 20],diffRecip[intersectRecip <= 20,2],pch=19)
segments(x0=intersectRecip[intersectRecip <= 20],x1=intersectRecip[intersectRecip <= 20],y0=diffRecip[intersectRecip <= 20,1],diffRecip[intersectRecip <= 20,3],lwd=2)
dev.off()

intersectTrans <- sort(intersect(unique(c(transitiveArray)),unique(c(transitiveArrayDep))))

diffTrans <- NULL
for(i in 1:length(intersectTrans)){
    valInd <- c(deviationArray)[which(c(transitiveArray)==intersectTrans[i])]
    valDep <- c(deviationArrayDep)[which(c(transitiveArrayDep)==intersectTrans[i])]
    ttobj <- t.test(valDep,valInd)
    diffTrans <- rbind(diffTrans,c(ttobj$conf.int[c(1)],-diff(ttobj$estimate),ttobj$conf.int[c(2)]))
    print(i)
}


pdf("./Draft/draft_figures/transitiveInterpretation.pdf",height=4,width=8,pointsize=12)
par(las=1)
plot(intersectTrans[intersectTrans <= 20],diffTrans[intersectTrans <= 20,2],xlab="Transitive Edge Value",ylab="Transitivity Effect",type="n",ylim=c(min(diffTrans[,1]),max(diffTrans[,3])))
points(intersectTrans[intersectTrans <= 20],diffTrans[intersectTrans <= 20,2],pch=19)
segments(x0=intersectTrans[intersectTrans <= 20],x1=intersectTrans[intersectTrans <= 20],y0=diffTrans[intersectTrans <= 20,1],diffTrans[intersectTrans <= 20,3],lwd=2)
dev.off()



