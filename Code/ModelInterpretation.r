# clear workspace, set seed, and set wd
rm(list=ls())

set.seed(19)

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Load Libraries
library(ergm)
library(ergm.count)


### Read in data
load("./Code/TERGM/fdi_cov.Rdata")
load("./Code/TERGM/fdi_net.Rdata")
load("./Code/TERGM/TERGMResults137.RData")
fit.dep <- ergms[[11]]
load("./Code/TERGM/TERGMResults138.RData")
fit.ind <- ergms[[11]]

fdi_net <- list_of_networks[[11]]

simNum <- 1000

set.seed(5)
system.time(simNets <- simulate(fit.dep,nsim=simNum,control=control.simulate.ergm(MCMC.interval=10000)))
save(list="simNets",file="./Code/TERGM/simulatedNetworks.RData")

print("DONE WITH DEP SIMS")

set.seed(5)
system.time(simNets <- simulate(fit.ind,nsim=simNum,control=control.simulate.ergm(MCMC.interval=10000)))
save(list="simNets",file="./Code/TERGM/simulatedNetworksIndependent.RData")

print("DONE WITH IND SIMS")

load("./Code/TERGM/simulatedNetworks.RData")
simNetsDep <- simNets

load("./Code/TERGM/simulatedNetworksIndependent.RData")
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


save(list=c("deviationArray","transposedArray","transitiveArray","deviationArrayDep","transposedArrayDep","transitiveArrayDep"),file="./Code/TERGM/interpretationStats.RData")

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
abline(h=(0:10)/2,lty=3,col="gray60")
abline(v=0:20,lty=3,col="gray60")
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
abline(h=(0:11)/2,lty=3,col="gray60")
abline(v=0:20,lty=3,col="gray60")
points(intersectTrans[intersectTrans <= 20],diffTrans[intersectTrans <= 20,2],pch=19)
segments(x0=intersectTrans[intersectTrans <= 20],x1=intersectTrans[intersectTrans <= 20],y0=diffTrans[intersectTrans <= 20,1],diffTrans[intersectTrans <= 20,3],lwd=2)
dev.off()



