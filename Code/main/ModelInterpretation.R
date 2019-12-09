# clear workspace
#rm(list=ls())
#set.seed(19)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


### Load Libraries
library(ergm)
library(ergm.count)


### Read in data
load("main_cov.Rdata")
load("main_net.Rdata")
load("main_network_results_RR.RData")
fit.dep <- ergms[[11]]
load("main_independent_results_RR.RData")
fit.ind <- ergms[[11]]

fdi_net <- list_of_networks[[11]]

simNum <- 1000

set.seed(5)
system.time(simNets <- simulate(fit.dep,nsim=simNum,control=control.simulate.ergm(MCMC.burnin = 50000, MCMC.interval=10000)))
save(list="simNets",file="simulatedNetworks.RData")

print("DONE WITH DEP SIMS")

set.seed(5)
system.time(simNets <- simulate(fit.ind,nsim=simNum,control=control.simulate.ergm(MCMC.burnin = 50000, MCMC.interval=10000)))
save(list="simNets",file="simulatedNetworksIndependent.RData")

print("DONE WITH IND SIMS")

load("simulatedNetworks.RData")
simNetsDep <- simNets

load("simulatedNetworksIndependent.RData")
simNets <- simNets

nodes <- network.size(fdi_net)




### Calculating interpretation quantities for independent model
amatArray <- array(0,dim=c(nodes,nodes,simNum))

for(i in 1:simNum){
    amat0 <- matrix(0,nodes,nodes)
    indexMatrix <- cbind(unlist(do.call('rbind',simNets[[i]]$mel)[,c(2)]),unlist(do.call('rbind',simNets[[i]]$mel)[,c(1)]))
    amat0[indexMatrix] <- get.edge.attribute(simNets[[i]],"tValue")
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
    amat0[indexMatrix] <- get.edge.attribute(simNetsDep[[i]],"tValue")
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


save(list=c("deviationArray","transposedArray","transitiveArray","deviationArrayDep","transposedArrayDep","transitiveArrayDep"),file="interpretationStats.RData")
load("interpretationStats.RData")


#intersectRecip <- sort(intersect(unique(c(transposedArray)),unique(c(transposedArrayDep))))

intersectRecip <- c(0:30)

diffRecip <- NULL
for(i in 1:length(intersectRecip)){
    valInd <- c(deviationArray)[which(c(transposedArray)==intersectRecip[i])]
    valDep <- c(deviationArrayDep)[which(c(transposedArrayDep)==intersectRecip[i])]
    ttobj <- t.test(valDep,valInd)
    diffRecip <- rbind(diffRecip,c(ttobj$conf.int[c(1)],-diff(ttobj$estimate),ttobj$conf.int[c(2)]))
    print(i)
}


pdf("figures/mutualInterpretation.pdf",height=4,width=8,pointsize=12)
par(las=1)
plot(intersectRecip[intersectRecip <= 25],diffRecip[intersectRecip <= 25,2],xlab="Mutual Edge Value",ylab="Reciprocity Effect",type="n",ylim=c(min(diffRecip[,1]),max(diffRecip[,2])))
abline(h=(0:10)/2,lty=3,col="gray60")
abline(v=0:25,lty=3,col="gray60")
points(intersectRecip[intersectRecip <= 25],diffRecip[intersectRecip <= 25,2],pch=19)
segments(x0=intersectRecip[intersectRecip <= 25],x1=intersectRecip[intersectRecip <= 25],y0=diffRecip[intersectRecip <= 25,1],diffRecip[intersectRecip <= 25,3],lwd=2)
dev.off()

intersectTrans <- c(0:30)

diffTrans <- NULL
for(i in 1:length(intersectTrans)){
    valInd <- c(deviationArray)[which(c(transitiveArray)==intersectTrans[i])]
    valDep <- c(deviationArrayDep)[which(c(transitiveArrayDep)==intersectTrans[i])]
    ttobj <- t.test(valDep,valInd)
    diffTrans <- rbind(diffTrans,c(ttobj$conf.int[c(1)],-diff(ttobj$estimate),ttobj$conf.int[c(2)]))
    print(i)
}


pdf("figures/transitiveInterpretation.pdf",height=4,width=8,pointsize=12)
par(las=1)
plot(intersectTrans[intersectTrans <= 25],diffTrans[intersectTrans <= 25,2],xlab="Transitive Edge Value",ylab="Transitivity Effect",type="n",ylim=c(min(diffTrans[,1]),2))
abline(h=(0:11)/2,lty=3,col="gray60")
abline(v=0:25,lty=3,col="gray60")
points(intersectTrans[intersectTrans <= 25],diffTrans[intersectTrans <= 25,2],pch=19)
segments(x0=intersectTrans[intersectTrans <= 25],x1=intersectTrans[intersectTrans <= 25],y0=diffTrans[intersectTrans <= 25,1],diffTrans[intersectTrans <= 25,3],lwd=2)
dev.off()



