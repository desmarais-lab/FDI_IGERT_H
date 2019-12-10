library(ergm.count)

#load data
load("main_network_results_RR.RData")

net2Wamat <- function(net,weight){
  require(network)
  amat <- net[,]
  elist <- as.matrix(net,"edgelist")
  eweight <- get.edge.attribute(net,weight)
  amat[elist] <- eweight
  amat
}

obs.net <- list()
for(i in 1:length(ergms)){
  neti <- ergms[[i]]$network
  obs.net[[i]] <- net2Wamat(neti,"tValue")
}

load("network_result_sims.RData")
dep_sims <- sim_list
dep.net <- list()

for(i in 1:length(dep_sims)){
  dep.simsi <- list()
  for(j in 1:length(dep_sims[[i]])){
    dep.simsi[[j]] <- net2Wamat(dep_sims[[i]][[j]],"tValue")
  }
  dep.net[[i]] <- dep.simsi
}

load("network_independent_sims.RData")
ind_sims <- sim_list

ind.net <- list()

for(i in 1:length(ind_sims)){
  ind.simsi <- list()
  for(j in 1:length(ind_sims[[i]])){
    ind.simsi[[j]] <- net2Wamat(ind_sims[[i]][[j]],"tValue")
  }
  ind.net[[i]] <- ind.simsi
}


# reciprocity

obs.recip <- rep(NA,length(obs.net))
dep.recip <- matrix(NA,length(ind.net[[1]]),length(obs.net))
ind.recip <- matrix(NA,length(ind.net[[1]]),length(obs.net))

for(i in 1:length(obs.net)){
  obs.recip[i] <- grecip(obs.net[[i]],measure="correlation")
  for(j in 1:nrow(dep.recip)){
    dep.recip[j,i] <- grecip(dep.net[[i]][[j]],measure="correlation")
    ind.recip[j,i] <- grecip(ind.net[[i]][[j]],measure="correlation")
  }
}

save(list=c("obs.recip","dep.recip","ind.recip"),file="recip_sim.RData")


pdf("recip_fit.pdf",width=6,height=4,pointsize=14)
par(las=1)

plot(2002:2012,obs.recip,ylim = c(min(c(obs.recip,dep.recip,ind.recip)),max(c(obs.recip,dep.recip,ind.recip))),type="n", ylab="reciprocity")

for(i in 1:length(obs.recip)){
  boxplot(c(dep.recip[,i],ind.recip[,i])~c(rep("dep",nrow(dep.recip)),rep("ind",nrow(dep.recip))),add=T,at=c(2000.9+i,2001.1+i),xlab=NULL,xaxt="n",outline=F,boxwex=.25,col=c("orange","grey70"))
}

abline(v=2002:2012,lty=2,col="grey55")

par(xpd=T)

lines(2002:2012,obs.recip,lwd=2,col="blue")

points(2002:2012,obs.recip,pch=8,col="blue")

legend("top",horiz=T,legend=c("network","independent"),pch=22,col=c("orange","grey70"),pt.bg=c("orange","grey70"),bty="n",inset=-.25,cex=1.25)
dev.off()

# transitivity

wgt.trans <- function(amat){
  ew <- NULL
  tw <- matrix(0,nrow(amat),nrow(amat))
  for(i in 1:nrow(amat)){
    for(j in (1:nrow(amat))[-i]){
      ew <- c(ew,amat[i,j])
      sent.by.i <- amat[i,]
      sent.to.j <- amat[,j]
      tws <- apply(cbind(sent.by.i,sent.to.j),1,min)[-c(i,j)]
      #tws <- NULL
      #for(k in (1:nrow(amat))[c(-i,-j)]){
      #  tws <- c(tws,min(c(amat[i,k],amat[k,j])))
      #}
      tw[i,j] <- min(c(amat[i,j],max(tws)))
    }
  }
  sum(tw)/sum(amat)
}

obs.trans <- rep(NA,length(obs.net))
dep.trans <- matrix(NA,length(ind.net[[1]]),length(obs.net))
ind.trans <- matrix(NA,length(ind.net[[1]]),length(obs.net))

save(list=c("obs.trans","dep.trans","ind.trans"),file="trans_sim.RData")


library(doParallel)
cl <- makeCluster(6) #Setup for parallel computing
registerDoParallel(cl)

for(i in 1:length(obs.net)){
  obs.trans[i] <- wgt.trans(obs.net[[i]])
  res <- foreach(j = 1:nrow(dep.trans)) %do% {
    dep.trans[j,i] <- wgt.trans(dep.net[[i]][[j]])
    ind.trans[j,i] <- wgt.trans(ind.net[[i]][[j]])
  }
  print(i)
}

stopCluster(cl)

pdf("trans_fit.pdf",width=6,height=4,pointsize=14)
par(las=1)

plot(2002:2012,obs.trans,ylim = c(min(c(obs.trans,dep.trans,ind.trans)),max(c(obs.trans,dep.trans,ind.trans))),type="n", ylab="transitivity")

for(i in 1:length(obs.trans)){
  boxplot(c(dep.trans[,i],ind.trans[,i])~c(rep("dep",nrow(dep.trans)),rep("ind",nrow(dep.trans))),add=T,at=c(2000.9+i,2001.1+i),xlab=NULL,xaxt="n",outline=F,boxwex=.25,col=c("orange","grey70"))
}

abline(v=2002:2012,lty=2,col="grey55")

par(xpd=T)

lines(2002:2012,obs.trans,lwd=2,col="blue")

points(2002:2012,obs.trans,pch=8,col="blue")

legend("top",horiz=T,legend=c("network","independent"),pch=22,col=c("orange","grey70"),pt.bg=c("orange","grey70"),bty="n",inset=-.25,cex=1.25)
dev.off()


# outdegsd

odsd <- function(amat){sd(apply(amat,1,sum))}

obs.odsd <- rep(NA,length(obs.net))
dep.odsd <- matrix(NA,length(ind.net[[1]]),length(obs.net))
ind.odsd <- matrix(NA,length(ind.net[[1]]),length(obs.net))

for(i in 1:length(obs.net)){
  obs.odsd[i] <- odsd(obs.net[[i]])
  for(j in 1:nrow(dep.odsd)){
    dep.odsd[j,i] <- odsd(dep.net[[i]][[j]])
    ind.odsd[j,i] <- odsd(ind.net[[i]][[j]])
  }
}

pdf("odsd_fit.pdf",width=6,height=4,pointsize=14)
par(las=1)

plot(2002:2012,obs.odsd,ylim = c(min(c(obs.odsd,dep.odsd,ind.odsd)),max(c(obs.odsd,dep.odsd,ind.odsd))),type="n", ylab="odsditivity")

for(i in 1:length(obs.odsd)){
  boxplot(c(dep.odsd[,i],ind.odsd[,i])~c(rep("dep",nrow(dep.odsd)),rep("ind",nrow(dep.odsd))),add=T,at=c(2000.9+i,2001.1+i),xlab=NULL,xaxt="n",outline=F,boxwex=.25,col=c("orange","grey70"))
}

abline(v=2002:2012,lty=2,col="grey55")

par(xpd=T)

lines(2002:2012,obs.odsd,lwd=2,col="blue")

points(2002:2012,obs.odsd,pch=8,col="blue")

legend("top",horiz=T,legend=c("network","independent"),pch=22,col=c("orange","grey70"),pt.bg=c("orange","grey70"),bty="n",inset=-.25,cex=1.25)
dev.off()



# indegsd

idsd <- function(amat){sd(apply(amat,2,sum))}

obs.idsd <- rep(NA,length(obs.net))
dep.idsd <- matrix(NA,length(ind.net[[1]]),length(obs.net))
ind.idsd <- matrix(NA,length(ind.net[[1]]),length(obs.net))

for(i in 1:length(obs.net)){
  obs.idsd[i] <- idsd(obs.net[[i]])
  for(j in 1:nrow(dep.idsd)){
    dep.idsd[j,i] <- idsd(dep.net[[i]][[j]])
    ind.idsd[j,i] <- idsd(ind.net[[i]][[j]])
  }
}

pdf("idsd_fit.pdf",width=6,height=4,pointsize=14)
par(las=1)

plot(2002:2012,obs.idsd,ylim = c(min(c(obs.idsd,dep.idsd,ind.idsd)),max(c(obs.idsd,dep.idsd,ind.idsd))),type="n", ylab="idsditivity")

for(i in 1:length(obs.idsd)){
  boxplot(c(dep.idsd[,i],ind.idsd[,i])~c(rep("dep",nrow(dep.idsd)),rep("ind",nrow(dep.idsd))),add=T,at=c(2000.9+i,2001.1+i),xlab=NULL,xaxt="n",outline=F,boxwex=.25,col=c("orange","grey70"))
}

abline(v=2002:2012,lty=2,col="grey55")

par(xpd=T)

lines(2002:2012,obs.idsd,lwd=2,col="blue")

points(2002:2012,obs.idsd,pch=8,col="blue")

legend("top",horiz=T,legend=c("network","independent"),pch=22,col=c("orange","grey70"),pt.bg=c("orange","grey70"),bty="n",inset=-.25,cex=1.25)
dev.off()
