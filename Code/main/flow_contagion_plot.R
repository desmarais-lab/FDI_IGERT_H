load("main_network_results_RR.RData")
library(ergm)
library(ergm.count)
library(intergraph)
yr <- 11

ergm.result <- ergms[[yr]]

net <- list_of_networks[[yr]]

edgecovars <- list_of_edgecovs[[yr]]

oecd_both <- edgecovars[["OECD_both"]]
not_oecd_both <- 1-oecd_both
edge_vals <- net %e% "Value"
net %e% "tValue" <- ceiling(log(edge_vals)*2)


el <- as.matrix(net,"edgelist")

edge.weight <- get.edge.attribute(net,"tValue")

obs.weighted.amat <- net[,]

obs.weighted.amat[el] <- edge.weight

set.seed(123)

sim.dist.net.neg <- simulate(net ~ sum+ sum(1/2) +nonzero+#mutual+
                          edgecovmutual(oecd_both)+ edgecovmutual(not_oecd_both)+
                          transitiveweights("min", "max", "min")+
                          nodeicov("polity")+nodeocov("polity")+
                          nodeicov("trade_opennes")+nodeocov("trade_opennes")+
                          nodeicov("gdp.pc")+nodeocov("gdp.pc")+
                          nodematch("OECD_mem")+
                          edgecov(edgecovars[[1]])+edgecov(edgecovars[[2]])+
                          edgecov(edgecovars[[3]])+edgecov(edgecovars[[4]])+
                          edgecov(edgecovars[[5]])+edgecov(edgecovars[[6]])+
                          edgecov(edgecovars[[7]])+edgecov(edgecovars[[9]]),
                        response="tValue",
                        reference=~Poisson, coef = coef(ergm.result),nsim=100,control=control.simulate.formula(MCMC.burnin=10000,MCMC.interval=5000))


save(list="sim.dist.net.neg",file="sim_dist_net_neg.RData")

set.seed(123)

results_summary <- NULL
for(sim in 1:20){

n.nodes <- nrow(net[,])
dist.edgecov <- matrix(runif(n.nodes^2,-1,1),n.nodes,n.nodes)

dist.edgecov <- matrix(runif(n.nodes^2,0,1),n.nodes,n.nodes)

dist.edgecov <- 1*(dist.edgecov > 0.95)

net.dist <- net
net.dist[,] <- net.dist[,]*(1-dist.edgecov)

dist.edgecov <- dist.edgecov*net[,]

sim.dist.net <- simulate(net.dist ~ edgecov(dist.edgecov*50)+sum+ sum(1/2) +nonzero+#mutual+
                          edgecovmutual(oecd_both)+ edgecovmutual(not_oecd_both)+
                          transitiveweights("min", "max", "min")+
                          nodeicov("polity")+nodeocov("polity")+
                          nodeicov("trade_opennes")+nodeocov("trade_opennes")+
                          nodeicov("gdp.pc")+nodeocov("gdp.pc")+
                          nodematch("OECD_mem")+
                          edgecov(edgecovars[[1]])+edgecov(edgecovars[[2]])+
                          edgecov(edgecovars[[3]])+edgecov(edgecovars[[4]])+
                          edgecov(edgecovars[[5]])+edgecov(edgecovars[[6]])+
                          edgecov(edgecovars[[7]])+edgecov(edgecovars[[9]]),
                        response="tValue",
                        reference=~Poisson, coef = c(-100,coef(ergm.result)),nsim=100,control=control.simulate.formula(MCMC.burnin=10000,MCMC.interval=5000))

save(list="sim.dist.net",file="sim_dist_net.RData")

total.amat <- matrix(0,nrow(net[,]),nrow(net[,]))

for(i in 1:length(sim.dist.net)){
  amat.i <- sim.dist.net[[i]][,]
  el.i <- as.matrix(sim.dist.net[[i]],"edgelist")
  amat.i[el.i] <- get.edge.attribute(sim.dist.net[[i]],"tValue")
  total.amat <- total.amat + amat.i
}

mean.amat <- total.amat/length(sim.dist.net)

mean.amat <- mean.amat*(1-dist.edgecov)

sim.mean.j.sends <- matrix(NA,nrow(mean.amat),nrow(mean.amat))
sim.mean.j.receives <- matrix(NA,nrow(mean.amat),nrow(mean.amat))
sim.mean.i.sends <- matrix(NA,nrow(mean.amat),nrow(mean.amat))
sim.mean.i.receives <- matrix(NA,nrow(mean.amat),nrow(mean.amat))

for(i in 1:nrow(mean.amat)){
  for(j in (1:nrow(mean.amat))[-i]){
    sim.mean.j.sends[i,j] <- mean(mean.amat[j,-i])
    sim.mean.i.sends[i,j] <- mean(mean.amat[i,-j])
    sim.mean.j.receives[i,j] <- mean(mean.amat[-i,j])
    sim.mean.i.receives[i,j] <- mean(mean.amat[-j,i])
  }
}




total.amat.neg <- matrix(0,nrow(net[,]),nrow(net[,]))

for(i in 1:length(sim.dist.net.neg)){
  amat.i <- sim.dist.net.neg[[i]][,]
  el.i <- as.matrix(sim.dist.net.neg[[i]],"edgelist")
  amat.i[el.i] <- get.edge.attribute(sim.dist.net.neg[[i]],"tValue")
  total.amat.neg <- total.amat.neg + amat.i
}

mean.amat.neg <- total.amat.neg/length(sim.dist.net.neg)

sim.mean.j.sends.neg <- matrix(NA,nrow(mean.amat),nrow(mean.amat))
sim.mean.j.receives.neg <- matrix(NA,nrow(mean.amat),nrow(mean.amat))
sim.mean.i.sends.neg <- matrix(NA,nrow(mean.amat),nrow(mean.amat))
sim.mean.i.receives.neg <- matrix(NA,nrow(mean.amat),nrow(mean.amat))

for(i in 1:nrow(mean.amat)){
  for(j in (1:nrow(mean.amat))[-i]){
    sim.mean.j.sends.neg[i,j] <- mean(mean.amat.neg[j,-i])
    sim.mean.i.sends.neg[i,j] <- mean(mean.amat.neg[i,-j])
    sim.mean.j.receives.neg[i,j] <- mean(mean.amat.neg[-i,j])
    sim.mean.i.receives.neg[i,j] <- mean(mean.amat.neg[-j,i])
  }
}

results_summary <- rbind(results_summary,cbind(c(mean.amat.neg)[which(c(dist.edgecov)==1)], 
                         (c(sim.mean.j.sends)[which(c(dist.edgecov)==1)] -  c(sim.mean.j.sends.neg)[which(c(dist.edgecov)==1)])/(1+c(sim.mean.j.sends.neg)[which(c(dist.edgecov)==1)]),
                         (c(sim.mean.i.sends)[which(c(dist.edgecov)==1)] -  c(sim.mean.i.sends.neg)[which(c(dist.edgecov)==1)])/(1+c(sim.mean.i.sends.neg)[which(c(dist.edgecov)==1)]),
                         (c(sim.mean.j.receives)[which(c(dist.edgecov)==1)] -  c(sim.mean.j.receives.neg)[which(c(dist.edgecov)==1)])/(1+c(sim.mean.j.receives.neg)[which(c(dist.edgecov)==1)]),
                         (c(sim.mean.i.receives)[which(c(dist.edgecov)==1)] -  c(sim.mean.i.receives.neg)[which(c(dist.edgecov)==1)])/(1+c(sim.mean.i.receives.neg)[which(c(dist.edgecov)==1)])))

write.csv(results_summary,file="contagion_simulation.csv",row.names=F)

print(sim)

}


