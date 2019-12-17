#(1) set every other covariate at its mean/median/mode value, 
#(2) simulate 500+ networks
#(3) plot the mean edge values against the covariate values.
# clear workspace
rm(list=ls())
set.seed(19)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


### Load Libraries
library(ergm)
library(ergm.count)

# choose year
yr <- 10
n <- 124
simNum <- 500
### Read in data, only using models that include depedence terms
load("main_network_results_RR.RData")
fit_dep <- ergms[[yr]]
net <- list_of_networks[[yr]]
#rm(list=setdiff(ls(), c("fit_dep","fdi_net")))
load("main_cov.Rdata")
#load("TERGM/fdi_net.Rdata")

# number of nodes and number of simulations



#list of covariates
#Edge list:  1:9 = c(lag, mass, dist, alliance, defense, trade_vol, bit, both_oecd, pta_depth)  
fdi_cov <- covlist[[yr]]
fdi_ver <- list()
oecd_both <- fdi_cov[["OECD_both"]]
not_oecd_both <- 1-oecd_both
edge_vals <- net %e% "Value"
net %e% "tValue" <- ceiling(log(edge_vals)*2)
edge_vals2 <- net %e% "tValue"

vertex_names <- c("polity", "trade_opennes", "gdp.pc", "OECD_mem")
for(i in 1:length(vertex_names)){
  fdi_ver[[i]] <- get.vertex.attribute(net, vertex_names[i])
} 
rm(covlist, i)


# simulate conditional on average values
ver_mean <- list()
cov_mean <- list()
for(i in 1:length(fdi_cov)){
  cov_mean[[i]] <- matrix(mean(fdi_cov[[i]]),n,n)
}
for(i in 1:length(vertex_names)){
  ver_mean[[i]] <- rep(mean(fdi_ver[[i]]),n) 
}
#set mean values as vertex attributes for the network
for(i in 1:length(vertex_names)){
  set.vertex.attribute(net,paste0(vertex_names[i],"_mean"),ver_mean[[i]])
}
list.vertex.attributes(net)

# simulate networks for Polity, in degree
sim.nets <- simulate(net ~ sum+ sum(1/2) +nonzero+
                       edgecovmutual(oecd_both)+ edgecovmutual(not_oecd_both)+
                       transitiveweights("min", "max", "min")+
                       nodeicov("polity_mean")+nodeocov("polity_mean")+
                       nodeicov("trade_opennes_mean")+nodeocov("trade_opennes_mean")+
                       nodeicov("gdp.pc_mean")+nodeocov("gdp.pc_mean")+
                       nodematch("OECD_mem_mean")+
                       edgecov(cov_mean[[1]])+edgecov(cov_mean[[2]])+
                       edgecov(cov_mean[[3]])+edgecov(cov_mean[[4]])+
                       edgecov(cov_mean[[5]])+edgecov(fdi_cov[[6]])+
                       edgecov(cov_mean[[7]])+edgecov(cov_mean[[9]]),
                       nsim=simNum,reference=~Poisson,
                       response="tValue",coef=coef(fit_dep))
  
  

# calculate average edge value
average.amat <- matrix(0,n,n)
for(i in 1:length(sim.nets)){
  # create empty adjacency matrix to store edge vals
  amat.i <- matrix(0,n,n)
  # store edge vals
  amat.i[as.matrix(sim.nets[[i]],"edgelist")] <- get.edge.attribute(sim.nets[[i]],"tValue")
  # add them to the runing sum
  average.amat <- average.amat + amat.i
}
# divide by number of edges simulated
average.amat <- average.amat/length(sim.nets)

#collapse average.amat to plot against changes in vertex attribute
average.col <- colMeans(average.amat) # average received

load("tradevol_sims.Rdata")
# plot relationship
pdf("figures/tradevol_sims.pdf",height=4,width=8,pointsize=12)
par(las=1)
plot(c(fdi_cov[[6]]),c(average.amat),
     xlab="Trade Volume",ylab="Average FDI Received (USD millions), logged",
     pch=20)
lines(lowess(c(fdi_cov[[6]]),c(average.amat)), col="Red",lwd =3)
abline(h=seq(0,20,2.5),lty=3,col="gray60")
abline(v=seq(0,20,2.5),lty=3,col="gray60")
dev.off()
