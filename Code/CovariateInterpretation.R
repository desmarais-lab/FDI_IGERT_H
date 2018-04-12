#(1) set every other covariate at its mean/median/mode value, 
#(2) simulate 500+ networks
#(3) plot the mean edge values against the covariate values.



# clear workspace, set seed, and set wd
rm(list=ls())
set.seed(19)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Load Libraries
library(ergm)
library(ergm.count)

# choose year
yr <- 1

### Read in data, only using models that include depedence terms
load("TERGM/TERGMResults137.RData")
fit_dep <- ergms[[yr]]
fdi_net <- list_of_networks[[yr]]
rm(list=setdiff(ls(), c("fit_dep","fdi_net")))
load("TERGM/fdi_cov.Rdata")
#load("TERGM/fdi_net.Rdata")

# number of nodes and number of simulations
yr <- 1
n <- 125
simNum <- 500


#list of covariates
# "lag"  "mass" "dist" "alliance"  "defense"   "trade_vol" "bit"  
vertex_names <- c("gdp.pc","polity","trade_opennes") 
fdi_cov <- covlist[[1]]
fdi_ver <- list()
for(i in 1:3){
  fdi_ver[[i]] <- get.vertex.attribute(fdi_net, vertex_names[i])
} 
rm(covlist, i)


# simulate conditional on average values
ver_mean <- list()
cov_mean <- list()
for(i in 1:7){
  cov_mean[[i]] <- matrix(mean(fdi_cov[[i]]),n,n)
}
for(i in 1:3){
  ver_mean[[i]] <- rep(mean(fdi_ver[[i]]),n) 
}
#set mean values as vertex attributes for the network
for(i in 1:3){
  set.vertex.attribute(fdi_net,paste0(vertex_names[i],"_mean"),ver_mean[[i]])
}
list.vertex.attributes(fdi_net)

# simulate networks for Polity, in degree
sim.nets <- simulate(fdi_net ~ sum + sum(pow=1/2)+ nonzero +
                       mutual(form="min")+transitiveweights("min", "max", "min")+
                       nodeicov("polity")+nodeocov("polity_mean")+
                       nodeicov("trade_opennes_mean")+nodeocov("trade_opennes_mean")+
                       nodeicov("gdp.pc_mean")+nodeocov("gdp.pc_mean")+
                       edgecov(cov_mean[[1]])+edgecov(cov_mean[[2]])+
                       edgecov(cov_mean[[3]])+edgecov(cov_mean[[4]])+
                       edgecov(cov_mean[[5]])+edgecov(cov_mean[[6]])+
                       edgecov(cov_mean[[7]]),
                       nsim=simNum,reference=~Poisson,
                       response="Value_ln",coef=coef(fit_dep))
  
  

# calculate average edge value
average.amat <- matrix(0,n,n)
for(i in 1:length(sim.nets)){
  # create empty adjacency matrix to store edge vals
  amat.i <- matrix(0,n,n)
  # store edge vals
  amat.i[as.matrix(sim.nets[[i]],"edgelist")] <- get.edge.attribute(sim.nets[[i]],"Value_ln")
  # add them to the runing sum
  average.amat <- average.amat + amat.i
}
# divide by number of edges simulated
average.amat <- average.amat/length(sim.nets)

#collapse average.amat to plot against changes in vertex attribute
average.col <- colMeans(average.amat) # average received

# plot relationship
plot(c(fdi_ver[[2]]),c(average.col),
     xlab="Polity, Indegree",ylab="Average FDI Received in USD, Logged",
     pch=20)
lines(lowess(c(fdi_ver[[2]]),c(average.col)), col="Red")
abline(h=(0:8)/2,lty=3,col="gray60")
abline(v=-10:10,lty=3,col="gray60")

