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


load("./Code/TERGM/simulatedNetworks.RData")
simNetsDep <- simNets

load("./Code/TERGM/simulatedNetworksIndependent.RData")
simNets <- simNets


library(sna)

obs.recip <- grecip(fdi_net,measure="dyadic.nonnull")
sim.recip.dep <- grecip(simNetsDep,measure="dyadic.nonnull")
sim.recip.ind <- grecip(simNets,measure="dyadic.nonnull")

pdf("./Draft/draft_figures/reciprocityFit.pdf",height=6,width=6,pointsize=12)
par(las=1)
Network <- sim.recip.dep
Independent <- sim.recip.ind
boxplot(cbind(Network,Independent),col="grey65")
abline(h=obs.recip,lwd=2,lty=2)
dev.off()

