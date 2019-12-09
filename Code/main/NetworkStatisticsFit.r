## Load Libraries
library(ergm)
library(ergm.count)


### Read in data
load("main_cov.Rdata")
load("main_net.Rdata")
load("main_network_results_RR.RData") #with
fit.dep <- ergms[[11]]
load("main_independent_results_RR.RData") #without
fit.ind <- ergms[[11]]

fdi_net <- list_of_networks[[11]]


load("simulatedNetworks.RData")
simNetsDep <- simNets

load("simulatedNetworksIndependent.RData")
simNets <- simNets


library(sna)

obs.recip <- grecip(fdi_net,measure="dyadic.nonnull")
sim.recip.dep <- grecip(simNetsDep,measure="dyadic.nonnull")
sim.recip.ind <- grecip(simNets,measure="dyadic.nonnull")

pdf("figures/reciprocityFit.pdf",height=6,width=6,pointsize=12)
par(las=1)
Network <- sim.recip.dep
Independent <- sim.recip.ind
boxplot(cbind(Network,Independent),col="grey65",ylab="Reciprocity")
abline(h=obs.recip,lwd=2,lty=2)
dev.off()

