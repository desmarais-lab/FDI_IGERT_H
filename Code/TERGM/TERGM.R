# clear workspace
rm(list=ls())

set.seed(19)

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# libraries
library(ergm.count)
library(network)
#library(igraph)


# load data

trade_openess <- read.csv("trade_openness.csv", stringsAsFactors=FALSE)
row.names(trade_openess) <- trade_openess[,1]
#trade_openess <- data.frame(trade_openess[,-1])
polity <- read.csv("polity.csv", stringsAsFactors=FALSE)
row.names(polity) <- polity[,1]
#polity <- data.frame(polity[,-1])
dist <- read.csv("dist.csv", stringsAsFactors=FALSE)
row.names(dist) <- dist[,1]
dist <- data.frame(dist[,-1])
mass <- read.csv("mass.csv", stringsAsFactors=FALSE)
row.names(mass) <- mass[,1]
mass <- data.frame(mass[,-1])
fdi_lag <- read.csv("fdi_lag.csv", stringsAsFactors=FALSE)
row.names(fdi_lag) <- fdi_lag[,1]
fdi_lag <- data.frame(fdi_lag[,-1])
fdi <- read.csv("fdi.csv", stringsAsFactors=FALSE)
row.names(fdi) <- fdi[,1]
fdi <- data.frame(fdi[,-1])

fdi.net <- network(fdi,dir=T)
fdi.edgelist <- as.matrix(fdi.net,"edgelist")
set.edge.attribute(fdi.net,"fdi.value",fdi[fdi.edgelist])
year <- substr(row.names(fdi),nchar(row.names(fdi))-3,nchar(row.names(fdi)))
set.vertex.attribute(fdi.net,"year",year)



#Model 2: add network terms
formula <- fdi.net ~ sum + sum(pow=1/2)+ nonzero
  #edgecov(dist, form="sum")+
  #edgecov(mass, form="sum")+
  #nodeocov(polity[,2], form="sum")+
  #nodeocov(trade_openess[,2], form="sum")+
  #nodeicov(polity[,2], form="sum")+
  #nodeicov(trade_openess[,2], form="sum")


# count model
fit.1 <- ergm(formula,
                 #estimate='MLE',
                 response="fdi.value",
                 reference=~Poisson,
                 verbose=TRUE,
                 #constraints= ~blockdiag("year"),
                 control=control.ergm(MCMLE.trustregion=100,
                                      MCMLE.maxit=50, 
                                      MCMC.samplesize=1000,
                                      MCMC.burnin=500,
                                      MCMC.interval=1000
                                      #,MCMC.prop.weights="0inflated"
                                      #,MCMC.prop.args=list(p0=0.75)
              ))


## save this model
save(fit.1, file = "model2_02_w.rda")


