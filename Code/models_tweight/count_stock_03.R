#=============================================================#
# John Schoeneman
# Work Done For: FDI Network Analysis RA-IGERT
# Date: Fall 2016
# Work Done: Perform ERGM Count analysis
# Machine: MacPro OSX Yosemite
#=============================================================#

# clear workspace
rm(list=ls())

set.seed(19)

# libraries
library(ergm.count)
library(network)
library(igraph)
library(doBy)
library(plyr)

#setwd("/Users/johnpschoeneman/Desktop/ACI/Count")

#load in data
fdi <- read.csv("sub_stock.csv", stringsAsFactors=FALSE)        #FDI
fdi <- fdi[,-1]
#125 countries, 12 years (2001-2012),
fdi <- fdi[,c(2,1,3:44)]
#extract one year
fdi02 <- subset(fdi, fdi$Year ==2003)

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

#check network
fdi_net
list.edge.attributes(fdi_net)

row.names(vertex_attr) <- vertex_attr[,1]



#Model 2: add exogenous variables
formula <- fdi_net ~ sum + sum(pow=1/2)+ nonzero +
  mutual(form="min")+transitiveweights("min", "max", "min")+
  edgecov(fdi_net, "lag_stock", form="sum")+
  edgecov(fdi_net, "mass", form="sum")+
  edgecov(fdi_net, "distance", form="sum")+
  edgecov(fdi_net, "contig", form="sum")+
  edgecov(fdi_net, "colony", form="sum")+
  edgecov(fdi_net, "lang_ethno", form="sum")+
  edgecov(fdi_net, "defence_t", form="sum")+
  edgecov(fdi_net, "nonagg_t", form="sum")+
  edgecov(fdi_net, "neut_t", form="sum")+
  edgecov(fdi_net, "entente_t", form="sum")+
  edgecov(fdi_net, "depth", form="sum")+
  nodeocov("Polity", form="sum")+
  nodeocov("TradeOpen", form="sum")+
  nodeocov("GDP.g", form="sum")+
  nodeocov("PV", form="sum")+
  nodeocov("GDPpc", form="sum")+
  nodeicov("Polity", form="sum")+
  nodeicov("TradeOpen", form="sum")+
  nodeicov("GDP.g", form="sum")+
  nodeicov("PV", form="sum")+
  nodeicov("GDPpc", form="sum")


# count model
fit.01.2 <- ergm(formula,
                 #estimate='MLE',
                 response="Value_ln",
                 reference=~Poisson,
                 #verbose=TRUE,
                 control=control.ergm(MCMLE.trustregion=100,
                                      MCMLE.maxit=50, 
                                      MCMC.samplesize=10000,
                                      MCMC.burnin=500,
                                      MCMC.interval=1000
                                      #,MCMC.prop.weights="0inflated"
                                      #,MCMC.prop.args=list(p0=0.75)
                 ))


## save this model
save(fit.01.2, file = "model2_03_w.rda")


