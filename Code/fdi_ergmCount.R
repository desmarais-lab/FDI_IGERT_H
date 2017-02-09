
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

setwd("/Users/johnpschoeneman/Desktop/ACI")


#load in data
fdi <- read.csv("fdi_sub.csv", stringsAsFactors=FALSE)        #FDI
fdi <- fdi[,-1]
#201 countries, 12 years (2000-2012),

#extract one year
fdi01 <- subset(fdi, fdi$Year ==2001)
#log variables to get to better scale
fdi01$Value <- round(log(fdi01$Value+1), digits=0)
fdi01$mass <- (log(fdi01$Dest.GDP)*log(fdi01$Origin.GDP))
fdi01$trade_int <- log(fdi01$trade_int)
fdi01$dist <- log(fdi01$dist)

#edge attr: "contig","comlang_off", "comlang_ethno","colony","comcol", "curcol","dist",
#　　　　　 "defense.max.x","nonaggression.max.x","neutrality.max.x","entente.max.x",
#　　　　　 "depth_index", "depth_latent","trade_hco", "trade_int", "trade_cap", "trade_mix"

#create vertex dataset
vertex_attr <- summaryBy(Origin.GDP+Origin.polity+Origin.TO+Origin.pop+Origin.GDP.g+
                           Origin.pv ~ Origin, data=fdi01)
#vertex attr: "Origin.GDP","Origin.polity","Origin.TO", "Origin.pop",  "Origin.GDP.g",　Origin.pv"
#rename vertex dataset
names(vertex_attr) <- c("name","GDP", "Polity", "TradeOpen", "Pop", "GDP.g", "PV")

#create network object 
detach("package:igraph", unload=TRUE)
fdi_net <- network(fdi01, matrix.type="edgelist", directed=TRUE)


#set edge attributes
set.edge.attribute(fdi_net, attrname="Value", value=fdi01$Value)
set.edge.attribute(fdi_net, attrname="distance", value=fdi01$dist)
set.edge.attribute(fdi_net, attrname="contig", value=fdi01$contig)
set.edge.attribute(fdi_net, attrname="colony", value=fdi01$colony)
set.edge.attribute(fdi_net, attrname="lang_ethno", value=fdi01$comlang_ethno)
set.edge.attribute(fdi_net, attrname="defence_t", value=fdi01$defense.max.x)
set.edge.attribute(fdi_net, attrname="nonagg_t", value=fdi01$nonaggression.max.x)
set.edge.attribute(fdi_net, attrname="neut_t", value=fdi01$neutrality.max.x)
set.edge.attribute(fdi_net, attrname="entente_t", value=fdi01$entente.max.x)
set.edge.attribute(fdi_net, attrname="depth", value=fdi01$depth_latent)
set.edge.attribute(fdi_net, attrname="trade_int", value=fdi01$trade_int)
set.edge.attribute(fdi_net, attrname="mass", value=fdi01$mass)

#set vertex attributes
set.vertex.attribute(fdi_net, attrname="GDP", value=vertex_attr$GDP)
set.vertex.attribute(fdi_net, attrname="Polity", value=vertex_attr$GDP)
set.vertex.attribute(fdi_net, attrname="TradeOpen", value=vertex_attr$GDP)
set.vertex.attribute(fdi_net, attrname="Pop", value=vertex_attr$GDP)
set.vertex.attribute(fdi_net, attrname="GDP.g", value=vertex_attr$GDP)
set.vertex.attribute(fdi_net, attrname="PV", value=vertex_attr$GDP)

#check network
fdi_net
list.edge.attributes(fdi_net)

row.names(vertex_attr) <- vertex_attr[,1]



#base formula for only network measures
formula <- fdi_net ~ sum + sum(pow=1/2)+ nonzero +mutual(form="geometric")


# count model
fit.01.1 <- ergm(formula,
                 #estimate='MLE',
                 response="Value",
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


summary(fit.01.1)
mcmc.diagnostics(fit.01.1, vars.per.page=2)


#extended model
formula <- fdi_net ~ sum + sum(pow=1/2)+ mutual(form="geometric") + nonzero + 
  edgecov(fdi_net, "mass", form="sum")+ 
  edgecov(fdi_net, "distance", form="sum")
#+edgecov(fdi_net, "trade_int", form="sum")
#+edgecov(fdi_net, "depth", form="sum")

# count model
fit.01.2 <- ergm(formula,
                 #estimate='MLE',
                 response="Value",
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


summary(fit.01.2)
mcmc.diagnostics(fit.01.2, vars.per.page=2)


#extended model 2, add transitivity
formula <- fdi_net ~ sum + sum(pow=1/2)+ mutual(form="geometric") + nonzero + 
  edgecov(fdi_net, "mass", form="sum")+ 
  edgecov(fdi_net, "distance", form="sum")+
  transitiveweights("min", "max", "min") + 
  cyclicalweights("min","max", "min")
# count model
fit.01.3 <- ergm(formula,
                 #estimate='MLE',
                 response="Value",
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

keepRunning = TRUE
iteration = 0
while(keepRunning){
  fit.01.3  <- ergm(formula,
                    #estimate='MLE', 
                    response="Value",
                    reference=~Poisson,
                    control=control.ergm(MCMLE.trustregion=100,
                                         MCMLE.maxit=50, 
                                         MCMC.samplesize=1000,
                                         MCMC.burnin=500,
                                         MCMC.interval=1000,
                                         init=coef(fit.01.3)
                                         #MCMC.prop.weights="0inflated"
                                         #MCMC.prop.args=list(p0=0.75)
                    ))
  keepRunning = summary(fit.01.3)$iterations == "50 out of 50"
  print(iteration)
  print(coef(fit.01.3))
  iteration = iteration + 1
  
}

summary(fit.01.3)
mcmc.diagnostics(fit.01.3, vars.per.page=2)


library(texreg)
texreg(l = list(fit.01.1, fit.01.2))


