
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


# create forumla
formula <- fdi_net ~ sum + nonzero +CMP+ mutual(form="geometric")

# count model
g.01.fit <- ergm(formula,
                 #estimate='MLE',
                 response="Value",
                 reference=~Poisson,
                 #verbose=TRUE,
                 control=control.ergm(MCMLE.trustregion=100,
                                      MCMLE.maxit=10, 
                                      MCMC.samplesize=1000,
                                      MCMC.burnin=500,
                                      MCMC.interval=1000,
                                      MCMC.prop.weights="0inflated"
                                      #MCMC.prop.args=list(p0=0.75)
                                      ))

keepRunning = TRUE
iteration = 0
while(keepRunning){
  g.01.fit <- ergm(formula,
                   #estimate='MLE', 
                   response="Value",
                   reference=~Binomial(985),
                   control=control.ergm(MCMLE.trustregion=100,
                                        MCMLE.maxit=50, 
                                        MCMC.samplesize=1000,
                                        MCMC.burnin=500,
                                        MCMC.interval=1000,
                                        init=coef(g.01.fit)
                                        #MCMC.prop.weights="0inflated"
                                        #MCMC.prop.args=list(p0=0.75)
                   ))
  keepRunning = summary(g.01.fit)$iterations == "50 out of 50"
  print(iteration)
  print(coef(g.01.fit))
  iteration = iteration + 1
    
}





summary(g.01.fit)
mcmc.diagnostics(g.01.fit, vars.per.page=2)

g.01.fit.gof<-gof(g.01.fit)
summary(g.01.fit.gof) 
par(mfrow=c(2,2))
plot(g.01.fit.gof) 


