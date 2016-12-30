#=============================================================#
# John Schoeneman
# Work Done For: FDI Network Analysis RA-IGERT
# Date: Fall 2016
# Work Done: Perform GERGM analysis
# Machine: MacPro OSX Yosemite
#=============================================================#


# clear workspace
rm(list=ls())

set.seed(19)

# libraries
library(GERGM)
library(network)
library(igraph)
library(doBy)


setwd("/Users/johnpschoeneman/Documents/school/Penn State/RA:TA/FDI_IGERT_H/Code")

#load in data
fdi <- read.csv("fdi_sub.csv", stringsAsFactors=FALSE)        #FDI
fdi <- fdi[,-1]
#201 countries, 12 years (2000-2012), 

#extract one year
fdi01 <- subset(fdi, fdi$Year ==2001)

# create graph object
fdi_graph <- graph.data.frame(fdi01)
fdi_y <- get.adjacency(fdi_graph,attr='Value', sparse=FALSE)
#check attributes
edge_attr_names(fdi_graph)

#create adjacency matrices for covariates
distance   <- get.adjacency(fdi_graph,attr='dist', sparse=FALSE)
contig     <- get.adjacency(fdi_graph,attr='contig', sparse=FALSE)
colony     <- get.adjacency(fdi_graph,attr='colony', sparse=FALSE)
lang_ethno <- get.adjacency(fdi_graph,attr='comlang_ethno', sparse=FALSE)
defence_t  <- get.adjacency(fdi_graph,attr='defense.max.x', sparse=FALSE)
nonagg_t   <- get.adjacency(fdi_graph,attr='nonaggression.max.x', sparse=FALSE)
neut_t     <- get.adjacency(fdi_graph,attr='neutrality.max.x', sparse=FALSE)
entente_t  <- get.adjacency(fdi_graph,attr='entente.max.x', sparse=FALSE)
depth      <- get.adjacency(fdi_graph,attr='depth_latent', sparse=FALSE)
trade_int  <- get.adjacency(fdi_graph,attr='trade_int', sparse=FALSE)
#edge attr: "contig","comlang_off", "comlang_ethno","colony","comcol", "curcol","dist",
#　　　　　 "defense.max.x","nonaggression.max.x","neutrality.max.x","entente.max.x",
#　　　　　 "depth_index", "depth_latent","trade_hco", "trade_int", "trade_cap", "trade_mix"

#create vertex dataset
vertex_attr <- summaryBy(Origin.GDP+Origin.polity+Origin.TO+Origin.pop+Origin.GDP.g+
                         Origin.pv ~ Origin, data=fdi01)
#vertex attr: "Origin.GDP","Origin.polity","Origin.TO", "Origin.pop",  "Origin.GDP.g",　Origin.pv"
#rename vertex dataset
names(vertex_attr) <- c("name","GDP", "Polity", "TradeOpen", "Pop", "GDP.g", "PV")

#set vertex attributes
fdi_graph <- set_vertex_attr(fdi_graph, name="GDP",value=vertex_attr$GDP)
fdi_graph <- set_vertex_attr(fdi_graph, name="Polity",value=vertex_attr$Polity)
fdi_graph <- set_vertex_attr(fdi_graph, name="TradeOpen",value=vertex_attr$TradeOpen)
fdi_graph <- set_vertex_attr(fdi_graph, name="Pop",value=vertex_attr$Pop)
fdi_graph <- set_vertex_attr(fdi_graph, name="GDP.g",value=vertex_attr$GDP.g)
fdi_graph <- set_vertex_attr(fdi_graph, name="PV",value=vertex_attr$PV)

vertex_attr_names(fdi_graph)


#network statistics
print("Network Statistics")
calculate_network_statistics(fdi_y)

# create forumla
formula <- fdi_y ~ edges + mutual(alpha = 0.8) + out2stars + in2stars + 
                  sender("GDP") + receiver("GDP") + sender("Polity") + receiver("Polity") + 
                  sender("TradeOpen") + receiver("TradeOpen") + sender("Pop") + receiver("Pop") +
                  sender("GDP.g") + receiver("GDP.g") + sender("PV") + receiver("PV") +
                  netcov(distance) + netcov(contig) + netcov(colony) +
                  netcov(lang_ethno) + netcov(defence_t) + netcov(nonagg_t) +
                  netcov(neut_t) + netcov(entente_t) + netcov(depth) + netcov(trade_int)

fdi.fit1 <- gergm(formula,
              covariate_data = vertex_attr,
              number_of_networks_to_simulate = 40000,
              thin = 1/100,
              proposal_variance = 0.05,
              MCMC_burnin = 10000,
              seed = 456,
              convergence_tolerance = 0.5)

