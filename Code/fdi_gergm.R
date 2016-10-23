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


setwd("/Users/johnpschoeneman/Documents/school/Penn State/RA:TA/FDI_IGERT_H/Code")

#load in data
fdi <- read.csv("fdi_panel.csv", stringsAsFactors=FALSE)        #FDI
#201 countries, 12 years (2000-2012), 

#extract one year
fdi01 <- subset(fdi, fdi$Year ==2001)
fdi01$Value <- ifelse(is.na(fdi01$Value), 0, fdi01$Value)
#keep gravity variables
fdi01 <- fdi01[c('dyadid','Destination','Origin','Value', 'dist')]

#create vertex dataset
vertex_attr <- na.omit(summaryBy(as.numeric(Origin.GDP) ~ Origin, data=fdi))

#clean #29793
fdi01 <- na.omit(fdi01)
fdi01 <- merge(fdi01, vertex_attr, by.x =c("Origin"), by.y = c("Origin"))
fdi01 <- merge(fdi01, vertex_attr, by.x =c("Destination"), by.y = c("Origin"))
vertex_attr <- summaryBy(fdi01[,6] ~ Origin, data=fdi01)
fdi01 <- fdi01[c('Destination','Origin','Value', 'dist')]
colnames(vertex_attr)[2] <- "GDP"

row.names(vertex_attr) <- vertex_attr[,1]

#create sociomatrix without NA
fdi_graph <- graph.data.frame(fdi01)

fdi_y <- get.adjacency(fdi_graph,attr='Value', sparse=FALSE)
fdi_dist <- get.adjacency(fdi_graph,attr='dist', sparse=FALSE)


#plot
plot_network(fdi_y)

# create forumla
formula <- fdi_y ~ edges + mutual(alpha = 0.8)+ netcov(fdi_dist) +
  sender("GDP") +  receiver("GDP") + out2stars+ in2stars


fdi.fit1 <- gergm(formula,
              covariate_data = vertex_attr,
              number_of_networks_to_simulate = 40000,
              thin = 1/100,
              proposal_variance = 0.05,
              MCMC_burnin = 10000,
              seed = 456,
              convergence_tolerance = 0.5)

