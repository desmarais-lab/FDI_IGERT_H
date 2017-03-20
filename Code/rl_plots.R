#=============================================================#
# John Schoeneman
# Work Done For: FDI Network Analysis RA-IGERT
# Date: Spring 2017
# Work Done: Rope Ladder Plots for Years
# Machine: MacPro OSX Yosemite
#=============================================================#
# model fit objects on in Box Folder "fdi_models"; 'wo' signifies no network terms

# clear workspace and set seed
rm(list=ls())
set.seed(19)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load in data
fdi <- read.csv("sub_stock.csv", stringsAsFactors=FALSE)        #FDI
fdi <- fdi[,-1]

# dataframe to store values
a <- data.frame(1:21)

#loop through each year
for(i in 2002:2012){
#create net object for models
fdi02 <- subset(fdi, fdi$Year ==i)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
#scale continuous variables
vars <- c(18:34,36, 38:39, 41:44)
for(j in vars){
  fdi02[,j] <- range01(fdi02[,j]) 
}
#create vertex dataset
vertex_attr <- summaryBy(Origin.GDP+Origin.polity+Origin.TO+Origin.pop+Origin.GDPg+ 
                           Origin.GDPpc+Origin.pv ~ Origin, data=fdi02)
#rename vertex dataset
names(vertex_attr) <- c("name","GDP", "Polity", "TradeOpen", "Pop", "GDP.g","GDPpc", "PV")
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

#load model
m_yr <- i-2000
m_yr <- ifelse(m_yr<10, paste("0", m_yr, sep=""), paste(m_yr, sep=""))
m_name <- paste("fdi_models/model_",m_yr, "_w.rda", sep="")
load(m_name)

#loop through coefficients to sign to dataframe
a <- cbind(a, coef(fit.01.2))
a <- cbind(a, confint(fit.01.2)[,1])
a <- cbind(a, confint(fit.01.2)[,2])

}

a <- a[,-1]
#save a

#load a

#name dataframe variables
vars <- c("Sum", "Sum(1/2)", "Nonzero", "Mutuality", "Transitive Ties",
          "LDV", "Mass", "Distance", "Contiguity", "Former Colony",
          "Common Language", "Defense Treaty", "Non-aggression Treaty",
          "Nuetrality Treat", "Entente Treaty", "PTA Depth",
          "Node Polity", "Node Trade Openness", "Node GDP growth",
          "Node Political Violence", "Node GDP per capita")

#Years
m_yr <- (2002:2012)


#reorganize data for ggplot
gg_a <- matrix(nrow=11, ncol=63)
#loop by variable and year
for(j in 1:21){
  gg_col <- j*3-2
  for(i in -2:8){
    #point estimates
    mul <- i +3
    a_i <- mul*3-2
    gg_a[mul,gg_col] <- a[j,a_i]
    #Upper CI
    mul <- i +3
    a_i <- mul*3-2+1
    gg_a[mul,gg_col+1] <- a[j,a_i]
    #Lower CI
    mul <- i +3
    a_i <- mul*3-2+2
    gg_a[mul,gg_col+2] <- a[j,a_i]
  }
}

gg_a <- data.frame(gg_a)

# plot the variables
library(ggplot2)
source("http://peterhaschke.com/Code/multiplot.R")


for(i in 4:12){
  pe   <- i*3-2
  Lci  <- i*3-1
  Uci  <- i*3
  df <- data.frame(PE = gg_a[,pe], Lower= gg_a[,Lci], Upper = gg_a[,Uci])
  name <- ggplot(df, aes(x=m_yr, y = PE, ymin= Lower, ymax= Upper))+
    geom_pointrange()+ geom_hline(yintercept = mean(df$PE))+ 
    ggtitle(vars[i])+ xlab("Year") + ylab("Coefficient")
  #create plot object
  assign(paste("p", i, sep=""), name)
}
multiplot(p4,p5,p6,p7,p8,p9,p10,p11,p12, cols=3)


for(i in 13:21){
  pe   <- i*3-2
  Lci  <- i*3-1
  Uci  <- i*3
  df <- data.frame(PE = gg_a[,pe], Lower= gg_a[,Lci], Upper = gg_a[,Uci])
  name <- ggplot(df, aes(x=m_yr, y = PE, ymin= Lower, ymax= Upper))+
    geom_pointrange()+ geom_hline(yintercept = mean(df$PE))+ 
    ggtitle(vars[i])+ xlab("Year") + ylab("Coefficient")
  #create plot object
  assign(paste("p", i, sep=""), name)
}
multiplot(p13,p14,p15,p16,p17,p18,p19,p20,p21, cols=3)


