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

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(doBy)
library(network)
#load in data
fdi <- read.csv("sub_stock.csv", stringsAsFactors=FALSE)        #FDI
fdi <- fdi[,-1]

i = 2002

# dataframe to store values
a <- data.frame(1:26)
wo <- data.frame(1:24)

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
m_name <- paste("models_tweight/model2_",m_yr, "_w.rda", sep="")
load(m_name)
m1_name <- paste("models_tweight/model_",m_yr, "_wo.rda", sep="")
load(m1_name)

#loop through coefficients to sign to dataframe
a <- cbind(a, coef(fit.01.2))
a <- cbind(a, confint(fit.01.2)[,1])
a <- cbind(a, confint(fit.01.2)[,2])

wo <- cbind(wo, coef(fit.01.1))
wo <- cbind(wo, confint(fit.01.1)[,1])
wo <- cbind(wo, confint(fit.01.1)[,2])

}

a <- a[,-1]
wo <- wo[,-1]
#save a

#load a

#name dataframe variables
vars <- c("Sum", "Sum(1/2)", "Nonzero", "Mutuality", "Transitivity",
          "LDV", "Mass", "Distance", "Contiguity", "Former Colony",
          "Common Language", "Defense Treaty", "Non-agg Treaty",
          "Nuetrality Treat", "Entente Treaty", "PTA Depth",
          "Origin Polity", "Origin TO", "Origin Growth",
          "Origin PV", "Origin GDPpc",
          "Dest. Polity", "Dest. TO", "Dest. Growth",
          "Dest. PV", "Dest. GDPpc")

vars_wo <- c("Sum", "Sum(1/2)", "Nonzero", 
             "LDV", "Mass", "Distance", "Contiguity", "Former Colony",
             "Common Language", "Defense Treaty", "Non-agg Treaty",
             "Nuetrality Treaty", "Entente Treaty", "PTA Depth",
             "Origin Polity", "Origin TO", "Origin Growth",
             "Origin PV", "Origin GDPpc",
             "Dest. Polity", "Dest. TO", "Dest. Growth",
             "Dest. PV", "Dest. GDPpc")

plot_n <- c("Sum", "Sum_5", "Nonzero", 
             "LDV", "Mass", "Distance", "Contiguity", "Former Colony",
             "Common Language", "Defense Treaty", "Non-agg Treaty",
             "Nuetrality Treaty", "Entente Treaty", "PTA Depth",
             "Origin Polity", "Origin TO", "Origin Growth",
             "Origin PV", "Origin GDPpc",
             "Dest. Polity", "Dest. TO", "Dest. Growth",
             "Dest. PV", "Dest. GDPpc")

vars_n <- c("Mutuality", "Transitivity")

#Years
m_yr <- (2002:2012)


#reorganize fit with network terms data for ggplot
gg_a <- matrix(nrow=11, ncol=78)
#loop by variable and year
for(j in 1:26){
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


#reorganize fit without network terms data for ggplot
gg_wo <- matrix(nrow=11, ncol=72)
#loop by variable and year
for(j in 1:24){
  gg_col <- j*3-2
  for(i in -2:8){
    #point estimates
    mul <- i +3
    a_i <- mul*3-2
    gg_wo[mul,gg_col] <- wo[j,a_i]
    #Upper CI
    mul <- i +3
    a_i <- mul*3-2+1
    gg_wo[mul,gg_col+1] <- wo[j,a_i]
    #Lower CI
    mul <- i +3
    a_i <- mul*3-2+2
    gg_wo[mul,gg_col+2] <- wo[j,a_i]
  }
}



gg_n <- gg_a[,10:15] # only network measures
gg_a <- gg_a[,c(1:9, 16:78)]# drop network measures (column 5&6)
#turn into data.frames and rename variables
gg_n <- data.frame(gg_n)
gg_a <- data.frame(gg_a)
gg_wo <- data.frame(gg_wo)
# create variable that denotes model
gg_wo$model <- "Without Network Terms"
gg_a$model <- "With Network Terms"
gg_wo$year <- 2002:2012-.1
gg_a$year <-  2002:2012+.1
# stack matrices to plot on topbox
gg_both <- rbind (gg_wo, gg_a)


# plot the variables
library(ggplot2)
library(gridExtra)
#source("http://peterhaschke.com/Code/multiplot.R")


for(i in 1:24){
  pe   <- i*3-2
  Lci  <- i*3-1
  Uci  <- i*3
  df <- data.frame(PE = gg_both[,pe], Lower= gg_both[,Lci], Upper = gg_both[,Uci],
                   Model = gg_both[,73], Year = gg_both[,74])
  name <- ggplot(df, aes(x=Year, y = PE, ymin= Lower, ymax= Upper, colour=Model), 
                 legend=TRUE)+
    geom_pointrange(size= .2)+ geom_hline(yintercept = 0)+
    scale_x_continuous(breaks= seq(2002,2012,3))+
    scale_color_manual(breaks = c("w/o", "w/"),
                       values=c("#D55E00", "#999999"))+
    ggtitle(vars_wo[i])+xlab("Year") + ylab("Coefficient")+ 
    theme(axis.text.x  = element_text(family="Times"),
          axis.title.x  = element_text(family="Times"),
          axis.text.y  = element_text(family="Times"),
          axis.title.y  = element_text(family="Times"),
          legend.text  = element_text(family="Times"),
          legend.title  = element_text(family="Times"),
          plot.title = element_text(family="Times"),
          legend.position="bottom") 
  #create plot object
  ggsave(paste("rl_plots/",plot_n[i], sep=""), name, device="pdf", width=3, height=2)
}


ggplot(df, aes(x=Year, y = PE, ymin= Lower, ymax= Upper, colour=Model,legend=TRUE), 
       legend=TRUE)+
  geom_pointrange(size= .3)+ geom_hline(yintercept = 0)+
  scale_x_continuous(breaks= seq(2002,2012,3))+
  scale_color_manual(breaks = c("Without Network Terms", "With Network Terms"),
                     values=c("#D55E00", "#999999"))+
  xlab("Year") + ylab("Coefficient")+ 
  theme(axis.text.x  = element_text(family="Times"),
        axis.title.x  = element_text(family="Times"),
        axis.text.y  = element_text(family="Times"),
        axis.title.y  = element_text(family="Times"),
        legend.text  = element_text(family="Times"),
        legend.title  = element_text(family="Times"),
        plot.title = element_text(family="Times"),
        legend.position="bottom")

ggsave("rl_plots/Legend", device="pdf", width=3, height=2)


for(i in 1:2){
  pe   <- i*3-2
  Lci  <- i*3-1
  Uci  <- i*3
  df <- data.frame(PE = gg_n[,pe], Lower= gg_n[,Lci], Upper = gg_n[,Uci])
  name <- ggplot(df, aes(x=m_yr, y = PE, ymin= Lower, ymax= Upper), 
                 legend=TRUE)+
    geom_pointrange(size= .2, colour= "#D55E00")+ geom_hline(yintercept = 0)+
    scale_x_continuous(breaks= seq(2002,2012,3))+
    ggtitle(vars_n[i])+xlab("Year") + ylab("Coefficient")+ 
    theme(axis.text.x  = element_text(family="Times"),
          axis.title.x  = element_text(family="Times"),
          axis.text.y  = element_text(family="Times"),
          axis.title.y  = element_text(family="Times"),
          legend.text  = element_text(family="Times"),
          legend.title  = element_text(family="Times"),
          plot.title = element_text(family="Times"),
          legend.position="bottom") 
  #create plot object
  ggsave(paste("rl_plots/",vars_n[i], sep=""), name, device="pdf", width=3, height=2)
}
