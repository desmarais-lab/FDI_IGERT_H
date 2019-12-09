# clear workspace
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#libraries
library(broom)
library(ergm.count)

for(q in c(25,50)){

#load ERGM results for with
load(paste0("q",q, "_results_RR.RData"))
  #create dataframe for with
  years <- 2002:2012
  df_w <- tidy(ergms[[1]], conf.int=TRUE)
  df_w$year <- 2002
  for(i in 2:11){
    df <- tidy(ergms[[i]], conf.int=TRUE)
    df$year <- years[i]
    df_w <- rbind(df_w, df)
  }
  
  #name dataframe variables
  vars <- c("Sum", "Sum_5", "Nonzero", "Mutuality_OECD", "Mutuality_notOECD",
            "Transitivity","Dest_Polity", "Origin_Polity", "Dest_TO","Origin_TO",
            "Dest_GDPpc", "Origin_GDPpc","Nodematch_OECD",
            "LDV", "Mass", "Distance", "Non_Defense_Treaty", "Defense_Treaty", 
            "Trade_Volume", "Bilateral_Investment_Treaty", "PTA_Depth")
  
  df_w$term <- rep(vars,11)
  #Years
  m_yr <- (2002:2012)
  
  # plot the variables
  #extrafont::loadfonts(device="win")
  library(ggplot2)
  #library(gridExtra)
  
  
  
  for(i in 1:21){
    df <- subset(df_w, df_w$term==vars[i])
    name <- ggplot(df, aes(x=year, y = estimate, ymin= conf.low, ymax= conf.high,
                           colour="#D55E00",legend=TRUE), legend=TRUE)+
      geom_pointrange(size= .5, fatten = 1.25)+ geom_hline(yintercept = 0)+
      scale_x_continuous(breaks= seq(2002,2012,3))+
      xlab("Year") + ylab("Coefficient")+ 
      theme(axis.text  = element_text(family="Times", size=9),
            axis.title  = element_text(family="Times", size=8),
            legend.text  = element_text(family="Times"),
            legend.title  = element_text(family="Times"),  
            plot.title = element_text(family="Times"),
            legend.position="none")
    #create plot object
    ggsave(paste0("q",q,"_rl_plots/",vars[i],".pdf"), name, device="pdf", width=2.85, height=2)
  }
  
}

