# clear workspace, setting random seed, and setting work directory
#rm(list=ls())
#set.seed(19)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(ggplot2)
library(ggthemes)
library(broom)


#load ERGM results for with
load("LSM_nrm_results.RData")

#check convergence
for(i in 1:11){
  beta.mcmc <- as.mcmc(LSMs[[i]]$BETA)
  print(geweke.diag(beta.mcmc))
}

#put results into df
years <- 2002:2012
df <- tidy(ergms[[1]], conf.int=TRUE)
df$year <- 2002
for(i in 2:11){
  dfi <- tidy(ergms[[i]], conf.int=TRUE)
  dfi$year <- years[i]
  df <- rbind(df, dfi)
}




#set vars to loop through.
vars <- c("Dest_Polity", "Origin_Polity", "Dest_TO","Origin_TO","Dest_GDPpc", "Origin_GDPpc",
          "LDV", "Mass", "Distance", "Non_Defense_Treaty", "Defense_Treaty", 
          "Trade_Volume", "Bilateral_Investment_Treaty","both_OECD", "PTA_Depth")


for(i in list_vars){
  df <- subset(df_50, df_50$term==df_names[i])
  df$year <- ifelse(df$model=="with", df$year+.1, df$year)
  df$year <- ifelse(df$model=="without", df$year-.1, df$year)
  name <- ggplot(df, aes(x=year, y = estimate, ymin= conf.low, ymax= conf.high,
                         colour=model,legend=TRUE, shape=model), legend=TRUE)+
    geom_pointrange(size= .5, fatten = 1.25)+ geom_hline(yintercept = 0)+
    scale_x_continuous(breaks= seq(2002,2012,3))+
    scale_color_manual(breaks = c("W/", "W/"),
                       values=c("#D55E00", "#000000"))+  
    xlab("Year") + ylab("Coefficient")+ 
    theme(axis.text  = element_text(family="Times", size=9),
          axis.title  = element_text(family="Times", size=8),
          legend.text  = element_text(family="Times"),
          legend.title  = element_text(family="Times"),  
          plot.title = element_text(family="Times"),
          legend.position="none")
  #create plot object
  ggsave(paste("figures/main_rl_plots/",vars[i],".pdf", sep=""), name, device="pdf", width=2.85, height=2)
}

